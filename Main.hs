{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.FileEmbed


import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Arrow (first, second)

import System.IO
import System.Exit

import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Builder

import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

import Data.Binary (decodeFile, encodeFile)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import qualified Jass.Parser as J
import qualified Jass.Ast as J
import qualified Jass.Printer as J
import qualified Jass.Opt.Rewrite as Jass.Opt

import qualified Hot.Ast as H
import qualified Hot.Var as H

import qualified Hot.Instruction as Ins
import qualified Hot.Instruction.Compiler as Ins

import qualified Hot.Init.Rename as Rename
import qualified Hot.Init.Stubs as Stubs
import qualified Hot.Init.Auto as Auto

import qualified Hot.Instruction.Opt.Rewrite as Ins.Opt

import qualified Hot.HandleCode as HandleCode
import qualified Hot.JassHelper as JH

import Data.Composeable
import Data.Hashable

import Text.Megaparsec (errorBundlePretty)
import qualified Text.Megaparsec as Mega

import Options.Applicative
import Development.GitRev (gitHash)

exceptT :: Either e a -> ExceptT e IO a
exceptT = ExceptT . return

parse fp p src = Mega.parse fp p $ src <> "\n"

concatPrograms :: J.Ast a J.Programm -> J.Ast a J.Programm -> J.Ast a J.Programm
concatPrograms (J.Programm a) (J.Programm b) = J.Programm $ a <> b

-- it might be that not every rule brings a huge advantage in the generated
-- assembler code, but not rule should hurt the generated code.
someJassRewriteRules =
  [ mkRR ["a"] "-(-a)" "a"
  , mkRR ["a"] "a==true" "a"
  , mkRR ["a"] "true==a" "a"
  , mkRR ["a", "b"] "not (a<=b)" "a>b"
  , mkRR ["a", "b"] "not (a<b)" "a>=b"
  , mkRR ["a", "b"] "not (a>=b)" "a<b"
  , mkRR ["a", "b"] "not (a>b)" "a<=b"
  , mkRR ["a"] "false==a" "not a"
  , mkRR ["a"] "a==false" "not a"
  , mkRR ["a"] "false!=a" "a"
  , mkRR ["a"] "a!=false" "a"
  , mkRR ["a"] "a!=true" "not a"
  , mkRR ["a"] "true!=a" "not a"
  , mkRR ["a"] "not (not a)" "a"
  , mkRR ["a", "b"] "(not a) or (not b)" "not (a and b)"
  , mkRR ["a", "b"] "(not a) and (not b)" "not (a or b)"
  , mkRR ["a"] "false and a" "false"
  , mkRR ["a"] "true or a" "true"
  , mkRR ["a"] "true and a" "a"
  , mkRR ["a"] "a and true" "a"
  , mkRR ["a"] "false or a" "a"
  , mkRR ["a"] "a or false" "a"
  , mkRR ["a", "b"] "IsUnit(a,b)" "a == b"
  , mkRR ["u", "l"] "SetUnitState(u, UNIT_STATE_LIFE, l)" "SetWidgetLife(u, l)" -- gg, bind vs conv
  , mkRR ["u"] "GetUnitState(u, UNIT_STATE_LIFE)" "GetWidgetLife(u)" -- gg, bind vs conv
  , mkRR ["u", "p"] "GetOwningPlayer(u) == p" "IsUnitOwnedByPlayer(u, p)"
  , mkRR ["i"] "I2R(i)" "(i+0.0)" -- bind, call vs lit, add
  , mkRR ["a", "b"] "a + (-b)" "a-b"
  ]
  where
    mkRR fv from to =
      Jass.Opt.Rule
        (Set.fromList fv)
        (fromJust $ Mega.parseMaybe (J.expression <* Mega.eof) from)
        (fromJust $ Mega.parseMaybe (J.expression <* Mega.eof) to)


runtime1 :: [String]
runtime1 = map S8.unpack
  [ $(embedFile "out/print.j"), $(embedFile "out/types.j")
  , $(embedFile "out/list.j"), $(embedFile "out/table.j")
  , $(embedFile "out/stringtable.j"), $(embedFile "out/convert.j")
  , $(embedFile "out/wrap-around.j"), $(embedFile "out/modified.j")
  ]

runtime2 :: [String]
runtime2 = map S8.unpack
  [ $(embedFile "out/context.j"), $(embedFile "out/instruction.j")
  , $(embedFile "out/parser.j"), $(embedFile "out/interpreter.j")
  , $(embedFile "out/init.j")
  ]

data Options =
    Init { commonjPath :: FilePath
         , blizzardjPath :: FilePath
         , inputjPath :: FilePath
         , processJasshelper :: Bool
         , statePath :: FilePath
         , outjPath :: FilePath
         , prefix :: String
         }
  | Update { inputjPath :: FilePath
           , preloadPath :: FilePath
           , processJasshelper :: Bool
           , statePath :: FilePath
           , showAsm :: Bool
           }
    deriving (Show)

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
      (  fullDesc
      <> header ("jhcr - A compiler to allow hot code reload in jass (v. git-" <> take 6 $(gitHash) <> ")")
      )
    pCommand = hsubparser
      (  command "init" (info initOptions ( progDesc "Compiles the mapscript to allow hot code reload"))
      <> command "update" (info updateOptions ( progDesc "Compiles updates  to be reloaded in the map"))
      )
    initOptions =
        Init <$> argument str (help "Path to common.j" <> metavar "common.j")
             <*> argument str (help "Path to Blizzard.j" <> metavar "Blizzard.j")
             <*> pWar3Map
             <*> pJasshelper
             <*> pState
             <*> pOutWar3Map
             <*> pPrefix
    updateOptions =
        Update <$> pWar3Map
               <*> pPreload
               <*> pJasshelper
               <*> pState
               <*> pAsm
    pJasshelper = switch
      (  long "jasshelper" 
      <> help "Treats the input script as if it was produced by jasshelper"
      )
    pWar3Map = argument str
        (  metavar "war3map.j"
        <> help "Path to the mapscript"
        )
    pOutWar3Map = strOption
        (  long "out"
        <> metavar "FILE"
        <> value "jhcr_war3map.j"
        <> help "Where to write the compiled mapscript"
        <> showDefault
        )
    pPrefix = strOption
        (  long "prefix"
        <> value "JHCR"
        <> help "Prefix for the runtime jass functions"
        <> showDefault
        )
    pState = strOption  
      (  long "state"
      <> metavar "FILE"
      <> value "jhcr.bin"
      <> help "State file to keep track of updates"
      <> showDefault
      )
    pPreload = strOption
        (  long "preload-path"
        <> metavar "PATH"
        <> help "Path to your CustomMapData folder"
        )
    pAsm = switch
      (  long "asm"
      <> help "Shows human readable assembler code"
      )

main = do
    options <- parseOptions
    case options of
        Update{} -> updateX options
        Init{} -> initX options


mkHashMap :: J.Ast J.Name x -> Map J.Name Int
mkHashMap x =
  case x of
    J.Function _ name _ _ _ -> Map.singleton name $ hash x
    
     -- we don't care about the value, just if it's already initialized or not
    J.Global (J.SDef _ name ty (Just e)) -> Map.singleton name $ hash ty
    _ -> composeFold mkHashMap x


updateX o = do
    let jhc = if processJasshelper o then JH.compile else id
        jass_opt = Jass.Opt.rewrite someJassRewriteRules
        ins_opt = (!!4) . iterate (Ins.Opt.rewrite Ins.Opt.someRules)
        
    (st, hmap) <- decodeFile (statePath o)

    p <- parse J.programm (inputjPath o) <$> readFile (inputjPath o)
    case p of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
            
        Right prog -> do
            let prog' = jhc prog
                J.Programm ast = prog'
                hmap' = mkHashMap prog'
                -- TODO: handle globals and set their correct value
                astU = filter (isUpdated hmap hmap') ast
                nameU = map getName $ filter J.isFunction astU
                progU = jass_opt $ J.Programm astU
            let (ast', st') = Rename.compile Rename.Update id st progU
                ast'' = H.jass2hot ast'

            forM_ nameU $ \n ->
                hPutStrLn stderr $ unwords ["Updating function", n]

            hPutStrLn stderr "Writing bytecode"
        
            let fnsCompiled = ins_opt $ Ins.compile ast''
                gCompiled = ins_opt $ Ins.compileGlobals $ H.globals2statements ast'
                
            when (showAsm o) $ do
                putStrLn "; functions"
                hPutBuilder stdout $ Ins.serializeAsm fnsCompiled
                putStrLn ""
                putStrLn "; globals"
                hPutBuilder stdout $ Ins.serializeAsm gCompiled
            
            let fnAsm = Ins.serializeChunked 700 fnsCompiled
                gAsm = Ins.serializeChunked 700 gCompiled
                preload' = mkPreload fnAsm gAsm
            
            case preload' of
                Nothing -> do
                    hPutStrLn stderr "Too many changes. Did not write bytecode to file"
                    exitFailure
                Just preload -> do
                    cfd <- openBinaryFile (preloadPath o </> "JHCR.txt") WriteMode
                    hPutBuilder cfd $ J.pretty preload
                    hFlush cfd
                    hClose cfd

                    hPutStrLn stderr "Writing state file"
                    encodeFile (statePath o) (st', hmap' <> hmap)
                    hPutStrLn stderr "Ok."
  where
    isUpdated :: Map J.Name Int -> Map J.Name Int -> J.Ast J.Name x -> Bool
    isUpdated old new x =
      case x of
        J.Function _ name _ _ _ ->
            case (o name, n name) of
                (Nothing, _) -> True
                (Just h1, Just h2) -> h1 /= h2
                _ -> True
        -- only report globals as fresh once or if their type has changed
        -- TODO: need to reload all functions which use that global.
        -- due to polymorphic number literals a change from integer to real for
        -- example wouldnt change a functions body like a change to a functions 
        -- signature would.
        J.Global (J.SDef _ v _ (Just _)) ->
            case (o v, n v) of
                (Nothing, _) -> True
                (Just t1, Just t2) -> t1 /= t2
                _ -> False
        _ -> True
      where
        o n = Map.lookup n old
        n n = Map.lookup n new
        
        
    getName :: J.Ast J.Name J.Toplevel -> J.Name
    getName (J.Function _ name _ _ _) = name
    getName (J.Global (J.SDef _ name _ _)) = name
    getName (J.Global (J.ADef name _)) = name
    
    isSDef :: J.Ast a J.Toplevel -> Bool
    isSDef (J.Global (J.SDef _ _ _ _)) = True
    isSDef _ = False
    
    mkPreload :: [String] -> [String] -> Maybe (J.Ast J.Name J.Programm)
    mkPreload fns globals = do
        fns' <- mkF fns fnsIds "1"
        g' <- mkF globals globalIds "2"
        return . J.Programm . pure . J.Function J.Normal "PreloadFiles" [] "nothing" $
            fns' <> g'
    
    mkF :: [String] -> [String] -> J.Lit -> Maybe [J.Ast J.Name J.Stmt]
    mkF asms ids slot = do
        let availableIds = map J.Rawcode ids
            cnt = length asms
            
            mkC id asm = J.Call "BlzSetAbilityTooltip" [ id, J.String asm, J.Int "1" ]
            
        guard (length availableIds >= cnt)
        
        let setCnt = J.Call "SetPlayerTechMaxAllowed" [J.Call "Player" [ J.Int "0" ], J.Int slot, J.Int $ show cnt ]
            setCodes = zipWith mkC availableIds asms

        return $ setCnt:setCodes

    fnsIds =
        [ "Agyv", "Aflk", "Agyb", "Ahea", "Ainf", "Aslo", "Afla", "Amls"
        , "Adis", "Acmg", "Amdf", "Adts", "Aast", "Aetf", "Absk", "Alsh"
        , "Aens", "Adcn", "Aliq", "Aspl", "Aven", "Ablo", "Acpf", "Awar"
        ]

    globalIds =
        [ "Adec", "Aeat", "Aco3", "Acoh", "Abrf", "Aro2", "Aro1", "Aegr"
        , "Aren"
        ]

initX o = do
    let jhc = if processJasshelper o then JH.compile else id
    x <- runExceptT $ do
        prelude <- J.Programm . mconcat <$> forM [commonjPath o, blizzardjPath o] (\j -> do
            src <- liftIO $ readFile j
            J.Programm ast <- exceptT $ parse J.programm j src
            return ast)

        rt1 <- J.Programm . mconcat <$> forM runtime1 (\src -> do
            J.Programm ast <- exceptT $ parse J.programm "rt1" src
            return ast)
        
        rt2 <- J.Programm . mconcat <$> forM runtime2 (\src -> do
            J.Programm ast <- exceptT $ parse J.programm "rt2" src
            return ast)
            
        return (prelude, rt1, rt2)
        
    case x of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right (prelude, rt1, rt2) -> do
            hPutStrLn stderr "Initializing...."
            
            let rt1' = addPrefix' (prefix o) rt1
                rt2' = addPrefix' (prefix o) rt2

            let (prelude', st) = Rename.compile' Rename.Init id prelude
            
            p <- parse J.programm (inputjPath o) <$> readFile (inputjPath o)
            case p of
                Left err -> do
                    hPutStrLn stderr $ errorBundlePretty err
                    exitFailure
                Right ast -> do
                    let jhast = jhc ast
                    let ast' :: J.Ast H.Var H.Programm
                        (ast', st') = first HandleCode.compile $
                                      Rename.compile Rename.Init conv st jhast
                        
                        conv x = if x == "code" then "integer" else x
                        
                        
                        generated :: [J.Ast H.Var H.Programm]
                        generated = Auto.compile $ concatPrograms prelude' ast'
                        
                        stubs :: J.Ast H.Var H.Programm
                        stubs = Stubs.compile ast'
                        
                        generated' :: J.Ast H.Var H.Programm
                        generated' = foldr1 concatPrograms $ stubs:generated
                        
                        generated'' = replaceExecuteFunc . J.fmap H.nameOf $ addPrefix (prefix o) generated'
                        
                        outj = concatPrograms (concatPrograms rt1' generated'') rt2'
                        
                        hmap = mkHashMap jhast
                    
                    hPutStrLn stderr "Writing state file"
                    encodeFile (statePath o) (st', hmap)

                    hPutStrLn stderr "Writing map script"
                    jh <- openBinaryFile (outjPath o) WriteMode
                    hPutBuilder jh $ J.pretty outj
                    hFlush jh
                    hClose jh

                    hPutStrLn stderr "Ok."
  where
    replaceExecuteFunc :: J.Ast J.Name x -> J.Ast J.Name x
    replaceExecuteFunc x =
      case x of
        J.Call "ExecuteFunc" args -> J.Call (prefix o <> "_Wrap_ExecuteFunc")
             -- dont think valid jass would allow nested execute funcs but whatev
            $ map replaceExecuteFunc args
        _ -> composeOp replaceExecuteFunc x

addPrefix' :: J.Name -> J.Ast J.Name a -> J.Ast J.Name a
addPrefix' p x =
  case x of
    J.Native c n args ret -> J.Native c (r n) (map (second r) args) ret
    J.Function c n args ret body -> J.Function c (r n) (map (second r) args) ret (map (addPrefix' p) body)
    J.Call n args -> J.Call (r n) (map (addPrefix' p) args)
    J.ADef v ty -> J.ADef (r v) ty
    J.SDef c v ty init -> J.SDef c (r v) ty (fmap (addPrefix' p) init)
    J.AVar v idx -> J.AVar (r v) (addPrefix' p idx)
    J.SVar v -> J.SVar (r v)
    J.Code v -> J.Code (r v)

    _ -> composeOp (addPrefix' p) x

  where
    r :: J.Name -> J.Name
    r v 
        -- 4 == length "JHCR"
        | "JHCR_" `isPrefixOf` v = p <> drop 4 v
        | otherwise = v

addPrefix :: H.Name -> J.Ast H.Var a -> J.Ast H.Var a
addPrefix p e =
  case e of
    J.Native c n args ret -> J.Native c (r n) (map (second r) args) ret
    J.Function c n args ret body -> J.Function c (r n) (map (second r) args) ret (map (addPrefix p) body)
    J.Call n args -> J.Call (r n) (map (addPrefix p) args)
    J.ADef v ty -> J.ADef (r v) ty
    J.SDef c v ty init -> J.SDef c (r v) ty (fmap (addPrefix p) init)
    J.AVar v idx -> J.AVar (r v) (addPrefix p idx)
    J.SVar v -> J.SVar (r v)
    J.Code v -> J.Code (r v)

    _ -> composeOp (addPrefix p) e

  where
    r v 
        | "_" `isPrefixOf` H.nameOf v = p H.## v
        | otherwise = v

