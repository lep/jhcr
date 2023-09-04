{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

import Debug.Trace

import Data.FileEmbed


import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Arrow (first, second)

import System.IO
import System.Exit

import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Builder

import Data.List (isPrefixOf, partition)

import Data.Binary (decodeFile, encodeFile)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import qualified Jass.Parser as J
import qualified Jass.Ast as J
import qualified Jass.Printer as J
import qualified Jass.Opt.Rewrite as Jass.Opt
import qualified Jass.Opt.Rewrite.SomeRules as Jass.Opt
import qualified Jass.LCA as LCA

import qualified Hot.Ast as H
import qualified Hot.Var as H

import qualified Hot.Instruction as Ins
import qualified Hot.Instruction.Compiler as Ins

import qualified Hot.Init.Rename as Rename
import qualified Hot.Init.Stubs as Stubs
import qualified Hot.Init.Auto as Auto

import qualified Hot.Instruction.Opt.Rewrite as Ins.Opt
import qualified Hot.Instruction.Opt.Rewrite.SomeRules as Ins.Opt

import qualified Hot.HandleCode as HandleCode
import qualified Hot.JassHelper as JH

import Hot.CommonJHash

import Data.Composeable
import Data.Hashable

import Data.Monoid

import Text.Megaparsec (errorBundlePretty)
import qualified Text.Megaparsec as Mega

import Text.Printf

import Options.Applicative
import Development.GitRev (gitHash)

import GHC.IO.Encoding (setLocaleEncoding, utf8)

exceptT :: Either e a -> ExceptT e IO a
exceptT = ExceptT . return

parse fp p src = Mega.parse fp p $ src <> "\n"

concatPrograms :: J.Ast a J.Programm -> J.Ast a J.Programm -> J.Ast a J.Programm
concatPrograms (J.Programm a) (J.Programm b) = J.Programm $ a <> b


runtime1 :: [String]
runtime1 = map S8.unpack
  [ $(embedFile "out/print.j") -- print is used by alloc
  , $(embedFile "out/list.j")
  , $(embedFile "out/table.j")
  , $(embedFile "out/stringtable.j")
  , $(embedFile "out/types.j")
  , $(embedFile "out/context.j")
  , $(embedFile "out/wrap-around.j")
  , $(embedFile "out/modified.j")
  , $(embedFile "out/instruction.j")
  , $(embedFile "out/parser.j")
  ]

runtime2 :: [String]
runtime2 = map S8.unpack
  [ $(embedFile "out/convert.j")
  , $(embedFile "out/interpreter.j")
  , $(embedFile "out/init.j")
  ]

data Options =
    Init { jassFiles :: [FilePath]
         , processJasshelper :: Bool
         , statePath :: FilePath
         , outjPath :: FilePath
         }
  | Update { inputjPath :: FilePath
           , preloadPath :: FilePath
           , processJasshelper :: Bool
           , statePath :: FilePath
           , showAsm :: Bool
           }
   | Compile { commonjPath :: FilePath
             , inputPaths :: [FilePath]
             , optJass :: Bool
             , optAsm :: Bool
             , showSerialize :: Bool
             }
    deriving (Show)

parseOptions = customExecParser (prefs showHelpOnEmpty) opts
  where
    opts = info (pCommand <**> helper)
      (  fullDesc
      <> header ("jhcr - A compiler to allow hot code reload in jass")
      <> footer ( unwords [ "Compiled from git commit", $(gitHash),
            "with common.j hash", printf "0x%x" commonjHash ])
      )
    pCommand = hsubparser
      (  command "init" (info initOptions ( progDesc "Compiles the mapscript to allow hot code reload"))
      <> command "update" (info updateOptions ( progDesc "Compiles updates  to be reloaded in the map"))
      <> command "compile" (info compileOptions (progDesc "Compiles code to asm. Used for debugging purposes"))
      )
    initOptions =
        Init <$> some (argument str (metavar "[FILE]" <> help "All jass files needed. First one should be common.j and last one should be the map script"))
             <*> pJasshelper
             <*> pState
             <*> pOutWar3Map
    updateOptions =
        Update <$> pWar3Map
               <*> pPreload
               <*> pJasshelper
               <*> pState
               <*> pAsm
    compileOptions =
        Compile <$> argument str (help "Path to common.j" <> metavar "common.j")
                <*> some (argument str (metavar "[FILE]" <> help "jass files to compile"))
                <*> switch (long "opt-jass" <> help "Applies jass optimisations")
                <*> switch (long "opt-asm" <> help "Applies asm optimisations")
                <*> switch (long "show-serialize" <> help "Show serialized asm")

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
    setLocaleEncoding utf8
    options <- parseOptions
    case options of
        Update{} -> updateX options
        Init{} -> initX options
        Compile{} -> compileX options


mkHashMap :: J.Ast J.Name x -> Map J.Name Int
mkHashMap x =
  case x of
    J.Function _ name _ _ _ -> Map.singleton name $ hash x
    
     -- we don't care about the value, just if it's already initialized or not
    J.Global (J.SDef _ name ty (Just _)) -> Map.singleton name $ hash ty
    _ -> composeFold mkHashMap x

compileX o = do
    let jass_opt = if optJass o then Jass.Opt.rewrite Jass.Opt.someRules else id
        ins_opt = if optAsm o then (!!4) . iterate (Ins.Opt.rewrite Ins.Opt.someRules) else id
    x <- runExceptT $ do
        src <- liftIO $ readFile (commonjPath o)
        commonj <- exceptT $ parse J.programm (commonjPath o) src
        
        toCompile <- forM (inputPaths o) $ \j -> do
            src <- liftIO $ readFile j
            J.Programm ast <- exceptT $ parse J.programm j src
            --traceShowM ast
            return ast
        return (commonj, J.Programm $ concat toCompile)
    case x of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right (commonj, j) -> do
            let typeHierachy = LCA.child2parent commonj
            let (_, st) = Rename.compile' Rename.Init id commonj
                prog = jass_opt j
                prog' = H.jass2hot . fst $ Rename.compile Rename.Init id st prog
                asm = ins_opt $ Ins.compile typeHierachy prog'
            --traceShowM j
            --traceShowM prog
            hPutBuilder stdout $ Ins.serializeAsm asm
            when (showSerialize o) $ do
                putStrLn ""
                forM_ (Ins.serializeChunked 700 asm) $ \s ->
                    putStrLn s
            

updateX o = do
    let jhc = if processJasshelper o then JH.compile else id
        jass_opt = Jass.Opt.rewrite Jass.Opt.someRules
        ins_opt = (!!4) . iterate (Ins.Opt.rewrite Ins.Opt.someRules)
        
    (st, hmap, typeHierachy, seq :: Int) <- decodeFile (statePath o)

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
                putStrLn $ unwords ["Updating function", n]

            putStrLn "Writing bytecode"
        
            let fnsCompiled = ins_opt $ Ins.compile typeHierachy ast''
                gCompiled = ins_opt $ Ins.compileGlobals typeHierachy $ H.globals2statements ast'
                
            when (showAsm o) $ do
                putStrLn "; functions"
                hPutBuilder stdout $ Ins.serializeAsm fnsCompiled
                putStrLn ""
                putStrLn "; globals"
                hPutBuilder stdout $ Ins.serializeAsm gCompiled
            
            let fnAsm = Ins.serializeChunked 1000 fnsCompiled
                gAsm = Ins.serializeChunked 1000 gCompiled
                preload' = mkPreload fnAsm gAsm
                --(if oldPatch o then mkPreload128 else mkPreload) fnAsm gAsm
            case preload' of
                Nothing -> do
                    hPutStrLn stderr "Too many changes. Did not write bytecode to file"
                    exitFailure
                Just preload -> do
                    createDirectoryIfMissing True $ preloadPath o
#if PATCH_LVL < 133
                    withBinaryFile (preloadPath o </> "JHCR.txt") WriteMode $ \f ->
                        hPutBuilder f $ J.pretty preload
#else
                    withBinaryFile (preloadPath o </> "JHCR-" <> show seq <> ".txt") WriteMode $ \f ->
                        hPutBuilder f $ J.pretty preload
#endif

                    putStrLn "Writing state file"
                    encodeFile (statePath o) (st', hmap' <> hmap, typeHierachy, succ seq)
                    putStrLn "Ok."
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
    isSDef (J.Global (J.SDef{})) = True
    isSDef _ = False

    mkPreload :: [String] -> [String] -> Maybe (J.Ast J.Name J.Programm)
    mkPreload fns globals = do
        let gc = J.Local (J.SDef J.Normal "gc" "gamecache" $ Just $ J.Call "InitGameCache" [J.String "JHCR.w3v"]) 
        fns' <- mkF (J.String "functions") fns
        g' <- mkF (J.String "globals") globals
        return . J.Programm . pure . J.Function J.Normal "PreloadFiles" [] "nothing" $
            gc:fns' <> g'


    mkF label asms = do
        let statements = zipWith (storeAsm label) asms [1..]
            storeLength = J.Call "StoreInteger" [ J.Var $ J.SVar "gc", label, J.String "count", J.Int $ show $ length statements]
        pure $ storeLength:statements

    storeAsm label bytecode index =
        J.Call "StoreString" [J.Var $ J.SVar "gc", label, J.String $ show index, J.String bytecode]



initX o = do
    let jhc = if processJasshelper o then JH.compile else id
    let commonJassFiles = init $ jassFiles o
        inputjPath = last $ jassFiles o
    when (length (jassFiles o) < 2) $ do
        hPutStrLn stderr "Provide at least two jass files"
        exitFailure 
        
    x <- runExceptT $ do
        (prelude, First (Just cjhash)) <- first J.Programm . mconcat <$> forM commonJassFiles (\j -> do
            src <- liftIO $ readFile j
            J.Programm ast <- exceptT $ parse J.programm j src
            return (ast, First . Just . hash $ J.Programm ast)
            )

        rt1 <- J.Programm . mconcat <$> forM runtime1 (\src -> do
            J.Programm ast <- exceptT $ parse J.programm "rt1" src
            return ast)
        
        rt2 <- J.Programm . mconcat <$> forM runtime2 (\src -> do
            J.Programm ast <- exceptT $ parse J.programm "rt2" src
            return ast)
            
        return (prelude, cjhash, rt1, rt2)
        
    case x of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right (prelude, cjhash, rt1, rt2) -> do
            putStrLn "Initializing...."
            when (cjhash /= commonjHash) $
                putStrLn $ unwords
                    [ "Potentially mismatching common.j files:"
                    , printf "yours (0x%x) mine (0x%x)" cjhash commonjHash
                    ]
            
            let rt1' = addPrefix' "JHCR" rt1
                rt2' = addPrefix' "JHCR" rt2

            let (prelude', st) = Rename.compile' Rename.Init id prelude
                typeHierachy = LCA.child2parent prelude
            
            p <- parse J.programm inputjPath <$> readFile inputjPath
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
                        
                        generated'' = replaceExecuteFunc . J.fmap H.nameOf $ addPrefix "JHCR" generated'
                        
                        (main, rest) = extractMainAndConfig generated''
                        main' = injectInit main
                        
                        outj = foldl1 concatPrograms [ rt1', rest, rt2', main']
                        
                        hmap = mkHashMap jhast
                    
                    putStrLn "Writing state file"
                    encodeFile (statePath o) (st', hmap, typeHierachy, 0::Int)

                    putStrLn "Writing map script"
                    withBinaryFile (outjPath o) WriteMode $ \f ->
                        hPutBuilder f $ J.pretty outj

                    putStrLn "Ok."
  where
    replaceExecuteFunc :: J.Ast J.Name x -> J.Ast J.Name x
    replaceExecuteFunc x =
      case x of
        J.Call "ExecuteFunc" args -> J.Call ("JHCR" <> "_Wrap_ExecuteFunc")
             -- dont think valid jass would allow nested execute funcs but whatev
            $ map replaceExecuteFunc args
        _ -> composeOp replaceExecuteFunc x
    
    extractMainAndConfig :: J.Ast J.Name J.Programm -> (J.Ast J.Name J.Programm, J.Ast J.Name J.Programm)
    extractMainAndConfig (J.Programm ts) =
        let (a, b) = flip partition ts $ \case
                        J.Function _ "main" _ _ _ -> True
                        J.Function _ "config" _ _ _ -> True
                        _ -> False
        in (J.Programm a, J.Programm b)
    
    injectInit :: J.Ast J.Name x -> J.Ast J.Name x
    injectInit x =
      case x of
        J.Function c "main" args ret body ->
            let (locals, stmts) = partition J.isLocal body
                call = J.Call ("JHCR" <> "_Init_init") []
            in J.Function c "main" args ret $ locals <> (call:stmts)
        _ -> composeOp injectInit x

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

