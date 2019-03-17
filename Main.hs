{-# LANGUAGE GADTs #-}



import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Control.Arrow (second)

import Data.Monoid

import System.IO
import System.Environment
import System.Exit

import System.FilePath ((</>))

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

import Data.List (isPrefixOf)

import Data.Binary (encode, decode)

import Data.Map (Map)
import qualified Data.Map as Map



import qualified Jass.Parser as J
import qualified Jass.Ast as J
import qualified Jass.Printer as J


import qualified Hot.Jass.Init as H
import qualified Hot.Ast as H
import qualified Hot.Var as H
import qualified Hot.Instruction as H
import qualified Hot.Instruction.Compiler as H

import Data.Composeable
import Data.Hashable

import Text.Megaparsec (errorBundlePretty, parse, ParseErrorBundle)

import qualified Data.Text.IO as Text

--exceptT :: Monad m => m a -> ExceptT e m a
exceptT :: Either e a -> ExceptT e IO a
exceptT = ExceptT . return


concatPrograms :: J.Ast a J.Programm -> J.Ast a J.Programm -> J.Ast a J.Programm
concatPrograms (J.Programm a) (J.Programm b) = J.Programm $ a <> b

runtime1paths =
  [ "out/types.j", "out/table.j", "out/convert.j"
  , "out/wrap-around.j", "out/modified.j"
  ]

runtime2paths =
  [ "out/context.j", "out/instruction.j", "out/instruction-parser.j"
  , "out/interpreter.j", "out/init.j"
  ]

mkHashMap :: J.Ast J.Name x -> Map J.Name Int
mkHashMap x =
  case x of
  --
    --Function :: Constant -> var -> [(Type, var)] -> Type -> [Ast var Stmt] -> Ast var Toplevel
    J.Function _ name _ _ _ -> Map.singleton name $ hash x
    _ -> composeFold mkHashMap x


main = do
    args' <- getArgs
    case args' of
        ["init", cj, bj, uj] -> init [cj, bj, uj]
        ["compile", uj] -> compile uj
        ["asm", uj] -> asm uj
        _ -> do
            hPutStrLn stderr $
              unlines [ "Usage:"
                      , "  init common.j Blizzard.j war3map.j"
                      , "  compile update.j"
                      , "  asm update.j"
                      ]
            
            exitFailure

  where
    asm updatej = do
        st <- decode <$> BL.readFile "jhcr.state"
        p <- parse J.programm updatej <$> readFile updatej
        case p of
            Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                exitFailure
                
            Right ast -> do
                let (ast', st') = H.runRenameM H.Compile st ast
                    ast'' = H.jass2hot ast'

                hPutBuilder stdout $ H.serializeAsm $ H.compile ast''
    
    isUpdated :: Map J.Name Int -> Map J.Name Int -> J.Ast J.Name x -> Bool
    isUpdated old new x =
      case x of
        J.Function _ name _ _ _ ->
            let o = Map.lookup name old
                n = Map.lookup name new
            in case (o, n) of
                (Nothing, _) -> True
                (Just h1, Just h2) -> h1 /= h2
                _ -> True
        _ -> True

    isFunction :: J.Ast a J.Toplevel -> Bool
    isFunction J.Function{} = True
    isFunction _ = False
    
    getFnName :: J.Ast J.Name J.Toplevel -> J.Name
    getFnName (J.Function _ name _ _ _) = name
        
    compile updatej = do
        stf <- openBinaryFile "jhcr.state" ReadWriteMode
        (st, hmap) <- decode <$> BL.hGetContents stf

        
        p <- parse J.programm updatej <$> readFile updatej
        case p of
            Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                hClose stf
                exitFailure
                
            Right prog@(J.Programm ast) -> do
                let hmap' = mkHashMap prog
                    astU = filter (isUpdated hmap hmap') ast
                    nameU = map getFnName $ filter isFunction astU
                    progU = J.Programm astU
                
                let (ast', st') = H.runRenameM H.Compile st progU
                    ast'' = H.jass2hot ast'
                
                forM_ nameU $ \n ->
                    putStrLn $ unwords ["Updating function", n]
  
                hPutStrLn stderr "Writing bytecode"
                cfd <- openBinaryFile "jhcr.txt" WriteMode
                hPutBuilder cfd $ H.serialize $ H.compile ast''
                hFlush cfd
                hClose cfd

                hPutStrLn stderr "Writing state file"

                hSeek stf AbsoluteSeek 0
                BL.hPutStr stf $ encode (st', hmap' <> hmap)
                hFlush stf
                hClose stf
                
                hPutStrLn stderr "Ok."
  
    init :: [FilePath] -> IO ()
    init [commonj, blizzardj, war3mapj] = do
    
        x <- runExceptT $ do
            prelude <- J.Programm . mconcat <$> forM [commonj, blizzardj] (\j -> do
                src <- liftIO $ readFile j
                J.Programm ast <- exceptT $ parse J.programm j src
                return ast)
                
            rt1 <- J.Programm . mconcat <$> forM runtime1paths (\j -> do
                src <- liftIO $ readFile j
                J.Programm ast <- exceptT $ parse J.programm j src
                return ast)
            
            rt2 <- J.Programm . mconcat <$> forM runtime2paths (\j -> do
                src <- liftIO $ readFile j
                J.Programm ast <- exceptT $ parse J.programm j src
                return ast)
                
            return (prelude, rt1, rt2)
            
        case x of
            Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                exitFailure
            Right (prelude, rt1, rt2) -> do
                hPutStrLn stderr "Initializing...."

                let (prelude', st) = H.runRenameM' H.Init prelude
                
                p <- parse J.programm war3mapj <$> readFile war3mapj
                case p of
                    Left err -> do
                        hPutStrLn stderr $ errorBundlePretty err
                        exitFailure
                    Right ast -> do
                        let ast' :: J.Ast H.Var H.Programm
                            (ast', st') = H.runRenameM H.Init st ast
                            
                            generated :: [J.Ast H.Var H.Programm]
                            generated = H.generate $ concatPrograms prelude' ast'
                            
                            stubs :: J.Ast H.Var H.Programm
                            stubs = H.stubify ast'
                            
                            generated' :: J.Ast H.Var H.Programm
                            generated' = foldr1 concatPrograms $ stubs:generated
                            
                            generated'' = J.fmap H.nameOf $ addPrefix "JHCR" generated'
                            
                            outj = concatPrograms (concatPrograms rt1 generated'') rt2
                            
                            hmap = mkHashMap ast
                        
                        hPutStrLn stderr "Writing state file"
                        BL.writeFile "jhcr.state" $ encode (st', hmap)

                        hPutStrLn stderr "Writing map script"
                        jh <- openBinaryFile "jhcr.j" WriteMode
                        hPutBuilder jh $ J.pretty outj
                        hFlush jh
                        hClose jh

                        hPutStrLn stderr "Ok."
                
                
                
{-
main = do
    args <- getArgs

    --prelude <- J.Programm . mappend <$> runExceptT . forM args $ \j ->  do
    --    src <- liftIO $ BL.readFile j
     --   exceptT $ J.parse J.programm src
    
    prelude <- runExceptT $ forM args $ \j -> do
        src <- liftIO $ readFile j
        J.Programm ast <- exceptT $ parse J.programm j src
        return ast

    {-
    [common'j, blizzard'j] <- getArgs
    prelude <- runExceptT $ do
        src <- liftIO $ BL.readFile common'j
        J.Programm cj <- exceptT $ J.parse J.programm src

        src <- liftIO $ BL.readFile blizzard'j
        J.Programm bj <- exceptT $ J.parse J.programm src

        return $ J.Programm $ cj <> bj
    -}


    case prelude of
        Left err -> do
            hPutStrLn stderr $ errorBundlePretty err
            exitFailure
        Right p -> do
            let prelude = J.Programm $ mconcat p
            let (prelude', preludeState) = H.runRenameM' H.Init prelude
            loop prelude' preludeState

loop prelude st =  do
    cmd <- words <$> getLine
    case take 1 cmd of
        ["init"] -> do
            st' <- init (unwords $ tail cmd)
            loop prelude st'
        ["compile"] -> do
            st' <- compile H.serialize (unwords $ tail cmd)
            loop prelude st'
        ["asm"] -> do
            st' <- compile H.serializeAsm (unwords $ tail cmd)
            loop prelude st'
        ["exit"] -> exitSuccess
        _ -> do
            hPutStrLn stderr "Unrecognized command"
            loop prelude st
  where
    --(prelude', preludeState) = H.runRenameM' prelude

    init file = do
        hPutStrLn stderr "Initializeing...."
        p <- parse J.programm file <$> readFile file
        case p of
            Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                return st
            Right ast -> do
                let 
                    ast' :: J.Ast H.Var H.Programm
                    (ast', st') = H.runRenameM H.Init st ast
                    
                    generated :: [J.Ast H.Var H.Programm]
                    generated = H.generate $ concatPrograms prelude ast'
                    
                    stubs :: J.Ast H.Var H.Programm
                    stubs = H.stubify ast'
                    
                    init_tables :: J.Ast H.Var H.Programm
                    init_tables = H.init_name2ids $ concatPrograms prelude ast'
                    
                    generated' :: [J.Ast H.Var H.Programm]
                    generated' = init_tables:stubs:generated
                    --generatedFns = H.generate $ concatPrograms prelude' ast'
                forM_ (zip generated' ["init_tables.j", "stubs.j", "i2code.j", "call_predefined.j", "setget.j"]) $ \(p, path) -> do
                    hPutStrLn stderr $ unwords ["Writing", path]
                    hdl <- openBinaryFile ("generated" </> path) WriteMode
                    --hSetBuffering hdl BlockBuffering
                    hPutBuilder hdl $ J.pretty . J.fmap H.nameOf $ addPrefix "JHCR" p
                    hPutStrLn stderr $ unwords ["Done Writing", path]
                    hFlush hdl
                    hClose hdl

                hPutStrLn stderr "Ok."
                return st'

    compile serialize file = do
        p <- parse J.programm file <$> readFile file
        case p of
            Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                return st
            Right ast -> do
                let (ast', st') = H.runRenameM H.Compile st ast
                    ast'' = H.jass2hot ast'
                hPutBuilder stdout $ serialize $ H.compile ast''
                hFlush stdout
                hPutStrLn stderr "Ok."
                return st'
                

    concatPrograms :: J.Ast a J.Programm -> J.Ast a J.Programm -> J.Ast a J.Programm
    concatPrograms (J.Programm a) (J.Programm b) = J.Programm $ a <> b
-}


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
