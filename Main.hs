{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Control.Arrow (second)

import Data.Monoid

import System.IO
import System.Environment
import System.Exit

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder



import qualified Jass.Parser as J
import qualified Jass.Ast as J
import qualified Jass.Printer as J


import qualified Hot.Jass.Init as H
import qualified Hot.Ast as H
import qualified Hot.Var as H
import qualified Hot.Instruction as H
import qualified Hot.Instruction.Compiler as H

import Data.Composeable

main = do
    [common'j, blizzard'j] <- getArgs

    prelude <- runEitherT $ do
        src <- liftIO $ BL.readFile common'j
        J.Programm cj <- hoistEither $ J.parse J.programm src

        src <- liftIO $ BL.readFile blizzard'j
        J.Programm bj <- hoistEither $ J.parse J.programm src

        return $ J.Programm $ cj <> bj

    case prelude of
        Left err -> do
            hPutStrLn stderr $ show err
            exitFailure
        Right p -> loop p


loop prelude = forever $ do
    cmd <- words <$> getLine
    case cmd of
        ["init", file] -> init file
        ["compile", file] -> compile file
        ["exit"] -> exitSuccess
        _ -> hPutStrLn stderr "Unrecognized command"
  where
    (prelude', preludeState) = H.runRenameM' prelude

    init file = do
        hPutStrLn stderr "Initializeing...."
        p <- J.parse J.programm <$> BL.readFile file
        case p of
            Left err -> hPutStrLn stderr $ show err
            Right ast -> do
                let ast' = H.execRenameM preludeState ast
                    generated = H.generate $ concatPrograms prelude' ast'
                    stubs = H.stubify ast'
                    generated' = stubs:generated
                    --generatedFns = H.generate $ concatPrograms prelude' ast'
                forM_ (zip generated' ["stubs.j", "i2code.j", "call_predefined.j", "setget.j"]) $ \(p, path) -> do
                    hdl <- openBinaryFile path WriteMode
                    hPutBuilder hdl $ J.pretty $ addPrefix "JHCR" p
                    hFlush hdl
                    hClose hdl

                hPutStrLn stderr "Ok."

    compile file = do
        p <- J.parse J.programm <$> BL.readFile file
        case p of
            Left err -> hPutStrLn stderr $ show err
            Right ast -> do
                let ast' = H.execRenameM preludeState ast
                    ast'' = H.jass2hot ast'
                hPutBuilder stdout $ H.serialize $ H.compile ast''
                hPutStrLn stderr "Ok."
                

    concatPrograms :: J.Ast a J.Programm -> J.Ast a J.Programm -> J.Ast a J.Programm
    concatPrograms (J.Programm a) (J.Programm b) = J.Programm $ a <> b

addPrefix :: BL.ByteString -> J.Ast H.Var a -> J.Ast H.Var a
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
        | "_" `BL.isPrefixOf` H.nameOf v = p H.## v
        | otherwise = v
