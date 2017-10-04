{-# LANGUAGE GADTs #-}


import qualified Jass.Parser as Jass
import qualified Jass.Ast as Jass

import qualified Hot.Instruction as I
import qualified Hot.Ast as A

import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Builder

import System.Environment
import System.IO

import Control.Monad



test src = do
    let Right j = Jass.parse Jass.programm src
        (ast, locals) = A.compile j
        ins = I.compile locals ast
    --forM_ ins print
    hPutBuilder stdout $ I.serialize ins

bla src =
    let Right j = Jass.parse Jass.programm src
        Jass.Programm ast1 = A.convertNamesToUniqueIds j
        Jass.Programm ast2 = A.renameLocals' $ Jass.Programm ast1
    in (last ast1, last ast2)
    

main = do
    [x] <- getArgs
    j <- Jass.parse Jass.programm <$> BL.readFile x
    case j of
        Left err -> print err
        Right j' -> do
            let (ast, locals) = A.compile j'
                ins = I.compile locals ast
            print ast
            putStrLn "\n\n"
            forM_ ins print

