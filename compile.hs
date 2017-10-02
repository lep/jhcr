

import qualified Jass.Parser as Jass
import qualified Hot.Instruction as I
import qualified Hot.Ast as A

import qualified Data.ByteString.Lazy as BL

import System.Environment

import Control.Monad



test src = do
    let Right j = Jass.parse Jass.programm src
        ast = A.compile j
        ins = I.compile ast
    forM_ ins print
    

main = do
    [x] <- getArgs
    j <- Jass.parse Jass.programm <$> BL.readFile x
    case j of
        Left err -> print err
        Right j' -> do
            let ast = A.compile j'
                ins = I.compile ast
            print ast
            putStrLn "\n\n"
            forM_ ins print

