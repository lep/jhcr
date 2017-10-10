{-# LANGUAGE GADTs #-}


import qualified Jass.Parser as Jass
import qualified Jass.Ast as Jass

import qualified Hot.Instruction as I
import qualified Hot.Ast as A
import qualified Hot.Generate as G

import qualified Data.ByteString.Lazy as BL

import Data.ByteString.Builder

import System.Environment
import System.IO

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map


import Data.Maybe (fromJust)
import Data.Tuple (swap)



--test src = do
--    let Right j = Jass.parse Jass.programm src
--        ast = A.compile j
--        ins = I.compile ast
--    --forM_ ins print
--    hPutBuilder stdout $ I.serialize ins
--
--bla src =
--    let Right j = Jass.parse Jass.programm src
--        Jass.Programm ast1 = A.convertNamesToUniqueIds j
--        Jass.Programm ast2 = A.renameLocals' $ Jass.Programm ast1
--    in (last ast1, last ast2)

gen src =
    let Right j = Jass.parse Jass.programm src
        (ast1, A.ScopeS env1 _ _) = runState (A.runScopeMonad $ A.name2ids j) A.emptyState
        (ast2, (_, env2)) = runState (A.partitionGlobalsByType' ast1) mempty

        uid2name = inv env1 :: Map A.Var A.Name
        p2uid = inv env2 :: Map A.Var A.Var

        l k = fromJust $ do
            k' <- Map.lookup k p2uid
            Map.lookup k' uid2name
    in G.generate l ast2
  where
    inv = Map.fromList . map swap . Map.toList
    

--main = do
--    [x] <- getArgs
--    j <- Jass.parse Jass.programm <$> BL.readFile x
--    case j of
--        Left err -> print err
--        Right j' -> do
--            let (ast, locals) = A.compile j'
--                ins = I.compile locals ast
--            print ast
--            putStrLn "\n\n"
--            forM_ ins print

