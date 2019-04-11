{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Jass.Opt.Rewrite (rewrite, RewriteRule(..) ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Control.Arrow
import Control.Monad
import Control.Monad.State

import Jass.Ast hiding (fmap)

import Data.Composeable

import Unsafe.Coerce

data RewriteRule = Rule { freeVars :: Set Name
                        , fromExpr :: Ast Name Expr
                        , toExpr   :: Ast Name Expr
                        }

type S = Map Name (Ast Name Expr)


unify :: Name -> Ast Name Expr -> State S Bool
unify name expr = do
    e <- Map.lookup name <$> get
    case e of
        Nothing -> do
            modify $ Map.insert name expr
            return True
        Just expr' -> if expr == expr'
                      then return True
                      else return False


rewrite' :: RewriteRule -> Map Name (Ast Name Expr) -> Ast Name Expr
rewrite' Rule{ toExpr = e } mappings = go e
  where
    go x =
      case x of
        Call n args -> Call n $ map go args
        Var (SVar n)
          | Just e' <- Map.lookup n mappings -> e'
        _ -> x


matches :: RewriteRule -> Ast Name Expr -> Maybe (Map Name (Ast Name Expr))
matches rr expr =
  case runState (matches' rr expr) Map.empty of
    (False, _) -> Nothing
    (True, x)  -> Just x

matches' :: RewriteRule -> Ast Name Expr -> State S Bool
matches' rr@Rule{..} expr =
  case (fromExpr, expr) of
    (Call n1 a1, Call n2 a2) ->
      if n1 == n2
      then and <$> zipWithM (\a b -> matches' rr{ fromExpr = a} b) a1 a2
      else return False
    (Var (SVar x), y)
      | x `Set.member` freeVars -> do
        same <- unify x y
        return same

    (Var (SVar x), Var (SVar y)) -> return $ x == y

    (String a, String b) -> return $ a == b
    (Code a, Code b)     -> return $ a == b
    (Bool a, Bool b)     -> return $ a == b
    (Real a, Real b)     -> return $ a == b
    (Int a, Int b)       -> return $ a == b

    (Null, Null) -> return True

    _ -> return False


apply :: RewriteRule -> Ast Name Expr -> Ast Name Expr
apply rule expr = fromMaybe expr $ rewrite' rule <$> matches rule expr


rewrite :: [RewriteRule] -> Ast Name a -> Ast Name a
rewrite rules ast =
  case ast of
    If cond body elseifs els ->
      let cond'    = rewriteExpr cond
          body'    = map (rewrite rules) body
          elseifs' = rewriteElseIfs elseifs
          else'    = rewriteElse els
      in If cond' body' elseifs' else'
    Exitwhen cond -> Exitwhen $ rewriteExpr cond
    Set lvar value -> Set (rewrite rules lvar) $ rewriteExpr value
    Return (Just e) -> Return . Just $ rewriteExpr e
    SDef c n t (Just e) -> SDef c n t . Just $ rewriteExpr e
    AVar n idx -> AVar n $ rewriteExpr idx

    -- always a statement
    Call{} -> unsafeCoerce $ rewriteCallStmt (unsafeCoerce ast)

    _      -> composeOp (rewrite rules) ast

  where
    rewriteElse :: Maybe [Ast Name Stmt] -> Maybe [Ast Name Stmt]
    rewriteElse = fmap (map (rewrite rules))

    rewriteElseIfs :: [(Ast Name Expr, [Ast Name Stmt])] -> [(Ast Name Expr, [Ast Name Stmt])]
    rewriteElseIfs = map (rewriteExpr *** (map (rewrite rules)))

    rewriteCallStmt :: Ast Name Stmt -> Ast Name Stmt
    rewriteCallStmt (Call n args) =
      let args' = map rewriteExpr args
          e     = Call n args'
          e'    = rewriteExpr e
      in if isValidStmt e'
      then e'
      else e

    rewriteExpr :: Ast Name a -> Ast Name a
    rewriteExpr e =
      case e of
        Call n args ->
          let args' = map rewriteExpr args
              e'    = Call n args'
          in unsafeCoerce $ foldl' (flip apply) e' rules
        _ -> composeOp rewriteExpr e

isValidStmt :: Ast Name a -> Bool
isValidStmt (Call n _) = not $ isOp n
isValidStmt _  = False
