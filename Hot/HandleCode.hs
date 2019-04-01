{-# LANGUAGE GADTs #-}

module Hot.HandleCode (compile) where

import Control.Arrow

import qualified Jass.Ast as J
import qualified Hot.Ast as H

import Data.Composeable

import Unsafe.Coerce

data Type = Replace | Orig

toType "_replace_code" = Replace
toType _ = Orig

fromType Replace = "_replace_code"
fromType Orig = "_whatever"

instance Semigroup Type where
    Replace <> _        = Replace
    _       <> Replace  = Replace
    Orig    <> _        = Orig

instance Monoid Type where
    mempty = Orig

compileReplace :: J.Ast H.Var x -> J.Ast H.Var x
compileReplace x =
  case x of
    J.Set lvar (J.Code (H.Fn _ _ _ id)) ->
        J.Set lvar $ J.Int (show id)
    J.SDef c var "_replace_code" (Just (J.Code (H.Fn _ _ _ id))) ->
        J.SDef c var "_replace_code" . Just . J.Int $ show id
    J.Call fn@(H.Fn _ types _ _) args ->
        let want = map conv types
            args' = zipWith ($) want $ map compileReplace args
        in J.Call fn args'
    _ -> composeOp compileReplace x

  where
    conv "code" x =
      case x of
        J.Code{} -> x
        _ -> J.Call (H.Fn "_Wrap_i2code" [] "" 0) [x]
    conv "_replace_code" (J.Code (H.Fn _ _ _ id)) = J.Int (show id)
    conv _ x = x

compileNull :: H.Type -> J.Ast H.Var x -> J.Ast H.Var x
compileNull ty x =
  case x of
    J.Null -> if ty == "_replace_code" then J.Int "0" else J.Null
    
    J.Call fn@(H.Op _) args ->
        let want = fromType $ foldMap typeOfExpr args
            args' = map (compileNull want) args
        in J.Call fn args'
    
    J.Call fn@(H.Fn _ types _ _) args ->
        let args' = zipWith compileNull types args
        in J.Call fn args'
        
    J.SDef c var ty init ->
        J.SDef c var ty $ fmap (compileNull ty) init
    
    J.Set lvar expr ->
        let ty = typeOfLVar lvar
            lvar' = compileNull (error "C") lvar
            expr' = compileNull ty expr
        in J.Set lvar' expr'
    
    _ -> composeOp (compileNull ty) x

typeOfExpr :: J.Ast H.Var J.Expr -> Type
typeOfExpr x =
  case x of
    J.Var lvar -> toType $ typeOfLVar lvar
    _ -> Orig

typeOfLVar :: J.Ast H.Var J.LVar -> H.Type
typeOfLVar x =
  case x of
    J.SVar var -> typeOfVar var
    J.AVar v idx -> typeOfVar v

typeOfVar :: H.Var -> H.Type
typeOfVar x =
  case x of
    H.Local _ ty _ _ -> ty
    H.Global _ _ ty _ _ -> ty
    H.Op _ -> error "B"
    H.Fn _ _ ret _ -> ret


code2custom :: H.Type -> H.Type -> J.Ast H.Var x -> J.Ast H.Var x
code2custom from to x =
  case x of
    J.SVar lvar -> J.SVar $ goVar lvar
    J.SDef c var ty init -> 
        J.SDef c (goVar var) (goType ty) init
    J.Function c v args ret body ->
        J.Function c (goVar v) (map (goType *** goVar) args) (goType ret) $ map (code2custom from to) body
    _ -> composeOp (code2custom from to) x

  where
    goVar :: H.Var -> H.Var
    goVar v =
      case v of
        H.Local name ty b id -> H.Local name (goType ty) b id
        H.Global c name ty b id -> H.Global c name (goType ty) b id
        H.Fn name args ret id -> H.Fn name (map goType args) (goType ret) id
        _ -> v
        
    goType :: H.Type -> H.Type
    goType ty = if ty == from then to else ty

cleanup :: J.Ast H.Var x -> J.Ast H.Var x
cleanup x =
  case x of
    J.Call (H.Fn "_Wrap_i2code" _ _ _) [J.Null] -> unsafeCoerce J.Null
    _ -> composeOp cleanup x

compile :: J.Ast H.Var x -> J.Ast H.Var x
compile =
    cleanup .
    code2custom "_replace_code" "integer" .
    compileReplace .
    compileNull "" .
    code2custom "code" "_replace_code"

