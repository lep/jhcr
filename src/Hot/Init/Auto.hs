{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Hot.Init.Auto (compile) where

import Data.List
import Data.String
import Data.Function

import qualified Data.Map as Map
import qualified Data.Set as Set

import Jass.Ast hiding (fmap, foldMap, traverse)

import Hot.Var (Var, mkFn, mkLocal, mkOp, mkGlobal)
import qualified Hot.Var as H
import qualified Hot.Types as Hot

data Signatur = FunctionSig [Type] Type
              | GlobalDef Type Bool
  deriving (Eq, Ord, Show)


hasSignatur :: Ast a Toplevel -> Bool
hasSignatur Function{} = True
hasSignatur Native{} = True
hasSignatur Global{} = True
hasSignatur _ = False

signatur :: Ast a Toplevel -> Signatur
signatur e =
  case e of
    Native _ _ args ret -> FunctionSig (map fst args) ret
    Function _ _ args ret _ -> FunctionSig (map fst args) ret
    Global (ADef _ t) -> GlobalDef t True
    Global (SDef _ _ t _) -> GlobalDef t False

isVar :: Ast a Toplevel -> Bool
isVar Global{} = True
isVar _ = False

isArray :: Ast a Toplevel -> Bool
isArray (Global ADef{}) = True
isArray _ = False


split :: Ast a Programm -> ([[Ast a Toplevel]], [[Ast a Toplevel]], [[Ast a Toplevel]])
split (Programm p) =
  let sorted = {-sortOn signatur $-} filter hasSignatur p
      (vars, functions) = partition isVar sorted
      (arrays, normals) = partition isArray vars
    in (g normals, g arrays, [functions])
  where
    g = groupBy ((==) `on` signatur) . sortOn signatur

compile :: Ast Var Programm -> [Ast Var Programm]
compile pr =
    let (normals, arrays, functions) = split pr
        unusedNormalTypes = Set.toList . Set.delete "nothing" $ Map.keysSet Hot.types Set.\\ Set.fromList (concatMap (map typeOf) normals)
        unusedArrayTypes =  Set.toList . Set.delete "nothing" $ Map.keysSet Hot.types Set.\\ Set.fromList (concatMap (map typeOf) arrays)
        i2code_dummies = Programm $ i2code $ concat functions

        enter_predefined = Programm [ enterFunction $ concat functions ]

        set_get_avail =
              [ map generateNormalGetters normals
              , map generateNormalSetters normals
              , map generateArraySetters arrays
              , map generateArrayGetters arrays
              ]

        set_get_empty =
          [ map generateEmptyNormalSetters unusedNormalTypes
          , map generateEmptyNormalGetters unusedNormalTypes
          , map generateEmptyArraySetters unusedArrayTypes
          , map generateEmptyArrayGetters unusedArrayTypes
          ]

        set_get = Programm $ concat set_get_avail <> concat set_get_empty

    in [i2code_dummies, enter_predefined, set_get]
  where
    defaultReturnValue "real" = Real "0.0"
    defaultReturnValue "integer" = Int "0"
    defaultReturnValue "string" = String ""
    defaultReturnValue "code" = Var $ SVar $ mkGlobal "_Wrap_Null"
    defaultReturnValue "boolean" = Bool False
    defaultReturnValue _ = Null

    generateEmptyArrayGetters ty =
        Function Normal (mkFn $ "_Auto_array_get_global_" <> ty) [("integer", uid), ("integer", idx)] ty [
            Return $ Just $ defaultReturnValue ty
        ]

    generateEmptyNormalGetters ty =
        Function Normal (mkFn $ "_Auto_get_global_" <> ty) [("integer", uid)] ty [
            Return $ Just $ defaultReturnValue ty
        ]

    generateEmptyArraySetters ty =
        Function Normal (mkFn $ "_Auto_array_set_global_" <> ty) [("integer", uid), ("integer", idx), (ty, val)] "nothing" []

    generateEmptyNormalSetters ty =
        Function Normal (mkFn $ "_Auto_set_global_" <> ty) [("integer", uid), (ty, val)] "nothing" []

    typeOf :: Ast Var Toplevel -> Type
    typeOf (Global (ADef _ ty)) = ty
    typeOf (Global (SDef _ _ ty _)) = ty


    uid = mkLocal "_i"
    idx = mkLocal "_idx"
    val = mkLocal "_v"

    ctx = mkLocal "_ctx"

    bind = AVar (mkLocal "_Context_bindings") (Var $ SVar ctx)
    scope = AVar (mkLocal "_Context_locals") (Var $ SVar ctx)

    {-
     - This creates the _Auto_dummyFunction<n> and the _Auto_i2code
     - functions
     -}
    i2code :: [Ast Var Toplevel] -> [Ast Var Toplevel]
    i2code fns =

        let r :: Int -> Ast Var Stmt
            r idx = let fn = (fns' ++ fns) !! (100+idx)
                        (v, args, name, isnative) = case fn of
                            Native _ v args _ -> (Code v, args, v, True)
                            Function _ v args _ _ -> (Code v, args, v, False)
                    in if
                        | isnative -> donothing
                        | H.nameOf name `elem` donttouch -> donothing
                        | null args -> Return $ Just v
                        | otherwise -> donothing
            donothing = Return . Just . Var . SVar $ mkGlobal "_Wrap_Null"


            mkDummyFn idx =
                let idx' = fromString . show $ negate idx
                in Function Normal (mkFn $ "_Auto_dummyFunction_" <> idx') [] "nothing" [
                    Call (mkFn "_Wrap_call_anything_around") [Call (H.Op "-") [Int idx']]
                ]
            fns' :: [Ast Var Toplevel]
            fns' = map mkDummyFn [-1, -2 .. -101]
        in fns' ++ [Function Normal (mkFn "_Auto_i2code") [("integer", uid)] "code" [
            bin (-100) (length fns) (Var $ SVar uid) r
        ]]



    {-
     - This creates the _Auto_call_predefined function.
     - The _call_predefined function is used to call a native, blizzard-j
     - or otherwise defined function. This uses the internal ID of each
     - function and calls the function with the context coming from an
     - active interpreter context. This function is basically the call
     - instruction in the interpreter.
    -}
    enterFunction :: [Ast Var Toplevel] -> Ast Var Toplevel
    enterFunction fns =
        let reg = mkLocal "_reg"
            r idx =
                let fn = fns !! pred idx
                    v@(H.Fn _ types ret _) = case fn of
                                                Native _ v _ _ -> v
                                                Function _ v _ _ _ -> v
                    args = zipWith (\ty pos ->
                                if ty == "code"
                                then Call (mkFn "_Auto_i2code") [Call (mkFn "_Table_get_integer") [Var bind, Int . fromString $ show pos]]
                                else Call (mkFn $ "_Table_get_" <> ty) [Var bind, Int . fromString $ show pos] )
                            types [1, 2 ..]

                    stmt
                      | H.nameOf v `elem` donttouch = Call (mkFn "DoNothing") []
                      | ret == "nothing" = Call v args
                      | ret == "code"    = Call (mkFn "_Table_set_code") [Call (mkFn "_Auto_i2code") [Var scope, Var $ SVar reg, Call v args]]
                      | otherwise        = Call (mkFn $ "_Table_set_" <> ret) [Var scope, Var $ SVar reg, Call v args ]

                in stmt

        in Function Normal (mkFn "_Auto_call_predefined") [("integer", reg), ("integer", uid), ("integer", ctx)] "nothing" [
               bin 1 (length fns) (Var $ SVar uid) r
           ]

    -- TODO: sort on uid and avoid (!!)
    generateArrayGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArrayGetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Return . Just . Var $ AVar v (Var $ SVar idx)
        in Function Normal (mkFn $ "_Auto_array_get_global_" <> ty) [("integer", uid), ("integer", idx)] ty [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateArraySetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArraySetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Set (AVar v (Var $ SVar idx)) (Var $ SVar val)
        in Function Normal (mkFn $ "_Auto_array_set_global_" <> ty) [("integer", uid), ("integer", idx), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateNormalSetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalSetters g@(x:_) =

        let Global (SDef _ _ ty _) = x
            r idx = let Global (SDef c v _ _) = g !! pred idx
                    in if c == Normal
                    then Set (SVar v) (Var $ SVar val)
                    else Return Nothing
        in Function Normal (mkFn $ "_Auto_set_global_" <> ty) [("integer", uid), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateNormalGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalGetters g@(x:_) =
        let Global (SDef _ _ ty _) = x
            r idx = let Global (SDef _ v _ _) = g !! pred idx
                    in Return . Just . Var $ SVar v
        in Function Normal (mkFn $ "_Auto_get_global_" <> ty) [("integer", uid)] ty
            [bin 1 (length g) (Var $ SVar uid) r]

    donttouch =
      [ "main", "config", "InitCustomPlayerSlots", "SetPlayerSlotAvailable"
      , "InitGenericPlayerSlots", "InitCustomTeams", "InitCustomTriggers"
      , "CreateAllUnits", "InitBlizzard", "InitGlobals"
      ]

bin :: Int -> Int -> Ast Var Expr -> (Int -> Ast Var Stmt) -> Ast Var Stmt
bin lo hi c f = go lo (hi+1)
  where
    go lo hi
        | lo +1== hi = f lo
        | otherwise =
            let mid = (lo+hi) `div` 2
                thenB = go lo mid
                elseB = go mid hi
            in If (Call (mkOp "<") [c, Int . fromString $ show mid]) [thenB] [] (Just [elseB])
