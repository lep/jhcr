{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Hot.Jass.Init where

import Control.Lens hiding (Const)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader


import Data.Int
import Data.Monoid
import Data.List
import Data.String
import Data.Function
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map


import Jass.Ast hiding (fmap, foldMap, traverse)

import Hot.Var (Var, mkFn, mkGlobal, mkLocal, mkOp, getId, getId', nameOf, (##))
import qualified Hot.Var as H

import Unsafe.Coerce
import Debug.Trace

stubify :: Ast Var Programm -> Ast Var Programm
stubify (Programm pr) = Programm $ concatMap go pr
  where
    go :: Ast Var Toplevel -> [Ast Var Toplevel]
    go e =
      case e of
        Function c n args ret body ->
            [ Function c ("_" ## n) args ret body
            , Function c n args ret [
                If (Call (mkFn "_modified") [Int $ getId' n]) [
                      Call (mkFn "_enterFunction") [Int $ getId' n]
                    ] [
                    ] (Just [
                      Call ("_" ## n) $ map (Var . SVar . snd) args
                    ])
              ]
            ]
        _ -> [e]



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
  let sorted = sortOn signatur $ filter hasSignatur p
      (vars, functions) = partition isVar sorted
      (arrays, normals) = partition isArray vars
    in (g normals, g arrays, g functions)
  where
    g = groupBy ((==) `on` signatur)


generate :: Ast Var Programm -> [Ast Var Programm]
generate pr =
    let (normals, arrays, functions) = split pr
        i2code_dummies = Programm $ i2code $ concat functions

        enter_predefined = Programm [ enterFunction $ concat functions ]

        set_get = Programm $ concat [
                map generateNormalGetters normals
              , map generateNormalSetters normals
              , map generateArraySetters arrays
              , map generateArrayGetters arrays
              ]
    in [i2code_dummies, enter_predefined, set_get]
  where
    uid = mkLocal "_i"
    idx = mkLocal "_idx"
    val = mkLocal "_v"

    bind = mkLocal "_binding"
    scope = mkLocal "_scope"

    --dummyCodeVars = [Global $ SDef Normal (H.Global Normal ("_dummy_code" <> fromString (show i)) "code" False 0) "code" Nothing | i <- [0..100]]

    i2code :: [Ast Var Toplevel] -> [Ast Var Toplevel]
    i2code fns =

        let r :: Int -> Ast Var Stmt
            r idx = let fn = (fns' ++ fns) !! (100+idx)
                        (v, args) = case fn of
                            Native _ v args _ -> (Code v, args)
                            Function _ v args _ _ -> (Code v, args)
                            Global (SDef _ v _ _) -> (Var $ SVar v, [])
                    in if null args
                    then Return $ Just v
                    else Return $ Just Null


            mkDummyFn idx =
                let idx' = fromString . show $ negate idx
                in Function Normal (mkFn $ "_dummyFunction_" <> idx') [] "nothing" [
                    Call (mkFn "_call_anything_around") [Call (H.Op "-") [Int idx']] 
                    --Return . Just . Var $ AVar (mkGlobal "_dummyFunctions") (Int idx')
                ]
            fns' :: [Ast Var Toplevel]
            fns' = map mkDummyFn [-1, -2 .. -101] 
        in fns' ++ [Function Normal (mkFn "_i2code") [("integer", uid)] "code" [
            bin (-100) (length fns) (Var $ SVar uid) r
        ]]
                    

    enterFunction :: [Ast Var Toplevel] -> Ast Var Toplevel
    enterFunction fns =
        let r idx = let fn = fns !! pred idx
                        v@(H.Fn _ types ret _) = case fn of
                                                    Native _ v _ _ -> v
                                                    Function _ v _ _ _ -> v
                        args = zipWith (\ty pos -> Call (mkFn $ "_get_" <> ty) [Var $ SVar bind, Int . fromString $ show pos] ) types [1, 2 ..]
                        stmt = if ret == "nothing"
                               then Call v args
                               else Call (mkFn $ "_set_" <> ret) [Var $ SVar scope, Int "0", Call v args]
                    in stmt
        
        in Function Normal (mkFn "_call_predefined") [("integer", uid)] "nothing" [
               bin 1 (length fns) (Var $ SVar uid) r
           ]

    -- TODO: sort on uid and avoid (!!)
    generateArrayGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArrayGetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Return . Just . Var $ AVar (v) (Var $ SVar idx)
        in Function Normal (mkFn $ "_array_get_global_" <> ty) [("integer", uid), ("integer", idx)] ty [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateArraySetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArraySetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Set (AVar (v) (Var $ SVar idx)) (Var $ SVar val)
        in Function Normal (mkFn $ "_array_set_global_" <> ty) [("integer", uid), ("integer", idx), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateNormalSetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalSetters g@(x:_) =

        let Global (SDef _ _ ty _) = x

            --offset = length $ dropWhile isConst g
            r idx = let Global (SDef c v _ _) = g !! pred idx
                    in if c == Normal
                    then Set (SVar (v)) (Var $ SVar val)
                    else Return Nothing
        in Function Normal (mkFn $ "_set_global_" <> ty) [("integer", uid), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r    
        ]

    isConst (Global (SDef Const _ _ _)) = True
    isConst _ = False


    generateNormalGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalGetters g@(x:_) =
        let Global (SDef _ v ty _) = x
            r idx = let Global (SDef _ v _ _) = g !! pred idx
                    in Return . Just . Var $ SVar v
        in Function Normal (mkFn $ "_get_global_" <> ty) [("integer", uid)] ty $ [bin 1 (length g) (Var $ SVar uid) r]

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
-----------------------------------------

isOp :: Name -> Bool
isOp x = x `elem` (["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "and", "or", "not"] :: [Name])

type IsArray = Bool

data RenameVariablesState = RenameVariablesState
    { _globalScope :: Map Name Var
    , _globalCount :: Map (Type, IsArray) Int32
    , _localScope :: Map Name Var
    , _fnScope :: Map Name Var
    }
makeLenses ''RenameVariablesState

defaultRenameVariableState = RenameVariablesState mempty mempty mempty mempty


newtype RenameVariablesM a = RenameVariablesM { unRenameVariablesM :: ReaderT (Int32 -> Int32) (State RenameVariablesState) a }
    deriving (Functor, Applicative, Monad, MonadState RenameVariablesState, MonadReader (Int32 -> Int32))

runRenameM :: RenameVariablesState -> Ast Name a -> (Ast Var a, RenameVariablesState)
runRenameM st = flip runState st . flip runReaderT id . unRenameVariablesM . renameVariables

runRenameM' = runRenameM defaultRenameVariableState

evalRenameM :: RenameVariablesState -> Ast Name a -> RenameVariablesState
evalRenameM st a = snd (runRenameM st a)

evalRenameM' = evalRenameM defaultRenameVariableState

execRenameM :: RenameVariablesState -> Ast Name a -> Ast Var a
execRenameM st a = fst (runRenameM st a)

execRenameM' = execRenameM defaultRenameVariableState


addLocal :: Name -> Type -> IsArray -> RenameVariablesM Var
addLocal name ty isArray = do
    r <- ( . fromIntegral) <$> ask
    id <- uses localScope (r . succ . Map.size)
    let v = H.Local name ty isArray id
    localScope %= (at name ?~ v)
    return v

addGlobal :: Constant -> Name -> Type -> IsArray -> RenameVariablesM Var
addGlobal c name ty isArray = do
    r <- ( . fromIntegral) <$> ask
    globalCount %= (Map.insertWith (+) (ty, isArray) 1)
    id <- uses globalCount (r . (Map.! (ty, isArray)))
    let v = H.Global c name ty isArray id
    globalScope %= (at name ?~ v)
    return v

addFunction :: Name -> [Type] -> Type -> RenameVariablesM Var
addFunction name args ret = do
    r <- ( . fromIntegral) <$> ask
    id <- uses fnScope (r . succ . Map.size)
    let v = H.Fn name args ret id
    fnScope %= (at name ?~ v)
    return v

getVar :: Name -> RenameVariablesM Var
getVar n = do
   gt <- use globalScope
   fn <- use fnScope
   lt <- use localScope
   let g = First $ Map.lookup n gt
   let f = First $ Map.lookup n fn
   let l = First $ Map.lookup n lt
   return . fromJust . getFirst $ l <> g <> f


enter :: RenameVariablesM ()
enter = localScope .= mempty

{-
This renames all functions, globals and locals.
Locals will start with 1 for the first parameter and count up.
Globals are counted by their primary key (Type, IsArray).
Functions and Natives start at 1 and are counted up.
-}
renameVariables :: Ast Name a -> RenameVariablesM (Ast Var a)
renameVariables = go
  where
    go :: Ast Name a -> RenameVariablesM (Ast Var a)
    go e =
      case e of
        Native c n args ret -> 
            Native c <$> addFunction n (map fst args) ret
                          <*> pure (map (\(typ, name) -> (typ, mkLocal name)) args)
                          <*> pure ret

        Function c name args ret body -> do
            enter
            name' <- addFunction name (map fst args) ret
            argNames <- mapM (\(ty, name) -> addLocal name ty False) args
            let args' = zip (map fst args) argNames
            body' <- mapM go body
            return $ Function c name' args' ret body'

        Global (ADef name ty) ->
            Global <$> (ADef <$> addGlobal Normal name  ty True <*> pure ty)

        Global (SDef c name ty init) ->
            Global <$> (SDef c <$> addGlobal c name ty False <*> pure ty <*> traverse go init)

        Local (SDef c name ty init)
            -> Local <$> (SDef c <$> addLocal name ty False <*> pure ty <*> traverse go init)

        Local (ADef name ty)
            -> Local <$> (ADef <$> addLocal name  ty True <*> pure ty)

        AVar name idx -> AVar <$> getVar name <*> go idx
        SVar name -> SVar <$> getVar name

        Var v -> Var <$> go v

        Set lvar e -> Set <$> go lvar <*> go e

        Call fn args | isOp fn -> Call (mkOp fn) <$> mapM go args
        Call fn args -> Call <$> getVar fn <*> mapM go args
        Code fn -> Code <$> getVar fn

        Programm p -> Programm <$> mapM go p
        Loop p -> Loop <$> mapM go p

        If cond tb eis eb ->
            If <$> go cond
                    <*> mapM go tb
                    <*> mapM go' eis
                    <*> traverse (mapM go) eb
          where
            go' (cond, body) = (,) <$> go cond <*> mapM go body


        Exitwhen cond -> Exitwhen <$> go cond
        Return e -> Return <$> traverse go e

        _ -> return $ unsafeCoerce e


{-
This should be used to parse .j-files which will not be changed, e.g.
common.j and Blizzard.j
-}
onlyParseToplevel :: Ast Name Programm -> RenameVariablesM ()
onlyParseToplevel (Programm pr) = mapM_ go pr
  where
    go :: Ast Name Toplevel -> RenameVariablesM ()
    go e =
      case e of
        Native _ name args ret    -> void $ addFunction name (map fst args) ret
        Function _ name args ret _ -> void $ addFunction name (map fst args) ret
        Global (SDef c name ty _)  -> void $ addGlobal c name ty False
        Global (ADef name ty)    -> void $ addGlobal Normal name ty True


----------------------------------------
