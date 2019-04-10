{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Hot.Init.Rename
    ( Mode(..)
    , compile
    , compile'
    ) where


import Control.Lens hiding (Const)

import Control.Monad.State
import Control.Monad.Reader


import Data.Int
import Data.Monoid
import Data.Maybe

import GHC.Generics
import Data.Binary


import Data.Map (Map)
import qualified Data.Map as Map


import Jass.Ast hiding (fmap, foldMap, traverse)


import Hot.Var (Var, mkLocal, mkOp)
import qualified Hot.Var as H

import Unsafe.Coerce


isOp :: Name -> Bool
isOp x = x `elem` (["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "not"] :: [Name])

type IsArray = Bool

data Mode = Init | Update deriving (Eq)

data RenameVariablesState = RenameVariablesState
    { _globalScope :: Map Name Var
    , _globalCount :: Map (Type, IsArray) Int32
    , _localScope :: Map Name Var
    , _fnScope :: Map Name Var
    , _newFnCount :: Int32
    } deriving (Generic)
makeLenses ''RenameVariablesState

instance Binary RenameVariablesState

defaultRenameVariableState :: RenameVariablesState
defaultRenameVariableState = RenameVariablesState mempty mempty mempty mempty 0


newtype RenameVariablesM a = RenameVariablesM { unRenameVariablesM :: ReaderT (Mode, Type -> Type) (State RenameVariablesState) a }
    deriving (Functor, Applicative, Monad, MonadState RenameVariablesState, MonadReader (Mode, Type -> Type))

compile :: Mode -> (Type -> Type) -> RenameVariablesState -> Ast Name a -> (Ast Var a, RenameVariablesState)
compile m f st = flip runState st . flip runReaderT (m,f) . unRenameVariablesM . renameVariables

compile' :: Mode -> (Type -> Type) -> Ast Name a -> (Ast Var a, RenameVariablesState)
compile' m f = compile m f defaultRenameVariableState


addLocal :: Name -> Type -> IsArray -> RenameVariablesM Var
addLocal name ty isArray = do
    v' <- uses globalScope $ Map.lookup name
    f <- asks snd
    case v' of
        Just v -> return v
        Nothing -> do
            let successor = if isArray then (+32768) else (+1)
            id <- uses localScope (fromIntegral . successor . Map.size)
            let v = H.Local name (f ty) isArray id
            localScope %= (at name ?~ v)
            return v

addGlobal :: Constant -> Name -> Type -> IsArray -> RenameVariablesM Var
addGlobal c name ty isArray = do
    v' <- uses globalScope $ Map.lookup name
    (m, f) <- ask
    let idf = if m == Init then id else idf' . negate
        idf' = if isArray then (*32768) else id
    case v' of
        Just v -> do
            let sig = H.Global c name ty isArray (H.getId v)
            if sig == v
            then return v
            else do
                globalScope %= sans name
                addGlobal c name ty isArray
        Nothing -> do
            globalCount %= Map.insertWith (+) (f ty, isArray) 1
            id <- idf <$> uses globalCount (Map.findWithDefault (error "xxx") (f ty, isArray))
            let v = H.Global c name (f ty) isArray id
            globalScope %= (at name ?~ v)
            return v

addFunction :: Name -> [Type] -> Type -> RenameVariablesM Var
addFunction name args ret = do
    v' <- uses fnScope $ Map.lookup name
    case v' of
        Just v -> do
            let sig = H.Fn name args ret (H.getId v)
            -- potentially update if signature has changed
            if v == sig
            then return v
            else do
                fnScope %= (at name ?~ sig)
                return sig
        Nothing -> do
            (mode, f) <-  ask
            case mode of
                Init -> do
                    id <- uses fnScope (fromIntegral . succ . Map.size)
                    let v = H.Fn name (map f args) (f ret) id
                    fnScope %= (at name ?~ v)
                    return v
                    
                Update -> do
                    id <- newFnCount <-= 1
                    let v = H.Fn name (map f args) (f ret) id
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
   return . fromMaybe (error $ show n) . getFirst $ l <> g <> f


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