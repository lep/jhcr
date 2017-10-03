{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hot.Ast
    ( Programm, Toplevel, LVar, Stmt, Expr, Name, Type
    , Var (..)
    , Ast (..)
    , compile
    , countLocals, renameLocals, renameLocals', convertNamesToUniqueIds
    ) where


import qualified Jass.Ast as Jass
import Jass.Ast (Programm, Toplevel, LVar, Stmt, Expr, Name, Type)

import Data.Composeable

import Control.Arrow (second)

import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Data.DList (DList)
import qualified Data.DList as DList

import Data.Int

import Data.Maybe 

import Data.ByteString.Lazy (ByteString)

import Control.Lens
import Control.Monad.State

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

data Var = Local Type Int
         | Global Type Int
         | Op Name
         | Fn [Type] Type Int
    deriving (Eq, Ord, Show)

type Scope = Map Var

data Ast var a where
    Programm :: [Ast var Toplevel] -> Ast var Programm
    Function :: var -> [(Type, var)] -> Type -> Ast var Stmt -> Ast var Toplevel


    Block :: [Ast var Stmt] -> Ast var Stmt

    Set :: Ast var LVar -> Ast var Expr -> Ast var Stmt
    If :: Ast var Expr -> Ast var Stmt -> Ast var Stmt -> Ast var Stmt
    Loop :: Ast var Stmt -> Ast var Stmt
    Exitwhen :: Ast var Expr -> Ast var Stmt
    Return :: Maybe (Ast var Expr) -> Ast var Stmt

    Call :: var -> [Ast var Expr] -> Ast var a

    Var :: Ast var LVar -> Ast var Expr
    Int :: Int32 -> Ast var Expr
    Real :: Float -> Ast var Expr
    Bool :: Bool -> Ast var Expr
    String :: ByteString -> Ast var Expr
    Code :: var -> Ast var Expr
    Null :: Ast var Expr

    AVar :: var -> Ast var Expr -> Ast var LVar
    SVar :: var -> Ast var LVar

deriving instance (Show var) => Show (Ast var a)

newtype ScopeMonad a = ScopeMonad { runScopeMonad :: State ScopeS a }
    deriving (Functor, Applicative, Monad, MonadState ScopeS)


emptyState = ScopeS mempty mempty 0

jass2hot:: Jass.Ast Var Programm -> Ast Var Programm
jass2hot (Jass.Programm p) = Programm $ mapMaybe go p
  where
    go :: Jass.Ast Var a -> Maybe (Ast Var a)
    go e =
      case e of
        Jass.Native{} -> Nothing
        Jass.Typedef{} -> Nothing
        Jass.Global{} -> Nothing

        Jass.Function _ n args ret body -> Just $ Function n args ret $ Block $ mapMaybe convertStmt body

    convertStmt :: Jass.Ast Var a -> Maybe (Ast Var a)
    convertStmt e =
      case e of
        Jass.Set lvar expr -> Just $ Set (convert lvar) (convert expr)
        Jass.If{} -> Just $ convertIfElse $ Jass.eliminateElseIfs e
        Jass.Loop body -> Just $ Loop $ Block $ mapMaybe convertStmt body
        Jass.Exitwhen cond -> Just $ Exitwhen $ convert cond
        Jass.Return e -> Just $ Return $ fmap convert e
        Jass.Call fn args -> Just $ Call fn $ fmap convert args
        Jass.Local (Jass.SDef _ n _ (Just e)) -> convertStmt $ Jass.Set (Jass.SVar n) e
        --Jass.Local (ADef

        _ -> Nothing

    convert :: Jass.Ast Var a -> Ast Var a
    convert e =
      case e of
        Jass.Call fn args -> Call fn $ fmap convert args
        Jass.Var v -> Var $ convert v
        Jass.Int i -> Int $ Jass.s2i i
        Jass.Rawcode i -> Int $ Jass.rawcode2int i
        Jass.Real r -> Real $ Jass.s2r r
        Jass.Bool b -> Bool b
        Jass.String s -> String s
        Jass.Code c -> Code c
        Jass.Null -> Null
        Jass.AVar n idx -> AVar n $ convert idx
        Jass.SVar v -> SVar v

    convertIfElse :: Jass.Ast Var Stmt -> Ast Var Stmt
    convertIfElse (Jass.If cond body [] elseB) =
        If (convert cond)
           (Block $ mapMaybe convertStmt body)
           (Block $ mapMaybe convertStmt $ concat elseB)

data ScopeS = ScopeS { _globalScope :: Map Name Var
                     , _localScope :: Map Name Var
                     , _uid :: Int
                     }
makeLenses ''ScopeS



enter :: ScopeMonad ()
enter = localScope .= mempty

getVar :: Name -> ScopeMonad Var
getVar n = do
   gt <- use globalScope
   lt <- use localScope
   let g = First $ Map.lookup n gt
   let l = First $ Map.lookup n lt
   return . fromJust . getFirst $ l <> g

addLocal :: Type -> Name -> ScopeMonad Var
addLocal t n = do
    v <- Local t <$> (uid <+= 1)
    localScope %= (at n ?~ v)
    return v

addGlobal :: Type -> Name -> ScopeMonad Var
addGlobal t n = do
    v <- Global t <$> (uid <+= 1)
    globalScope %= (at n ?~ v)
    return v

addFunction :: [Type] -> Type -> Name -> ScopeMonad Var
addFunction args ret n = do
    v <- Fn args ret <$> (uid <+= 1)
    globalScope %= (at n ?~ v)
    return v

isOp :: Name -> Bool
isOp x = x `elem` (["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "and", "or", "not"] :: [Name])

name2ids :: Jass.Ast Name a -> ScopeMonad (Jass.Ast Var a)
name2ids e =
  case e of
    Jass.Native c n args ret ->
        Jass.Native c <$> addFunction (map fst args) ret n
                      <*> pure (map (\(typ, name) -> (typ, Local typ 0)) args)
                      <*> pure ret


    Jass.Function c name args ret body -> do
        enter
        name' <- addFunction (map fst args) ret name
        names' <- mapM (uncurry addLocal) args
        let args' = zip (map fst args) names'
        body' <- mapM name2ids body
        return $ Jass.Function c name' args' ret body'

    Jass.Global (Jass.ADef name typ) -> Jass.Global <$> (Jass.ADef <$> addGlobal typ name<*> pure typ)
    Jass.Global (Jass.SDef c name typ init) -> Jass.Global <$> (Jass.SDef c <$> addGlobal typ name <*> pure typ <*> traverse name2ids init)


    Jass.Local (Jass.ADef name typ) -> Jass.Local <$> (Jass.ADef <$> addLocal typ name <*> pure typ)



    Jass.Local (Jass.SDef c name typ init) ->
        Jass.Local <$> (Jass.SDef c <$> addLocal typ name <*> pure typ <*> traverse name2ids init)


    Jass.AVar name idx -> Jass.AVar <$> getVar name <*> name2ids idx
    Jass.SVar name -> Jass.SVar <$> getVar name

    Jass.Var v -> Jass.Var <$> name2ids v

    Jass.Set lvar e -> Jass.Set <$> name2ids lvar <*> name2ids e

    Jass.Call fn args | isOp fn -> Jass.Call (Op fn) <$> mapM name2ids args
    Jass.Call fn args -> Jass.Call <$> getVar fn <*> mapM name2ids args
    Jass.Code fn -> Jass.Code <$> getVar fn

    Jass.Programm p -> Jass.Programm <$> mapM name2ids p
    Jass.Loop p -> Jass.Loop <$> mapM name2ids p

    Jass.If cond tb eis eb ->
        Jass.If <$> name2ids cond
                <*> mapM name2ids tb
                <*> mapM go eis
                <*> traverse (mapM name2ids) eb
      where
        go (cond, body) = (,) <$> name2ids cond <*> mapM name2ids body


    Jass.Exitwhen cond -> Jass.Exitwhen <$> name2ids cond
    Jass.Return e -> Jass.Return <$> traverse name2ids e

    --Jass.Int i -> return $ Jass.Int i
    --Jass.Bool b -> return $ Jass.Bool b
    --_ -> error $ unwords ["Unhandled case", show e]

    _ -> return $ unsafeCoerce e



renameLocals :: Jass.Ast Var a -> State (Int, Map Int Var) (Jass.Ast Var a)
renameLocals e =
  case e of
    Jass.Function c n args ret body -> do
        _1 .= 0
        let (types, names) = unzip args
        names' <- mapM rename names

        body' <- mapM renameLocals body
        return $ Jass.Function c n (zip types names') ret body'

    Jass.SDef c v t init -> Jass.SDef c <$> rename v <*> pure t <*> traverse renameLocals init
    Jass.ADef v t -> Jass.ADef <$> rename v <*> pure t
    Jass.SVar l -> Jass.SVar <$> rename l
    Jass.AVar l idx -> Jass.AVar <$> rename l <*> renameLocals idx
    _ -> composeM renameLocals e
  where
    rename :: Var -> State (Int, Map Int Var) Var
    rename v =
      case v of
        Local t n -> do
            m <- use _2
            case Map.lookup n m of
                Just l -> return l
                Nothing -> do
                    n' <- _1 <+= 1
                    _2 %= (at n ?~ Local t n')
                    return $ Local t n'
        _ -> return v

compile :: Jass.Ast Name Programm -> (Ast Var Programm, Map Var Int)
compile ast = (jass2hot ast', m)
  where
    ast' = renameLocals' $ convertNamesToUniqueIds ast
    m = countLocals ast'


renameLocals' = flip evalState (0, mempty) . renameLocals
convertNamesToUniqueIds = flip evalState emptyState . runScopeMonad . name2ids



countLocals :: Jass.Ast Var Programm -> Map Var Int
countLocals (Jass.Programm toplevel) = 
    foldMap (\f -> Map.singleton (nameOf f) (go f)) $
    filter isFunction toplevel
  where
    isFunction (Jass.Function{}) = True
    isFunction _ = False

    nameOf :: Jass.Ast Var Toplevel -> Var
    nameOf (Jass.Function _ n _ _ _) = n

    go :: Jass.Ast Var Toplevel -> Int
    go e =
      case e of
        Jass.Function _ _ args _ body ->
            length args + sum (map count body)
        _ -> 0

    count (Jass.Local _) = 1
    count _ = 0
