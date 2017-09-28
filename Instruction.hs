
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}


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

{-
type Register = Int

type RegisterOrLiteral = Either Register String

data Instruction
    = Lt Type Register Register RegisterOrLiteral
    | Le Type Register Register RegisterOrLiteral
    | Gt Type Register Register RegisterOrLiteral
    | Ge Type Register Register RegisterOrLiteral
    | Eq Type Register Register RegisterOrLiteral
    | Neq Type Register Register RegisterOrLiteral
    | Add Type Register Register RegisterOrLiteral
    | Sub Type Register Register RegisterOrLiteral
    | Mul Type Register Register RegisterOrLiteral
    | Div Type Register Register RegisterOrLiteral
    
    | Set Type Register RegisterOrLiteral
    | SetArray Type Register Register RegisterOrLiteral
    | GetArray Type Register Register RegisterOrLiteral
    | SetGlobal Type Register RegisterOrLiteral
    | GetGlobal Type Register Register
    | SetGlobalArray Type Register Register RegisterOrLiteral
    | GetGlobalArray Type Register Register RegisterOrLiteral

    | Call Type Register Register
    | Bind Type Register RegisterOrLiteral

    -- Can't use And/Or due to their lazy semantics
    -- Gonna use a bunch of ifs
    -- | And Register Register Register
    -- | Or Register Register Register
    | Not Register Register

    | I2R Register Register

    | Label Register
    | Jmp Register
    | JmpT Register Register

    | Ret
-}


-- when compiling to bytecode we dont care about sequential ids
data Var = Local Int Type
         | Global Int Type
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


compileExpression :: Ast Var Expr -> CompileMonad RegisterOrLiteral
compileExpression e =
  case e of
    Call "<" [a, b] -> do
        t <- getNumericType a b
        r <- fresh
        [Register a', b'] <- mapM compileExpression [a, b]
        emit $ Lt t r a' b'
    Call "*" [a, b] -> do
        -- int * real
        t <- getNumericType a b



{-
newtype ScopeMonad = Identity

name2ids :: Jass.Ast Name a -> ScopeMonad (Jass.Ast Var a)
name2ids e =
  case e of
    Function c name args body ret -> do
        enter
        name' <- newId name ret
        args' <- mapM (uncurry newId) args
        body' <- mapM name2ids body
        return $ Function c name' args' body' ret

    Global (ADef name typ) -> Global . ADef <$> newId name typ
    Global (SDef c name typ init) -> Global . SDef c <$> newId name typ <*> traverse name2ids init

    Local (ADef name typ) -> Local . ADef <$> newId name typ
    Local (SDef name typ init) -> Local . SDef <$> newId name typ <*> traverse name2ids init

    AVar name idx -> AVar <$> getVar name <*> name2ids idx
    SVar name -> SVar <$> getVar name

    Call fn args -> Call <$> getVar fn <*> mapM name2ids args
    Code fn -> Code <$> getVar fn

    -- interesting cases a re over...

    Programm p -> Programm <$> mapM name2ids p
    Loop p -> Loop <$> mapM name2ids p
    If cond tb eis eb -> If <$> name2ids cond <*> mapM go eis <*> fmap (mapM name2ids) eb
      where
        go (cond, body) = (,) <$> name2ids cond <*> mapM name2ids body
    Exitwhen cond -> Exitwhen <$> name2ids cond
    Return e -> Return <$> fmap name2ids e

    _ -> unsafeCoerce e
-}
    




{-
data CMState =
    CMState { _labelId :: Int
            , _varId :: Int
            , _globals :: Map Name Int
            , _locals :: Map Name Int
            }

newtype CompileMonad = State CMState

-- TODO transform: Ast Name a -> Ast (Either Local Global) a

compileToplevel :: Ast Name Toplevel -> CompileMonad [Instruction]
compileToplevel = _

compileStmt :: Ast Name Stmt -> CompileMonad [Instruction]
compileStmt e =
  case e of
    Set (SVar (GlobalV n)) e -> do
        (t, stmts) <- compileExpression e
        typ <- typeof e
        return $ stmts ++ [SetGlobal typ n (Left t)]
    Set (SVar (LocalV n)) e -> do
        (t, stmts) <- compileExpression e
        typ <- typeof e
        return $ stmts ++ [Set typ n (Left t)]
    Return Nothing -> return [Ret]
    Return (Just e) -> do
        (t, stmts) <- compileExpression e
        typ <- typeof e
        return $ stmts ++ [Set typ 0 (Right t), Ret]

compileExpression :: Ast Name Expr -> CompileMonad (Register, [Instruction])
compileExpression = _
-}
