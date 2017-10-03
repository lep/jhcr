
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hot.Instruction
    ( compileExpr, compileStmt, compileToplevel, compileProgram
    , compile
    ) where

import qualified Hot.Ast as Hot
import Hot.Ast (Programm, Toplevel, LVar, Stmt, Expr, Name, Type)
import Hot.Ast ( Var(..) )


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
import Control.Monad.Writer
import Control.Monad.Reader

import Unsafe.Coerce (unsafeCoerce)

type Register = Int
type Label = Int


data Instruction
    = Lt Type Register Register Register
    | Le Type Register Register Register
    | Gt Type Register Register Register
    | Ge Type Register Register Register
    | Eq Type Register Register Register
    | Neq Type Register Register Register
    | Add Type Register Register Register
    | Sub Type Register Register Register
    | Mul Type Register Register Register
    | Div Type Register Register Register
    | Negate Type Register Register

    | Literal Type Register (Hot.Ast Var Expr)
    
    | Set Type Register Register
    | SetArray Type Register Register Register
    | GetArray Type Register Register Register
    | SetGlobal Type Register Register
    | GetGlobal Type Register Register
    | SetGlobalArray Type Register Register Register
    | GetGlobalArray Type Register Register Register

    | Call Type Register Register
    | Bind Type Register Register

    | Not Register Register

    | Label Register
    | Jmp Register
    | JmpT Register Register

    | Convert Type Register Type Register

    | Function Int
    | Ret
    deriving (Show)


newtype CompileMonad a = CompileMonad { runCompileMonad :: ReaderT Type (StateT CompileState (Writer (DList Instruction))) a }
  deriving (Functor, Applicative, Monad, MonadWriter (DList Instruction), MonadState CompileState, MonadReader Type )



name2op n = fromJust $ lookup n [ ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)
                                , ("==", Eq), ("!=", Neq), ("-", Sub)
                                , ("+", Add), ("*", Mul), ("/", Div)
                                ]


-- when compiling to bytecode we dont care about sequential ids

data CompileState = CompileS { _loopStack :: [(Label, Label)]
                             , _labelId :: Int
                             , _scope :: Map Var Int
                             , _registerId :: Int
                             , _localCount :: Map Var Int
                             }
makeLenses ''CompileState

emit :: Instruction -> CompileMonad ()
emit = tell . DList.singleton

push :: (Label, Label) -> CompileMonad ()
push e = loopStack %= (e:)

pop :: CompileMonad ()
pop = loopStack %= tail

peek :: CompileMonad (Label, Label)
peek = uses loopStack head

newLabel :: CompileMonad Label
newLabel = labelId <+= 1

newRegister :: CompileMonad Register
newRegister = registerId <+= 1

isOp :: Var -> Bool
isOp (Op _) = True
isOp _ = False

getVarId :: Var -> CompileMonad Int
getVarId v =
  case v of
    Fn _ _ id -> return id
    Local _ id -> return id
    Global _ id -> return id

isBooleanOp x = x `elem` (["==", "!=", "<", "<=", ">", ">=", "and", "or"] :: [Name])

typeOfExpr :: Hot.Ast Var Expr -> Type
typeOfExpr e =
  case e of
    Hot.Call (Op "-") [a] -> typeOfExpr a
    Hot.Call (Op "not") [a] -> "boolean"
    Hot.Call (Op o) [a, b]
        | isBooleanOp o -> "boolean"
        | otherwise -> numericType (typeOfExpr a) (typeOfExpr b)
    Hot.Call n _ -> typeOfVar n
    Hot.Var (Hot.SVar v) -> typeOfVar v
    Hot.Int{} -> "integer"
    Hot.Real{} -> "real"
    Hot.Bool{} -> "boolean"
    Hot.String{} -> "string"
    Hot.Null -> "handle"

typeOfVar :: Var -> Type
typeOfVar v =
  case v of
    Local t _ -> t
    Global t _ -> t
    Fn _ t _ -> t
    _ -> ""

compileProgram :: Hot.Ast Var Programm -> CompileMonad ()
compileProgram (Hot.Programm toplevel) = mapM_ compileToplevel toplevel

compileToplevel :: Hot.Ast Var Toplevel -> CompileMonad ()
compileToplevel (Hot.Function n args r body) = do
    fn <- Function <$> getVarId n
    emit fn

    labelId .= 1
    m <- use localCount
    registerId .= fromJust (Map.lookup n m)

    typed r $ compileStmt body
    emit Ret

typed t = local (const t)


typedGet sourcetype source = do
    wanted <- ask
    if wanted /= sourcetype
    then do
        r <- newRegister
        emit $ Convert wanted r sourcetype source
        return r
    else
        return source


compileCall :: Hot.Ast Var a -> CompileMonad Register
compileCall (Hot.Call n@(Fn aTypes rType _) args) = do
    r <- newRegister
    v <- getVarId n
    forM_ (zip3 args aTypes [1..]) $ \(arg, typ, pos) -> typed typ $ do
        r <- compileExpr arg
        emit $ Bind typ pos r
    emit $ Call (typeOfVar n) r v
    typedGet (typeOfVar n) r

compileStmt :: Hot.Ast Var Stmt -> CompileMonad ()
compileStmt e =
  case e of
    Hot.Return Nothing -> emit Ret
    Hot.Return (Just e) -> do
        r <- compileExpr e
        emit $ Set (typeOfExpr e) 0 r
        emit Ret

    Hot.Call{} -> void $ compileCall e


    Hot.If cond tb eb -> do
        trueLabel <- newLabel
        joinLabel <- newLabel
        r <- compileExpr cond
        emit $ JmpT trueLabel r
        compileStmt eb
        emit $ Jmp joinLabel
        emit $ Label trueLabel
        compileStmt tb
        emit $ Label joinLabel


    Hot.Loop body -> do
        loopEntry <- newLabel
        loopExit <- newLabel
        push (loopEntry, loopExit)
        
        emit $ Label loopEntry
        compileStmt body
        emit $ Jmp loopEntry
        emit $ Label loopExit
        pop

    Hot.Exitwhen cond -> do
        (_, loopExit) <- peek
        r <- compileExpr cond
        emit $ JmpT loopExit r

    Hot.Set (Hot.SVar v@Hot.Local{}) e -> do
        let t = typeOfVar v
        r <- typed t $ compileExpr e
        vid <- getVarId v
        emit $ Set t vid r

    Hot.Set (Hot.SVar v@Hot.Global{}) e -> do
        let t = typeOfVar v
        r <- typed t $ compileExpr e
        vid <- getVarId v
        emit $ SetGlobal t vid r

    Hot.Block blk -> mapM_ compileStmt blk

numericType "real" _ = "real"
numericType _ "real" = "real"
numericType a _ = a

compileExpr :: Hot.Ast Var Expr -> CompileMonad Register
compileExpr e =
  case e of
    Hot.Call (Op "not") [a] -> do
        r <- newRegister
        t <- compileExpr a
        emit $ Not r t
        return r
    Hot.Call (Op "-") [a] -> do
        r <- newRegister
        t <- compileExpr a
        emit $ Negate (typeOfExpr a) r t
        return r

    Hot.Call (Op "or") [a, b] -> do
        r <- newRegister
        cont <- newLabel
        a' <- typed "boolean" $ compileExpr a
        b' <- typed "boolean" $ compileExpr b
        emit $ Set "boolean" r a'
        emit $ JmpT cont r
        emit $ Set "boolean" r b'
        emit $ Label cont
        return r

    Hot.Call (Op "and") [a, b] -> do
        r <- newRegister
        cont <- newLabel
        a' <- typed "boolean" $ compileExpr a
        b' <- typed "boolean" $ compileExpr b
        emit $ Set "boolean" r a'
        emit $ Not r r
        emit $ JmpT cont r
        emit $ Set "boolean" r b'
        emit $ Label cont
        return r
        
    Hot.Call (Op n) [a, b] -> do
        r <- newRegister
        let t = numericType (typeOfExpr a) (typeOfExpr b)
        a' <- typed t $ compileExpr a
        b' <- typed t $ compileExpr b

        let op = name2op n
        emit $ op (typeOfExpr a) r a' b'

        wanted <- ask
        if wanted /= t
        then do
            s <- newRegister
            emit $ Convert wanted s t r
            return s
        else
            return r

    
    Hot.Call{} -> compileCall e

    Hot.Var (Hot.SVar l@Hot.Local{}) -> getVarId l >>= typedGet (typeOfVar l)
    Hot.Var (Hot.SVar g@Hot.Global{}) -> do
        r <- newRegister
        v <- getVarId g
        emit $ GetGlobal (typeOfVar g) r v
        typedGet (typeOfVar g) r

    Hot.Int _ -> do
        r <- newRegister
        t <- ask
        emit $ Literal t r e
        return r

    Hot.Real _ -> do
        r <- newRegister
        emit $ Literal "real" r e
        return r

    Hot.Bool _ -> do
        r <- newRegister
        emit $ Literal "boolean" r e
        return r

    Hot.Null -> do
        r <- newRegister
        t <- ask
        emit $ Literal t r e
        return r

compile :: Map Var Int -> Hot.Ast Var Programm -> [Instruction]
compile m = DList.toList . execWriter . flip evalStateT emptyState . flip runReaderT "" . runCompileMonad . compileProgram
  where
    emptyState = CompileS mempty 0 mempty 0 m
