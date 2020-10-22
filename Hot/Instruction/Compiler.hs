{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
 
module Hot.Instruction.Compiler (compile, compileGlobals) where

import Data.DList (DList)
import qualified Data.DList as DList

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow (first)

import qualified Jass.LCA as Jass

import Hot.Ast hiding (Call, Set, Function)
import qualified Hot.Ast as H

import Hot.Var
import Hot.Instruction


newtype CompileMonad a = CompileMonad { runCompileMonad :: ReaderT (Type, Map Type Type) (StateT CompileState (Writer (DList Instruction))) a }
  deriving (Functor, Applicative, Monad, MonadWriter (DList Instruction), MonadState CompileState, MonadReader (Type, Map Type Type) )



name2op n = fromJust $ lookup n [ ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)
                                , ("==", Eq), ("!=", Neq), ("-", Sub)
                                , ("+", Add), ("*", Mul), ("/", Div), ("%", Mod)
                                ]


-- when compiling to bytecode we dont care about sequential ids

data CompileState = CompileS { _loopStack :: [(Label, Label)]
                             , _labelId :: Label
                             , _registerId :: Register
                             }
makeLenses ''CompileState

emptyState = CompileS mempty 0 0

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
newRegister = registerId <-= 1

isBooleanOp x = x `elem` (["==", "!=", "<", "<=", ">", ">=", "and", "or"] :: [Name])

code2int "code" = "integer"
code2int x      = x

typeOfExpr :: Ast Var Expr -> CompileMonad Type
typeOfExpr e =
  case e of
    H.Call (Op "-") [a] -> typeOfExpr a
    H.Call (Op "not") [_] -> pure "boolean"
    H.Call (Op o) [a, b]
        | isBooleanOp o -> pure "boolean"
        | otherwise -> numericType <$> (typeOfExpr a) <*> (typeOfExpr b)
    H.Call n _ -> pure $ typeOfVar n
    Var (SVar v) -> pure $ typeOfVar v
    Var (AVar v _) -> pure $ typeOfVar v

    -- we might not need this since all code references are now integers...
    Code{} -> pure "integer" 

    --Int{} -> "integer"
    Int{} -> do
        wanted <- asks fst
        if wanted == "real"
        then pure "real"
        else pure "integer"
        
    Real{} -> pure "real"
    Bool{} -> pure "boolean"
    String{} -> pure "string"
    Null -> pure "handle"

typeOfVar :: Var -> Type
typeOfVar v =
  case v of
    Local _ t _ _ -> t
    Global _ _ t _ _ -> t
    Fn _ _ t _ -> t
    _ -> ""
    

typeOfVar' :: Var -> Type
typeOfVar' = code2int . typeOfVar

lca :: Type -> Type -> CompileMonad Type
lca a b = do
    hierachy <- asks snd
    let t = fromMaybe (error $ unwords ["lca error for types", a, b]) $ Jass.lca hierachy a b
    return t

compileProgram :: Ast Var Programm -> CompileMonad ()
compileProgram (Programm toplevel) = mapM_ compileToplevel toplevel

compileToplevel :: Ast Var Toplevel -> CompileMonad ()
compileToplevel (H.Function n _ r body) = do
    let fname = nameOf n
    let fn = Function (getId n) fname
    emit fn

    labelId .= 1
    registerId .= 0

    typed r $ compileStmt body
    -- we removed double rets via rewrites
    emit $ Ret r

typed :: Type -> CompileMonad a -> CompileMonad a
typed t = local (first (const t))

typedGet :: Type -> Register -> CompileMonad Register
typedGet sourcetype source = do
    wanted <- asks fst
    if wanted /= sourcetype &&
       wanted /= "nothing" &&
       not (wanted == "code" && sourcetype == "integer")
    then do
        r <- newRegister
        emit $ Convert wanted r sourcetype source
        return r
    else
        return source


compileCall :: Ast Var a -> CompileMonad Register
compileCall (H.Call n@(Fn _ aTypes _ _) args) = do
    r <- newRegister
    let vname = nameOf n
    let v = getId n
    binds <- forM (zip3 args aTypes [1, 2..]) $ \(arg, typ, pos) -> typed typ $ do
        r <- compileExpr arg
        return $ Bind (code2int typ) pos r
    mapM_ emit binds
    emit $ Call r v vname
    typedGet (typeOfVar n) r

compileStmt :: Ast Var Stmt -> CompileMonad ()
compileStmt e = do
  --registerId .= 0
  case e of
    Return Nothing -> emit . Ret =<< asks fst
    Return (Just e) -> do
        r <- compileExpr e
        wanted <- asks fst
        emit $ Set wanted 0 r
        emit $ Ret wanted

    H.Call{} -> void . typed "nothing" $ compileCall e


    If cond tb eb -> do
        trueLabel <- newLabel
        joinLabel <- newLabel
        r <- compileExpr cond
        emit $ JmpT trueLabel r
        compileStmt eb
        emit $ Jmp joinLabel
        emit $ Label trueLabel
        compileStmt tb
        emit $ Label joinLabel


    Loop body -> do
        loopEntry <- newLabel
        loopExit <- newLabel
        push (loopEntry, loopExit)
        
        emit $ Label loopEntry
        compileStmt body
        emit $ Jmp loopEntry
        emit $ Label loopExit
        pop

    Exitwhen cond -> do
        (_, loopExit) <- peek
        r <- compileExpr cond
        emit $ JmpT loopExit r

    H.Set (SVar v@Local{}) e -> do
        let t = typeOfVar' v
        r <- typed t $ compileExpr e
        emit $ Set t (getId v) r

    H.Set (SVar v@Global{}) e -> do
        let t = typeOfVar' v
        r <- typed t $ compileExpr e
        emit $ SetGlobal t (getId v) r

    H.Set (AVar v@Local{} idx) e -> do
        let t = typeOfVar' v
        idx' <- typed "integer" $ compileExpr idx
        r <- typed t $ compileExpr e
        emit $ SetLocalArray t (getId v) idx' r

    H.Set (AVar v@Global{} idx) e -> do
        let t = typeOfVar' v
        idx' <- typed "integer" $ compileExpr idx
        r <- typed t $ compileExpr e
        emit $ SetGlobalArray t (getId v) idx' r

    Block blk -> mapM_ compileStmt blk

numericType "real" _ = "real"
numericType _ "real" = "real"
numericType a _ = a

compType "string" _ = pure "string"
compType _ "string" = pure "string"
compType "code" _ = pure "code"
compType _ "code" = pure "code"
compType a b = lca a b

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f x y = liftM2 (,) x y >>= uncurry f

compileExpr :: Ast Var Expr -> CompileMonad Register
compileExpr e =
  case e of
    H.Call (Op "not") [a] -> do
        r <- newRegister
        t <- compileExpr a
        emit $ Not r t
        return r
        
    H.Call (Op "-") [a] -> do
        r <- newRegister
        t <- compileExpr a
        ta <- typeOfExpr a
        emit $ Negate ta r t
        typedGet ta r

    H.Call (Op "or") [a, b] -> do
        r <- newRegister
        cont <- newLabel
        a' <- typed "boolean" $ compileExpr a
        emit $ Set "boolean" r a'
        emit $ JmpT cont r
        b' <- typed "boolean" $ compileExpr b
        emit $ Set "boolean" r b'
        emit $ Label cont
        return r

    H.Call (Op "and") [a, b] -> do
        r <- newRegister
        t <- newRegister
        cont <- newLabel
        a' <- typed "boolean" $ compileExpr a
        emit $ Set "boolean" r a'
        emit $ Not t r
        emit $ JmpT cont t
        b' <- typed "boolean" $ compileExpr b
        emit $ Set "boolean" r b'
        emit $ Label cont
        return r
    
    H.Call (Op n) [a, b] | n `elem` ["<", "<=", ">", ">="] -> do
        let op = name2op n
        t <- numericType <$> (typeOfExpr a) <*> (typeOfExpr b)
        r <- newRegister
        a' <- typed t $ compileExpr a
        b' <- typed t $ compileExpr b
        emit $ op t r a' b'
        return r
    
    H.Call (Op n) [a, b] | n `elem` ["==", "!="] -> do
        let op = name2op n
        t <- code2int <$> bind2 compType (typeOfExpr a) (typeOfExpr b)
        r <- newRegister
        a' <- typed t $ compileExpr a
        b' <- typed t $ compileExpr b
        emit $ op t r a' b'
        return r

    H.Call (Op n) [a, b] | n `elem` ["+", "-", "*", "/", "%"] -> do
        let op = name2op n
        t <- numericType <$> (typeOfExpr a) <*> (typeOfExpr b)
        r <- newRegister
        a' <- typed t $ compileExpr a
        b' <- typed t $ compileExpr b
        emit $ op t r a' b' 
        typedGet t r

    
    H.Call{} -> compileCall e

    Var (SVar l@Local{}) -> typedGet (typeOfVar' l) (getId l)
    Var (SVar g@Global{}) -> do
        r <- newRegister
        emit $ GetGlobal (typeOfVar' g) r (getId g)
        typedGet (typeOfVar' g) r

    Var (AVar l@Local{} idx) -> do
        let t = typeOfVar' l

        r <- newRegister
        idx' <- typed "integer" $ compileExpr idx

        emit $ GetLocalArray t r (getId l) idx'
        typedGet t r

    Var (AVar g@Global{} idx) -> do
        let t = typeOfVar' g
        
        r <- newRegister
        idx' <- typed "integer" $ compileExpr idx
        emit $ GetGlobalArray t r (getId g) idx'
        typedGet t r


    Int val -> do
        r <- newRegister
        t <- asks fst
        let e' = if t == "real" then Real $ fromIntegral val else e
        emit $ Literal t r e'
        return r

    Real _ -> do
        r <- newRegister
        emit $ Literal "real" r e
        return r

    Bool _ -> do
        r <- newRegister
        emit $ Literal "boolean" r e
        return r

    String _ -> do
        r <- newRegister
        emit $ Literal "string" r e
        return r

    Null -> do
        r <- newRegister
        t <- code2int <$> asks fst
        if t == "integer"
        then emit $ Literal "integer" r $ Int 0
        else emit $ Literal t r e
        return r

    Code v -> do
        r <- newRegister
        emit . Literal "integer" r . Int $ getId v
        return r

compile :: Map Type Type -> Ast Var Programm -> [Instruction]
compile m =
    DList.toList .
    execWriter .
    flip evalStateT emptyState .
    flip runReaderT ("", m) .
    runCompileMonad .
    compileProgram


compileGlobals :: Map Type Type -> [Ast Var Stmt] -> [Instruction]
compileGlobals m =
    DList.toList .
    execWriter .
    flip evalStateT emptyState .
    flip runReaderT ("", m) .
    runCompileMonad .
    mapM_ compileStmt