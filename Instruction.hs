
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


import Jass.Ast
import Data.Composeable

import Control.Arrow (second)

import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

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


newtype CompileMonad = Identity

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
