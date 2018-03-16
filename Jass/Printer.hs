{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Jass.Printer (pretty) where

import Prelude hiding (unwords, unlines)

import Data.ByteString.Builder
import Data.Monoid
import Data.List (intersperse, partition)

import Jass.Ast

pretty :: Ast Name Programm -> Builder
pretty = printProgram

isGlobal :: Ast Name Toplevel -> Bool
isGlobal (Global _) = True
isGlobal _ = False

printProgram :: Ast Name Programm -> Builder
printProgram (Programm toplevel) =
    let (globals, fns) = partition isGlobal toplevel
    in unlines [printGlobals globals, printFns fns]

printGlobals :: [Ast Name Toplevel] -> Builder
printGlobals globals = unlines
    [ "globals"
    , unlines $ map printGlobal globals
    , "endglobals"
    ]

printGlobal :: Ast Name Toplevel -> Builder
printGlobal (Global vdef) = printVDef vdef

printFns :: [Ast Name Toplevel] -> Builder
printFns fns = unlines $ map printFn fns

printFn :: Ast Name Toplevel -> Builder
printFn (Function c name args retty body) = unlines
    [ unwords [ printConst c, "function", lazyByteString name, "takes", printArgs args, "returns", lazyByteString retty ]
    , block body
    , "endfunction"
    ]

printArgs [] = "nothing"
printArgs args = unlist $ map (\(ty, name) -> unwords [lazyByteString ty, lazyByteString name]) args


unwords = mconcat . intersperse (charUtf8 ' ')
unlines = mconcat . intersperse (charUtf8 '\n')
unlist = mconcat . intersperse (charUtf8 ',')

between a b c = a <> c <> b

parens = between (charUtf8 '(') (charUtf8 ')')
brackets = between (charUtf8 '[') (charUtf8 ']')

block b = unlines $ map printStmt b

printStmt :: Ast Name Stmt -> Builder
printStmt a =
  case a of
    Set lvar expr -> unwords ["set", printLVar lvar, "=", printExpr expr]
    Local vdef -> unwords ["local", printVDef vdef]
    Exitwhen cond -> unwords ["exitwhen", printExpr cond]
    Return e -> unwords ["return", maybe mempty printExpr e ]
    Call n args -> unwords ["call", lazyByteString n, parens $ unlist $ map printExpr args]
    Loop b -> unlines
        [ "loop" 
        , block b
        , "endloop"
        ]
    If cond thenBranch elseifs elseBranch -> unlines 
        [ unwords ["if", printExpr cond, "then" ]
        , unlines $ map printStmt thenBranch
        , printElseifs elseifs
        , printElse elseBranch
        , "endif"
        ]

  where
    printElseifs eifs = unlines $ map printElseIf eifs
    printElseIf (cond, b) = unlines
        [ unwords ["elseif", printExpr cond, "then" ]
        , block b
        ]
    printElse Nothing = mempty
    printElse (Just b) = unlines
        [ "else"
        , block b
        ]

isOp x = x `elem` ["and", "or", "not"
                  , "+", "-", "*", "/", "%"
                  , "==", "!=", "<=", ">=", "<", ">"
                  ]

printExpr :: Ast Name Expr -> Builder
printExpr e =
  case e of
    Call n args
        | isOp n -> printOp n args
        | otherwise -> unwords [lazyByteString n, parens $ unlist $ map printExpr args]
    Null -> "null"
    Code n -> unwords ["function", lazyByteString n]
    Bool False -> "false"
    Bool True -> "true"
    Rawcode r -> charUtf8 '\'' <> lazyByteString r <> charUtf8 '\''
    Int i -> lazyByteString i
    Real r -> lazyByteString r
    String s -> "\"" <> lazyByteString s <> "\""
    Var lvar -> printLVar lvar

printOp "not" [x] = parens $ unwords ["not", printExpr x]
printOp "-" [x] = unwords ["-", printExpr x]
printOp "+" [x] = printExpr x
printOp op [a, b] = parens $ unwords [printExpr a, lazyByteString op, printExpr b]

printLVar :: Ast Name LVar -> Builder
printLVar lv =
  case lv of
    AVar n idx -> unwords [lazyByteString n, brackets $ printExpr idx ]
    SVar n -> lazyByteString n

printVDef :: Ast Name VarDef -> Builder
printVDef vdef =
  case vdef of
    SDef c name ty init -> unwords [printConst c, lazyByteString ty, lazyByteString name, printInit init]
    ADef name ty -> unwords [lazyByteString ty, "array", lazyByteString name]

printConst Const = "constant"
printConst Normal = mempty


printInit :: Maybe (Ast Name Expr) -> Builder
printInit Nothing = mempty
printInit (Just e) = unwords ["=", printExpr e]