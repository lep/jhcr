{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Jass.Parser
    ( expression
    , statement
    , toplevel
    , programm

    , identifier
    , intlit
    , stringlit
    , reallit
    , rawcode
    
    , Jass.Parser.parse
    ) where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty ( NonEmpty(..) )


import Jass.Ast
import qualified Jass.Tokenizer as Tok
import Jass.Parser.Internal

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Pos


ident :: Parsec Dec TokenStream BL.ByteString
ident = token p Nothing
  where
    p (TokenPos (Tok.Id i) _) = Right i
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)


stringlit :: Parsec Dec TokenStream BL.ByteString
stringlit = token p Nothing
  where
    p (TokenPos (Tok.String s) _) = Right s
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)


intlit = token p Nothing
  where
    p (TokenPos (Tok.Intlit i) _) = Right i
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)


reallit = token p Nothing
  where
    p (TokenPos (Tok.Reallit r) _) = Right r
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)


rawcode = token p Nothing
  where
    p (TokenPos (Tok.Rawcode r) _) = Right r
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)




identifier = ident

parens = between (tok Tok.LParen) (tok Tok.RParen)
brackets = between (tok Tok.LBracket) (tok Tok.RBracket)

horizontalSpace = some $ tok Tok.Newline

toplevel = globals
        <|> typedef
        <|> functionLike

  where
    functionLike = do
        const <- fromMaybe Normal <$> optional (tok Tok.Constant *> pure Jass.Ast.Const)
        native const <|> function const

    globals = between (tok Tok.Globals <* horizontalSpace)
                      (tok Tok.Endglobals <* horizontalSpace) $ many $ do
        const <- fromMaybe Normal <$> optional (tok Tok.Constant *> pure Jass.Ast.Const)
        vdecl <- vardecl const
        return $ Global vdecl

    typedef = do
    tok Tok.Type
    new <- identifier
    tok Tok.Extends
    base <- identifier
    horizontalSpace
    return [Typedef new base]


native const = do
    tok Tok.Native
    name <- identifier
    tok Tok.Takes
    args <- (tok Tok.Nothin *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (tok Tok.Comma)
    tok Tok.Returns
    ret <- (tok Tok.Nothin *> pure "nothing") <|> identifier
    horizontalSpace
    return [Native const name args ret]

function const = do
    tok Tok.Function
    name <- identifier
    tok Tok.Takes
    args <- (tok Tok.Nothin *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (tok Tok.Comma)
    tok Tok.Returns
    ret <- (tok Tok.Nothin *> pure "nothing") <|> identifier
    horizontalSpace
    body <- many statement
    tok Tok.Endfunction
    horizontalSpace
    return [Function const name args ret body]

statement = returnStmt
          <|> if_
          <|> callStmt
          <|> loop
          <|> set
          <|> exitwhen
          <|> local
          <?> "statement"
    where
        local = Local <$> (tok Tok.Local *> vardecl Normal)
        returnStmt = Return <$> (tok Tok.Return *> optional expression <* horizontalSpace)
        callStmt = Call <$> (tok Tok.Call *> identifier) <*> parens arglist <* horizontalSpace
        loop = Loop <$> between startLoop endLoop (many statement)

        set = Set <$> (tok Tok.Set *> lvar)
                  <*> (tok Tok.Equal *> expression)
                  <* horizontalSpace
        lvar = do
            v <- identifier
            arr <- optional $ brackets expression
            case arr of
                Just idx -> return $ AVar v idx
                Nothing -> return $ SVar v

            
        exitwhen = Exitwhen <$> (tok Tok.Exitwhen *> expression <* horizontalSpace)

        if_ = If <$> (tok Tok.If *> expression <* tok Tok.Then <* horizontalSpace)
                 <*> (many statement)
                 <*> many elseif
                 <*> optional else_
                 <*  tok Tok.Endif <* horizontalSpace


        elseif =
            (,) <$> (tok Tok.Elseif *> expression)
                <*> (tok Tok.Then >> horizontalSpace *> many statement)


        else_ = tok Tok.Else *> horizontalSpace *> many statement

        startLoop = tok Tok.Loop <* horizontalSpace
        endLoop = tok Tok.Endloop <* horizontalSpace

vardecl constantness = do
    typ <- identifier
    isArray <- tok Tok.Array *> pure True <|> pure False
    if isArray
    then varArray typ <* horizontalSpace
    else varNormal typ <* horizontalSpace

  where
    varArray typ = ADef <$> identifier <*> pure typ
    varNormal typ = SDef constantness <$> identifier <*> pure typ <*> optional (tok Tok.Equal *> expression)

expression = makeExprParser term table
            <?> "expression"
  where
    table = [ [ binary Tok.Mult "*", binary Tok.Div "/"]
            , [ binary Tok.Plus "+", binary Tok.Minus "-"]
            , zipWith binary
                [Tok.LEQ, Tok.LTtok, Tok.GEQ, Tok.GTtok, Tok.NEQ, Tok.EQtok]
                ["<="   , "<"      , ">="   , ">"      , "!="   , "==" ]
            , [ binary Tok.Or "or"]
            , [ binary Tok.And "and"]
            ]
    binary t op = InfixL (tok t *> pure (\a b -> Call op [a, b]))

term = parens expression
    <|> tok Tok.Not   *> (((\e -> Call "not" [e])) <$> expression)
    <|> tok Tok.Minus *> (((\e -> Call "-" [e])) <$> expression)
    <|> tok Tok.Plus  *> (((\e -> Call "+" [e])) <$> expression)
    <|> literal
    <|> varOrCall
    <?> "term"
  where
    literal = String <$> stringlit
            <|> Int <$> intlit
            <|> Real <$> reallit
            <|> Rawcode <$> rawcode
            <|> (tok Tok.FALSE *> pure ( Bool False))
            <|> (tok Tok.TRUE *> pure ( Bool True))
            <|> (tok Tok.NULL *> pure Null)
            <|> Code <$> (tok Tok.Function *> identifier)

    varOrCall = do
        name <- identifier
        choice [ c name, a name, v name ]

    c name = Call name <$> parens arglist
    a name = Var . AVar name <$> brackets expression
    v name = return . Var $ SVar name

arglist = expression `sepBy` tok Tok.Comma

programm = Programm . concat <$> (many horizontalSpace *> many toplevel <* eof)


parse :: Parsec e TokenStream a -> BL.ByteString -> Either (ParseError TokenPos e) a
parse parser = Text.Megaparsec.parse parser ""
                . TokenStream . map (uncurry TokenPos) . Tok.alexScanTokens
