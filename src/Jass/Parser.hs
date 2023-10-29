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

    ) where

import Control.Applicative hiding (many, some)
import Control.Monad

import Data.Maybe
import Data.Void

import Jass.Ast

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr
import Data.Functor (($>))

type Parser = Parsec Void String

sc = L.space sc' lc empty
  where
    sc' =void $ takeWhile1P (Just "white space") (\w -> w == ' ' || w == '\t')
    lc = L.skipLineComment "//"


lexeme = L.lexeme sc

stringlit = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

rawcode :: Parser String
rawcode = lexeme $ char '\'' *> (escaped <|> anyCC)
  where
    escaped = do
       char '\\'
       c <- oneOf ("btrnf" :: String)
       char '\''
       return ['\\', c]
    anyCC = manyTill anySingle (char '\'')

intlit :: Parser String
intlit = try $ do
    n <- option "" $ string "-"
    int <- show <$> lexeme (try hexlit <|> try octlit <|> L.decimal)
    return $ n <> int

hexlit = char '$' *> L.hexadecimal
     <|> char '0' *> (char 'X' <|> char 'x') *> L.hexadecimal
octlit = char '0' *> L.octal


reallit = try $ do
    n <- option "" $ string "-"
    r <- lexeme $ dotReal <|> realDot
    return $ n <> r
dotReal = do
    char '.'
    a <- some digitChar
    return $ "0." <> a
realDot = do
    a <- some digitChar
    char '.'
    b <- many digitChar
    return $ a <> "." <> b


symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` keywords
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x
    keywords = [ "globals", "endglobals", "if", "then", "elseif", "else"
               , "endif", "loop", "endloop", "set", "call", "return"
               , "takes", "returns", "constant", "native", "function"
               , "nothing", "true", "false", "null", "and", "or", "not"
               , "local", "array"
               ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")


horizontalSpace = void $ some $ lexeme $ optional (L.skipLineComment "//") *> eol

toplevel = globals
        <|> typedef
        <|> functionLike

  where
    functionLike = do
        const <- fromMaybe Normal <$> optional (reserved "constant" $> Jass.Ast.Const)
        native const <|> function const

    globals = between (reserved "globals"<* horizontalSpace)
                      (reserved "endglobals" ) $
                        pGlobal `sepEndBy` horizontalSpace
    pGlobal = do
        const <- fromMaybe Normal <$> optional (reserved "constant" $> Jass.Ast.Const)
        vdecl <- vardecl const
        return $ Global vdecl

    typedef = do
        reserved "type"
        new <- identifier
        reserved "extends"
        base <- identifier
        return [Typedef new base]

pSignature = do
    name <- identifier
    reserved "takes"
    args <- (reserved "nothing" $> []) <|> ((,) <$> identifier <*> identifier) `sepBy` symbol ","
    reserved "returns"
    ret <- (reserved "nothing" $> "nothing") <|> identifier
    return (name, args, ret)

native const = do
    reserved "native"
    (name, args, ret) <- pSignature
    return [Native const name args ret]

function const = do
    reserved "function"
    (name, args, ret) <- pSignature
    horizontalSpace
    body <- statement `sepEndBy` horizontalSpace
    reserved "endfunction"
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
        local = Local <$> (reserved "local"*> vardecl Normal)
        returnStmt = Return <$> (reserved "return" *> optional expression)
        callStmt = Call <$> (optional (reserved "debug") *> reserved "call" *> identifier) <*> parens arglist
        loop = Loop <$> between startLoop endLoop (statement `sepEndBy` horizontalSpace)

        set = Set <$> (reserved "set" *> lvar)
                  <*> (symbol "=" *> expression)
        lvar = do
            v <- identifier
            arr <- optional $ brackets expression
            case arr of
                Just idx -> return $ AVar v idx
                Nothing -> return $ SVar v


        exitwhen = Exitwhen <$> (reserved "exitwhen" *> expression)

        if_ = If <$> (reserved "if" *> expression <* reserved "then" <* horizontalSpace)
                 <*> statement `sepEndBy` horizontalSpace
                 <*> many elseif
                 <*> optional else_
                 <*  reserved "endif"


        elseif =
            (,) <$> (reserved "elseif" *> expression)
                <*> (reserved "then" >> horizontalSpace *> statement `sepEndBy` horizontalSpace)


        else_ = reserved "else" *> horizontalSpace *> statement `sepEndBy` horizontalSpace

        startLoop = reserved "loop" <* horizontalSpace
        endLoop = reserved "endloop"

vardecl constantness = do
    typ <- identifier
    isArray <- (reserved "array" $> True) <|> pure False
    if isArray
    then varArray typ
    else varNormal typ

  where
    varArray typ = ADef <$> identifier <*> pure typ
    varNormal typ = SDef constantness <$> identifier <*> pure typ <*> optional (symbol "=" *> expression)

expression = makeExprParser term table
            <?> "expression"
  where
    table = [ [ prefix (reserved "not") "not" ],
              [ binary (symbol "%") "%", binary (symbol "*") "*", binary (symbol "/") "/"]
            , [ binary (symbol "+") "+", binary (symbol "-") "-"]
            , zipWith binary
                [symbol "<=", symbol "<" , symbol ">=", symbol ">", symbol "!=" , symbol "==" ]
                ["<="       , "<"        , ">="   , ">"      , "!="   , "==" ]
            , [ binary (reserved "or") "or"]
            , [ binary (reserved "and") "and"]
            ]
binary t op = InfixL (t $> (\a b -> Call op [a, b]))
prefix t op = Prefix (t $> (\e -> Call op [e]))


term = parens expression
    <|> literal
    <|> varOrCall
    <|> symbol "+"     *> ((\e -> Call "+" [e]) <$> term)
    <|> symbol "-"     *> ((\e -> Call "-" [e]) <$> term)
    -- makeExprParser doesn't support multiple prefix operations
    -- as per documentation so we have to handle it in both term and
    -- expression. Looks like it gives the correct result...
    <|> reserved "not" *> ((\e -> Call "not" [e]) <$> expression)
    <?> "term"
  where
    literal = String <$> stringlit
            <|> either Real Int <$> eitherP (try reallit) intlit
            <|> Rawcode <$> rawcode
            <|> (reserved "true" $> Bool True)
            <|> (reserved "false" $> Bool False)
            <|> (reserved "null" $> Null)
            <|> Code <$> (reserved "function" *> identifier)

    varOrCall = do
        name <- identifier
        choice [ c name, a name, v name ]

    c name = Call name <$> parens arglist
    a name = Var . AVar name <$> brackets expression
    v name = return . Var $ SVar name

arglist = expression `sepBy` symbol ","

programm :: Parser (Ast Name Programm)
programm = do
    space
    many horizontalSpace
    ts <- concat <$> toplevel `sepEndBy` horizontalSpace
    eof
    pure $ Programm ts
