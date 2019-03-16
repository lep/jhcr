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

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text

import Data.List.NonEmpty ( NonEmpty(..) )

import Data.Void


import Jass.Ast

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

sc = L.space sc' lc empty
  where
    sc' =void $ takeWhile1P (Just "white space") (\w -> w == ' ' || w == '\t')
    lc = L.skipLineComment "//"


lexeme = L.lexeme sc

--reallit = lexeme $ some digitChar 
--stringlit = lexeme $ char '"' >* some digitChar 
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
intlit = show <$> (lexeme (try hexlit <|> try octlit <|> L.decimal))

hexlit = char '0' *> (char 'X' <|> char 'x') *> L.hexadecimal
octlit = char '0' *> L.octal


reallit = lexeme $ dotReal <|> realDot
dotReal = do
    char '.'
    a <- some digitChar
    return $ "0" <> a
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


horizontalSpace = void $ some $ lexeme $ optional (L.skipLineComment "//") *> newline

toplevel = globals
        <|> typedef
        <|> functionLike

  where
    functionLike = do
        const <- fromMaybe Normal <$> optional (reserved "constant"*> pure Jass.Ast.Const)
        native const <|> function const

    globals = between (reserved "globals"<* horizontalSpace)
                      (reserved "endglobals" <* horizontalSpace) $ many $ do
        const <- fromMaybe Normal <$> optional (reserved "constant" *> pure Jass.Ast.Const)
        vdecl <- vardecl const
        return $ Global vdecl

    typedef = do
    reserved "type"
    new <- identifier
    reserved "extends"
    base <- identifier
    horizontalSpace
    return [Typedef new base]


native const = do
    reserved "native"
    name <- identifier
    reserved "takes"
    args <- (reserved "nothing" *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (symbol ",")
    reserved "returns"
    ret <- (reserved "nothing" *> pure "nothing") <|> identifier
    horizontalSpace
    return [Native const name args ret]

function const = do
    reserved "function"
    name <- identifier
    reserved "takes"
    args <- (reserved "nothing" *> pure []) <|> ((,) <$> identifier <*> identifier) `sepBy` (symbol ",")
    reserved "returns"
    ret <- (reserved "nothing" *> pure "nothing") <|> identifier
    horizontalSpace
    body <- many statement
    reserved "endfunction"
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
        local = Local <$> (reserved "local"*> vardecl Normal)
        returnStmt = Return <$> (reserved "return" *> optional expression <* horizontalSpace)
        callStmt = Call <$> (reserved "call" *> identifier) <*> parens arglist <* horizontalSpace
        loop = Loop <$> between startLoop endLoop (many statement)

        set = Set <$> (reserved "set" *> lvar)
                  <*> (symbol "=" *> expression)
                  <* horizontalSpace
        lvar = do
            v <- identifier
            arr <- optional $ brackets expression
            case arr of
                Just idx -> return $ AVar v idx
                Nothing -> return $ SVar v

            
        exitwhen = Exitwhen <$> (reserved "exitwhen" *> expression <* horizontalSpace)

        if_ = If <$> (reserved "if" *> expression <* reserved "then" <* horizontalSpace)
                 <*> (many statement)
                 <*> many elseif
                 <*> optional else_
                 <*  reserved "endif" <* horizontalSpace


        elseif =
            (,) <$> (reserved "elseif" *> expression)
                <*> (reserved "then" >> horizontalSpace *> many statement)


        else_ = reserved "else" *> horizontalSpace *> many statement

        startLoop = reserved "loop" <* horizontalSpace
        endLoop = reserved "endloop" <* horizontalSpace

vardecl constantness = do
    typ <- identifier
    isArray <- reserved "array" *> pure True <|> pure False
    if isArray
    then varArray typ <* horizontalSpace
    else varNormal typ <* horizontalSpace

  where
    varArray typ = ADef <$> identifier <*> pure typ
    varNormal typ = SDef constantness <$> identifier <*> pure typ <*> optional (symbol "=" *> expression)

expression = makeExprParser term table
            <?> "expression"
  where
    table = [ [ binary (symbol "%") "%", binary (symbol "*") "*", binary (symbol "/") "/"]
            , [ binary (symbol "+") "+", binary (symbol "-") "-"]
            , zipWith binary
                [symbol "<=", symbol "<" , symbol ">=", symbol ">", symbol "!=" , symbol "==" ]
                ["<="       , "<"        , ">="   , ">"      , "!="   , "==" ]
            , [ binary (reserved "or") "or"]
            , [ binary (reserved "and") "and"]
            ]
    binary t op = InfixL (t *> pure (\a b -> Call op [a, b]))

term = parens expression
    <|> reserved "not"   *> (((\e -> Call "not" [e])) <$> expression)
    <|> symbol "-" *> (((\e -> Call "-" [e])) <$> expression)
    <|> symbol "+"  *> (((\e -> Call "+" [e])) <$> expression)
    <|> literal
    <|> varOrCall
    <?> "term"
  where
    literal = String <$> stringlit
            <|> either Real Int <$> eitherP (try reallit) intlit
            <|> Rawcode <$> rawcode
            <|> (reserved "true" *> pure ( Bool True))
            <|> (reserved "false" *> pure ( Bool False))
            <|> (reserved "null" *> pure Null)
            <|> Code <$> (reserved "function" *> identifier)

    varOrCall = do
        name <- identifier
        choice [ c name, a name, v name ]

    c name = Call name <$> parens arglist
    a name = Var . AVar name <$> brackets expression
    v name = return . Var $ SVar name

arglist = expression `sepBy` symbol ","

programm = Programm . concat <$> (many horizontalSpace *> many toplevel <* eof)


--parse :: Parsec e TokenStream a -> BL.ByteString -> Either (ParseError TokenPos e) a
--parse parser = Text.Megaparsec.parse parser ""
--                . TokenStream . map (uncurry TokenPos) . Tok.alexScanTokens
