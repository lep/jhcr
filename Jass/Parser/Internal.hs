{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Jass.Parser.Internal where

import Text.Megaparsec

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List.NonEmpty ( NonEmpty(..) )

import qualified Jass.Tokenizer as Tok

newtype TokenStream = TokenStream [TokenPos]
    deriving (Show)

data TokenPos = TokenPos Tok.Token Tok.AlexPosn
    deriving (Show, Eq, Ord)

instance Stream TokenStream where
    type Token TokenStream = TokenPos

    uncons (TokenStream []) = Nothing
    uncons (TokenStream (x:xs)) = Just (x, TokenStream xs)

    updatePos proxy twidth pos tok = (tokenToSourcePos tok, tokenToSourcePos tok)


alexPosnToSourcePos (Tok.AlexPn _ l c) = SourcePos "" (unsafePos$fromIntegral l) (unsafePos$fromIntegral c)

tokenToSourcePos (TokenPos _ pos) = alexPosnToSourcePos pos


tok ::Tok.Token -> Parsec Dec TokenStream TokenPos
tok t = token p Nothing
  where
    p (TokenPos t' _) | t' == t = Right $ TokenPos t undefined
    p t' = Left (Set.singleton (Tokens (t':|[])), Set.empty, Set.empty)
