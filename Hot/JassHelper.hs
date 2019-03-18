{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Hot.JassHelper (compile) where

import Control.Arrow
import Control.Applicative
import Control.Monad

import Control.Monad.Trans.Except
import Control.Monad.IO.Class


import Jass.Parser
import Jass.Ast
import Jass.Printer

import Data.Composeable

import Data.List

import System.IO
import System.Environment
import System.Exit

import Data.ByteString.Builder

import Text.Megaparsec (errorBundlePretty, parse, ParseErrorBundle)

cleanName x =
  let x' = concatMap (\case '_':'_':_ -> "__"; x -> x) $ group x
  in if "jasshelper__init" `isPrefixOf` x
  then "jasshelper__init0000"
  else x'


compile :: Ast Name x -> Ast Name x
compile x =
  case x of
    Code f -> Code $ cleanName f
    Call "ExecuteFunc" [String f] -> Call "ExecuteFunc" [String $ cleanName f]
    Call f args -> Call (cleanName f) $ map compile args
    Function c name args ret body ->
        Function c (cleanName name) args ret $ map compile body
   
    _ -> composeOp compile x