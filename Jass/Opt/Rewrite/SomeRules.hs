module Jass.Opt.Rewrite.SomeRules (someRules) where

import qualified Jass.Opt.Rewrite as Jass.Opt
import qualified Jass.Parser as J

import qualified Data.Set as Set
import qualified Text.Megaparsec as Mega

import Data.Maybe (fromJust)

-- it might be that not every rule brings a huge advantage in the generated
-- assembler code, but no rule should hurt the generated code.
someRules =
  [ mkRR ["a"] "-(-a)" "a"
  , mkRR ["a"] "a==true" "a"
  , mkRR ["a"] "true==a" "a"
  , mkRR ["a", "b"] "not (a<=b)" "a>b"
  , mkRR ["a", "b"] "not (a<b)" "a>=b"
  , mkRR ["a", "b"] "not (a>=b)" "a<b"
  , mkRR ["a", "b"] "not (a>b)" "a<=b"
  , mkRR ["a"] "false==a" "not a"
  , mkRR ["a"] "a==false" "not a"
  , mkRR ["a"] "false!=a" "a"
  , mkRR ["a"] "a!=false" "a"
  , mkRR ["a"] "a!=true" "not a"
  , mkRR ["a"] "true!=a" "not a"
  , mkRR ["a"] "not (not a)" "a"
  , mkRR ["a", "b"] "(not a) or (not b)" "not (a and b)"
  , mkRR ["a", "b"] "(not a) and (not b)" "not (a or b)"
  , mkRR ["a"] "false and a" "false"
  , mkRR ["a"] "true or a" "true"
  , mkRR ["a"] "true and a" "a"
  , mkRR ["a"] "a and true" "a"
  , mkRR ["a"] "false or a" "a"
  , mkRR ["a"] "a or false" "a"
  , mkRR ["a", "b"] "IsUnit(a,b)" "a == b"
  , mkRR ["u", "l"] "SetUnitState(u, UNIT_STATE_LIFE, l)" "SetWidgetLife(u, l)" -- gg, bind vs conv
  , mkRR ["u"] "GetUnitState(u, UNIT_STATE_LIFE)" "GetWidgetLife(u)" -- gg, bind vs conv
  , mkRR ["u", "p"] "GetOwningPlayer(u) == p" "IsUnitOwnedByPlayer(u, p)"
  , mkRR ["p"] "GetPlayerId(Player(p))" "p"
  
  -- this one seems rather nice: bind, call vs conv, lit, add but
  -- the asm rewrite engine can reduce it to just conv
  , mkRR ["i"] "I2R(i)" "(i+0.0)"
  
  , mkRR ["a", "b"] "a + (-b)" "a-b"
  
  -- note the lacking decimal point, as otherwise we could lose promotion
  -- from int to real but we use the knowledge that our parser parses these as
  -- integer literals. these rules are jass safe.
  -- look below for normally non-safe rewrite rules.
  , mkRR ["a"] "-1*a" "-a"
  , mkRR ["a"] "a*-1" "-a"
  , mkRR ["a"] "a+0" "a"
  , mkRR ["a"] "0+a" "a"
  , mkRR ["a"] "1*a" "a"
  , mkRR ["a"] "a*1" "a"
  
  -- 1.0*a can be used in jass to promote an integer to a real, so normally
  -- this would not be a valid rewrite rule, but the instruction compiler
  -- inserts the correct conv instructions which allows these rules
  , mkRR ["a"] "-1.0*a" "-a"
  , mkRR ["a"] "a*-1.0" "-a"
  , mkRR ["a"] "a+0.0" "a"
  , mkRR ["a"] "0.0+a" "a"
  , mkRR ["a"] "1.0*a" "a"
  , mkRR ["a"] "a*1.0" "a"
  
  , mkRR ["a"] "a/1" "a"
  , mkRR ["a"] "a/1.0" "a"
  
  , mkRR ["a"] "a/-1" "-a"
  , mkRR ["a"] "a/-1.0" "-a"
  ]
  where
    mkRR fv from to =
      Jass.Opt.Rule
        (Set.fromList fv)
        (fromJust $ Mega.parseMaybe (J.expression <* Mega.eof) from)
        (fromJust $ Mega.parseMaybe (J.expression <* Mega.eof) to)