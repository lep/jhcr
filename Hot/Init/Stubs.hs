{-# LANGUAGE GADTs #-}

module Hot.Init.Stubs (compile) where

import Data.String
import Data.Maybe

import qualified Data.Map as Map

import Jass.Ast hiding (fmap, foldMap, traverse)

import Data.Composeable

import Hot.Var (Var, mkFn, mkLocal, getId', (##))
import qualified Hot.Var as H


compile :: Ast Var Programm -> Ast Var Programm
compile (Programm pr) = Programm $ concatMap stubifyFn pr

stubifyFn :: Ast Var Toplevel -> [Ast Var Toplevel]
stubifyFn e =
  case e of
    Function _ (H.Fn n _ _ _) _ _ _ | n `elem` donttouch -> [e]
    Function c n args retty body ->
        let binds :: [Ast Var Stmt]
            binds = zipWith (\(ty, var) idx ->
                                Call (mkFn $ "_Table_set_" <> ty)
                                      [ Var $ SVar scope
                                      , Int . fromString $ show idx
                                      , Var $ SVar var])
                                args
                                [1..]
            call :: Ast Var Stmt
            call = Call (mkFn "_Wrap_call_anything_around") [Int $ getId' n]
            
            ldef = if retty == "nothing"
                   then Nothing
                   else Just . Local $ SDef Normal (mkLocal "_ret") retty Nothing

            mkRetVar = if retty == "code"
                       then Call (mkFn "_Auto_i2code") [Call (mkFn $ "_Table_get_" <> retty) [Var $ SVar ltbl, Int "0"]]
                       else Call (mkFn $ "_Table_get_" <> retty) [Var $ SVar ltbl, Int "0"]

            set = if retty == "nothing"
                  then Nothing
                  else Just $ Set (SVar $ mkLocal "_ret") mkRetVar
              
            flush = Call (mkFn "_Table_destroy") [Var $ SVar ltbl]
            
            localTbl = Local (SDef Normal ltbl "integer" Nothing)
                  
            ret = if retty == "nothing"
                  then Return Nothing
                  else Return . Just . Var . SVar $ mkLocal "_ret"
                  
            sets = [ Set (SVar ltbl) (Call (mkFn "_Table_alloc") [])
                   , Set (SVar scope) (Var $ SVar ltbl) ]

            body' =  sets <> binds <> [call] <> maybeToList set <> [flush, ret]


        in [ Function c ("_" ## n) args retty  $ map (rename n) body
        , Function c n args retty $ maybeToList ldef <> [
            localTbl,
            If (Call (mkFn "_Modified_modified") [Int $ getId' n])
                  body'
                [] (Just [
                  if retty == "nothing"
                  then Call ("_" ## n) $ map (Var . SVar . snd) args
                  else Return . Just $ Call ("_" ## n) $ map (Var . SVar . snd) args
                ])
          ]
        ]
    _ -> [e]
  where
    donttouch =
      [ "main", "config", "InitCustomPlayerSlots", "SetPlayerSlotAvailable"
      , "InitGenericPlayerSlots", "InitCustomTeams", "InitCustomTriggers"
      , "CreateAllUnits", "InitBlizzard", "InitGlobals"
      ]
      
    scope = mkLocal "_Wrap_args"
    
    ltbl = mkLocal "_tbl"
    
    rename :: Var -> Ast Var x -> Ast Var x
    rename v x =
      case x of
        Call n args -> Call (r n) $ map (rename v) args
        Code n -> Code $ r n
        _ -> composeOp (rename v) x
      where
        r n = if n == v then "_" ## n else n