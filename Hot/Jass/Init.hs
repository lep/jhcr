{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}


module Hot.Jass.Init where

import Control.Lens hiding (Const)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader


import Data.Int
import Data.Monoid
import Data.List
import Data.String
import Data.Function
import Data.Maybe

import GHC.Generics
import Data.Binary

import Data.DList (DList)
import qualified Data.DList as DList

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


import Jass.Ast hiding (fmap, foldMap, traverse)

import Data.Composeable

import Hot.Var (Var, mkFn, mkGlobal, mkLocal, mkOp, getId, getId', nameOf, (##))
import qualified Hot.Var as H
import qualified Hot.Types as Hot

import Unsafe.Coerce
import Debug.Trace


init_name2ids :: Ast Var Programm -> Ast Var Programm
init_name2ids x =
    Programm
        [ Function Normal (mkFn "_Auto_name_tables_init") [] "nothing" $
            DList.toList $ go x
        ]
  where
    go :: Ast Var x -> DList (Ast Var Stmt)
    go x =
      case x of
        Global (SDef _ v _ _) -> DList.singleton $ addInit v
        Function _ v _ _ _ -> DList.singleton $ addInit v
        Native _ v _ _ -> DList.singleton $ addInit v
        _ -> composeFold go x
    addInit v =
      case v of
        -- call _Names_insert_global(ty, name, uid)
        H.Global _ name ty isarray uid ->
            Call (mkFn "_Names_insert_global")
                 [ Int . fromString . show $ (Map.findWithDefault (error "Unknown type") ty Hot.types)
                 , String name
                 , Int . fromString $ show uid
                 ]

        -- call _Names_insert_function(name, uid)
        H.Fn name _ _ uid ->
            Call (mkFn "_Names_insert_function")
                 [ String name
                 , Int . fromString $ show uid
                 ]


stubify :: Ast Var Programm -> Ast Var Programm
stubify (Programm pr) = Programm $ concatMap stubifyFn pr

stubifyFn :: Ast Var Toplevel -> [Ast Var Toplevel]
stubifyFn e =
  case e of
    Function _ (H.Fn n _ _ _) _ _ _ | n `elem` donttouch -> [e]
    --Function _ (H.Fn "config" _ _ _) _ _ _ -> [e]
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
                       then Call (mkFn "_Auto_i2code") [Call (mkFn $ "_Table_get_" <> retty) [Var $ SVar scope, Int "0"]]
                       else Call (mkFn $ "_Table_get_" <> retty) [Var $ SVar scope, Int "0"]

            set = if retty == "nothing"
                  then Nothing
                  else Just $ Set (SVar $ mkLocal "_ret") mkRetVar
              
            flush = Call (mkFn "_Table_flush") [Var $ SVar scope]
                  
            ret = if retty == "nothing"
                  then Return Nothing
                  else Return . Just . Var . SVar $ mkLocal "_ret"

            body' =  binds <> [call] <> maybeToList set <> [flush, ret]


        in [ Function c ("_" ## n) args retty  $ map (rename n) body
        , Function c n args retty $ maybeToList ldef <> [
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
    {-
    call CreateAllUnits ()
call InitBlizzard ()
call InitGlobals ()
call InitCustomTriggers ()
-}
      [ "main", "config", "InitCustomPlayerSlots", "SetPlayerSlotAvailable"
      , "InitGenericPlayerSlots", "InitCustomTeams", "InitCustomTriggers"
      , "CreateAllUnits", "InitBlizzard", "InitGlobals"
      ]
    bind = mkLocal "_Scopes_binding"
    scope = mkLocal "_Wrap_args"
    
    rename :: Var -> Ast Var x -> Ast Var x
    rename v x =
      case x of
        Call n args -> Call (r n) $ map (rename v) args
        Code n -> Code $ r n
        _ -> composeOp (rename v) x
      where
        r n = if n == v then ("_" ## n) else n
    

data Signatur = FunctionSig [Type] Type
              | GlobalDef Type Bool
  deriving (Eq, Ord, Show)


hasSignatur :: Ast a Toplevel -> Bool
hasSignatur Function{} = True
hasSignatur Native{} = True
hasSignatur Global{} = True
hasSignatur _ = False

signatur :: Ast a Toplevel -> Signatur
signatur e =
  case e of
    Native _ _ args ret -> FunctionSig (map fst args) ret
    Function _ _ args ret _ -> FunctionSig (map fst args) ret
    Global (ADef _ t) -> GlobalDef t True
    Global (SDef _ _ t _) -> GlobalDef t False

isVar :: Ast a Toplevel -> Bool
isVar Global{} = True
isVar _ = False

isArray :: Ast a Toplevel -> Bool
isArray (Global ADef{}) = True
isArray _ = False
    


split :: Ast a Programm -> ([[Ast a Toplevel]], [[Ast a Toplevel]], [[Ast a Toplevel]])
split (Programm p) = 
  let sorted = {-sortOn signatur $-} filter hasSignatur p
      (vars, functions) = partition isVar sorted
      (arrays, normals) = partition isArray vars
    in (g normals, g arrays, [functions])
  where
    g = groupBy ((==) `on` signatur) . sortOn signatur


{-
removeCodeVars :: Ast Var x -> Ast Var x
removeCodeVars x =
  case x of
    SDef c v "code" init -> SDef c v "integer" $ fmap removeCodeVars init
    Code c -> Int $ getId' c
    SVar (Local name ty isarray uid)
      | ty == "code" = SVar $ Local name "integer" isarray uid
    SVar (Global c name ty isarray uid)
      | ty == "code" = SVar $ Global c name "integer" isarray uid
-}



generate :: Ast Var Programm -> [Ast Var Programm]
generate pr =
    let (normals, arrays, functions) = split pr
        unusedNormalTypes = Set.toList . Set.delete "nothing" $ Map.keysSet Hot.types Set.\\ Set.fromList (concatMap (map typeOf) normals)
        unusedArrayTypes =  Set.toList . Set.delete "nothing" $ Map.keysSet Hot.types Set.\\ Set.fromList (concatMap (map typeOf) arrays)
        i2code_dummies = Programm $ i2code $ concat functions

        enter_predefined = Programm [ enterFunction $ concat functions ]

        set_get_avail =
              [ map generateNormalGetters normals
              , map generateNormalSetters normals
              , map generateArraySetters arrays
              , map generateArrayGetters arrays
              ]
      
        set_get_empty = 
          [ map generateEmptyNormalSetters unusedNormalTypes
          , map generateEmptyNormalGetters unusedNormalTypes
          , map generateEmptyArraySetters unusedArrayTypes
          , map generateEmptyArrayGetters unusedArrayTypes
          ]
      
        set_get = Programm $ concat [concat set_get_avail, concat set_get_empty]

        --stubs = Programm $ concatMap stubifyFn $ concat functions
    in [i2code_dummies, enter_predefined, set_get]
  where
    defaultReturnValue "real" = Real "0.0"
    defaultReturnValue "integer" = Int "0"
    defaultReturnValue "string" = String ""
    defaultReturnValue "code" = Code (mkFn "DoNothing")
    defaultReturnValue "boolean" = Bool False
    defaultReturnValue _ = Null
  
    generateEmptyArrayGetters ty =
        Function Normal (mkFn $ "_Auto_array_get_global_" <> ty) [("integer", uid), ("integer", idx)] ty [
            Return $ Just $ defaultReturnValue ty
        ]
        
    generateEmptyNormalGetters ty =
        Function Normal (mkFn $ "_Auto_get_global_" <> ty) [("integer", uid)] ty [
            Return $ Just $ defaultReturnValue ty
        ]
        
    generateEmptyArraySetters ty =
        Function Normal (mkFn $ "_Auto_array_set_global_" <> ty) [("integer", uid), ("integer", idx), (ty, val)] "nothing" []
        
    generateEmptyNormalSetters ty =
        Function Normal (mkFn $ "_Auto_set_global_" <> ty) [("integer", uid), (ty, val)] "nothing" []
  
    typeOf :: Ast Var Toplevel -> Type
    typeOf (Global (ADef _ ty)) = ty
    typeOf (Global (SDef _ _ ty _)) = ty
    
    
    uid = mkLocal "_i"
    idx = mkLocal "_idx"
    val = mkLocal "_v"
    
    ctx = mkLocal "_ctx"

    bind = AVar (mkLocal "_Context_bindings") (Var $ SVar ctx)
    scope = AVar (mkLocal "_Context_locals") (Var $ SVar ctx)

    --dummyCodeVars = [Global $ SDef Normal (H.Global Normal ("_dummy_code" <> fromString (show i)) "code" False 0) "code" Nothing | i <- [0..100]]

    i2code :: [Ast Var Toplevel] -> [Ast Var Toplevel]
    i2code fns =

        let r :: Int -> Ast Var Stmt
            r idx = let fn = (fns' ++ fns) !! (100+idx)
                        (v, args) = case fn of
                            Native _ v args _ -> (Code v, args)
                            Function _ v args _ _ -> (Code v, args)
                            Global (SDef _ v _ _) -> (Var $ SVar v, [])
                    in if null args
                    then Return $ Just v
                    else Return . Just . Code $ mkFn "DoNothing"
                    --else Return $ Just Null


            mkDummyFn idx =
                let idx' = fromString . show $ negate idx
                in Function Normal (mkFn $ "_Auto_dummyFunction_" <> idx') [] "nothing" [
                    Call (mkFn "_Wrap_call_anything_around") [Call (H.Op "-") [Int idx']] 
                    --Return . Just . Var $ AVar (mkGlobal "_dummyFunctions") (Int idx')
                ]
            fns' :: [Ast Var Toplevel]
            fns' = map mkDummyFn [-1, -2 .. -101] 
        in fns' ++ [Function Normal (mkFn "_Auto_i2code") [("integer", uid)] "code" [
            bin (-100) (length fns) (Var $ SVar uid) r
        ]]
                    

    enterFunction :: [Ast Var Toplevel] -> Ast Var Toplevel
    enterFunction fns =
        let reg = mkLocal "reg"
            r idx = let fn = fns !! pred idx
                        v@(H.Fn _ types ret _) = case fn of
                                                    Native _ v _ _ -> v
                                                    Function _ v _ _ _ -> v
                        args = zipWith (\ty pos -> 
                                    if ty == "code"
                                    then Call (mkFn "_Auto_i2code") [Call (mkFn "_Table_get_integer") [Var $ bind, Int . fromString $ show pos]]
                                    else Call (mkFn $ "_Table_get_" <> ty) [Var $ bind, Int . fromString $ show pos] )
                                types [1, 2 ..]

                        stmt = if ret == "nothing"
                               then Call v args
                               else if ret == "code"
                               then Call (mkFn "_Table_set_code") [Call (mkFn "_Auto_i2code") [Var $ scope, Var $ SVar reg, Call v args]]
                               else Call (mkFn $ "_Table_set_" <> ret) [Var $ scope, Var $ SVar reg, Call v args {-, Int $ show idx-}]
                    in stmt
        
        in Function Normal (mkFn "_Auto_call_predefined") [("integer", reg), ("integer", uid), ("integer", ctx)] "nothing" [
               bin 1 (length fns) (Var $ SVar uid) r
           ]

    -- TODO: sort on uid and avoid (!!)
    generateArrayGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArrayGetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Return . Just . Var $ AVar (v) (Var $ SVar idx)
        in Function Normal (mkFn $ "_Auto_array_get_global_" <> ty) [("integer", uid), ("integer", idx)] ty [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateArraySetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateArraySetters g@(x:_) =
        let Global (ADef _ ty) = x
            r idx' = let Global (ADef v _) = g !! pred idx'
                    in Set (AVar (v) (Var $ SVar idx)) (Var $ SVar val)
        in Function Normal (mkFn $ "_Auto_array_set_global_" <> ty) [("integer", uid), ("integer", idx), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r
        ]

    generateNormalSetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalSetters g@(x:_) =

        let Global (SDef _ _ ty _) = x

            --offset = length $ dropWhile isConst g
            r idx = let Global (SDef c v _ _) = g !! pred idx
                    in if c == Normal
                    then Set (SVar (v)) (Var $ SVar val)
                    else Return Nothing
        in Function Normal (mkFn $ "_Auto_set_global_" <> ty) [("integer", uid), (ty, val)] "nothing" [
            bin 1 (length g) (Var $ SVar uid) r    
        ]

    isConst (Global (SDef Const _ _ _)) = True
    isConst _ = False


    generateNormalGetters :: [Ast Var Toplevel] -> Ast Var Toplevel
    generateNormalGetters g@(x:_) =
        let Global (SDef _ v ty _) = x
            r idx = let Global (SDef _ v _ _) = g !! pred idx
                    in Return . Just . Var $ SVar v
        in Function Normal (mkFn $ "_Auto_get_global_" <> ty) [("integer", uid)] ty $ [bin 1 (length g) (Var $ SVar uid) r]

bin :: Int -> Int -> Ast Var Expr -> (Int -> Ast Var Stmt) -> Ast Var Stmt
bin lo hi c f = go lo (hi+1)
  where
    go lo hi
        | lo +1== hi = f lo
        | otherwise =
            let mid = (lo+hi) `div` 2
                thenB = go lo mid
                elseB = go mid hi
            in If (Call (mkOp "<") [c, Int . fromString $ show mid]) [thenB] [] (Just [elseB])
-----------------------------------------

isOp :: Name -> Bool
isOp x = x `elem` (["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "and", "or", "not"] :: [Name])

type IsArray = Bool

data Mode = Init | Compile deriving (Eq)

data RenameVariablesState = RenameVariablesState
    { _globalScope :: Map Name Var
    , _globalCount :: Map (Type, IsArray) Int32
    , _localScope :: Map Name Var
    , _fnScope :: Map Name Var
    , _newFnCount :: Int32
    } deriving (Generic)
makeLenses ''RenameVariablesState

instance Binary RenameVariablesState

defaultRenameVariableState = RenameVariablesState mempty mempty mempty mempty 0


newtype RenameVariablesM a = RenameVariablesM { unRenameVariablesM :: ReaderT (Mode, (Type -> Type)) (State RenameVariablesState) a }
    deriving (Functor, Applicative, Monad, MonadState RenameVariablesState, MonadReader (Mode, Type -> Type))

runRenameM :: Mode -> (Type -> Type) -> RenameVariablesState -> Ast Name a -> (Ast Var a, RenameVariablesState)
runRenameM m f st = flip runState st . flip runReaderT (m,f) . unRenameVariablesM . renameVariables

runRenameM' m f = runRenameM m f defaultRenameVariableState

evalRenameM :: Mode -> (Type -> Type) -> RenameVariablesState -> Ast Name a -> RenameVariablesState
evalRenameM m f st a = snd (runRenameM m f st a)

evalRenameM' m f = evalRenameM m f defaultRenameVariableState

execRenameM :: Mode -> (Type -> Type) -> RenameVariablesState -> Ast Name a -> Ast Var a
execRenameM m f st a = fst (runRenameM m f st a)

execRenameM' m f = execRenameM m f defaultRenameVariableState


addLocal :: Name -> Type -> IsArray -> RenameVariablesM Var
addLocal name ty isArray = do
    v' <- uses globalScope $ Map.lookup name
    f <- snd <$> ask
    case v' of
        Just v -> return v
        Nothing -> do
            let successor = if isArray then (+32768) else (+1)
            id <- uses localScope (fromIntegral . successor . Map.size)
            let v = H.Local name (f ty) isArray id
            localScope %= (at name ?~ v)
            return v

addGlobal :: Constant -> Name -> Type -> IsArray -> RenameVariablesM Var
addGlobal c name ty isArray = do
    v' <- uses globalScope $ Map.lookup name
    (m, f) <- ask
    let idf = if m == Init then id else (idf' . negate)
        idf' = if isArray then (*32768) else id
    --f <- snd <$> ask
    case v' of
        Just v -> return v
        Nothing -> do
            globalCount %= (Map.insertWith (+) (f ty, isArray) 1)
            id <- idf <$> uses globalCount (Map.findWithDefault (error "xxx") (f ty, isArray))
            let v = H.Global c name (f ty) isArray id
            globalScope %= (at name ?~ v)
            return v

addFunction :: Name -> [Type] -> Type -> RenameVariablesM Var
addFunction name args ret = do
    v' <- uses fnScope $ Map.lookup name
    case v' of
        Just v -> return v
        Nothing -> do
            (mode, f) <-  ask
            case mode of
                Init -> do
                    id <- uses fnScope (fromIntegral . succ . Map.size)
                    let v = H.Fn name (map f args) (f ret) id
                    fnScope %= (at name ?~ v)
                    return v
                    
                Compile -> do
                    id <- newFnCount <-= 1
                    let v = H.Fn name (map f args) (f ret) id
                    fnScope %= (at name ?~ v)
                    return v
            

getVar :: Name -> RenameVariablesM Var
getVar n = do
   gt <- use globalScope
   fn <- use fnScope
   lt <- use localScope
   let g = First $ Map.lookup n gt
   let f = First $ Map.lookup n fn
   let l = First $ Map.lookup n lt
   return . fromMaybe (error $ show n) . getFirst $ l <> g <> f


enter :: RenameVariablesM ()
enter = localScope .= mempty

{-
This renames all functions, globals and locals.
Locals will start with 1 for the first parameter and count up.
Globals are counted by their primary key (Type, IsArray).
Functions and Natives start at 1 and are counted up.
-}
renameVariables :: Ast Name a -> RenameVariablesM (Ast Var a)
renameVariables = go
  where
    go :: Ast Name a -> RenameVariablesM (Ast Var a)
    go e =
      case e of
        Native c n args ret -> 
            Native c <$> addFunction n (map fst args) ret
                          <*> pure (map (\(typ, name) -> (typ, mkLocal name)) args)
                          <*> pure ret

        Function c name args ret body -> do
            enter
            name' <- addFunction name (map fst args) ret
            argNames <- mapM (\(ty, name) -> addLocal name ty False) args
            let args' = zip (map fst args) argNames
            body' <- mapM go body
            return $ Function c name' args' ret body'

        Global (ADef name ty) ->
            Global <$> (ADef <$> addGlobal Normal name  ty True <*> pure ty)

        Global (SDef c name ty init) ->
            Global <$> (SDef c <$> addGlobal c name ty False <*> pure ty <*> traverse go init)

        Local (SDef c name ty init)
            -> Local <$> (SDef c <$> addLocal name ty False <*> pure ty <*> traverse go init)

        Local (ADef name ty)
            -> Local <$> (ADef <$> addLocal name  ty True <*> pure ty)

        AVar name idx -> AVar <$> getVar name <*> go idx
        SVar name -> SVar <$> getVar name

        Var v -> Var <$> go v

        Set lvar e -> Set <$> go lvar <*> go e

        Call fn args | isOp fn -> Call (mkOp fn) <$> mapM go args
        Call fn args -> Call <$> getVar fn <*> mapM go args
        Code fn -> Code <$> getVar fn

        Programm p -> Programm <$> mapM go p
        Loop p -> Loop <$> mapM go p

        If cond tb eis eb ->
            If <$> go cond
                    <*> mapM go tb
                    <*> mapM go' eis
                    <*> traverse (mapM go) eb
          where
            go' (cond, body) = (,) <$> go cond <*> mapM go body


        Exitwhen cond -> Exitwhen <$> go cond
        Return e -> Return <$> traverse go e

        _ -> return $ unsafeCoerce e


{-
This should be used to parse .j-files which will not be changed, e.g.
common.j and Blizzard.j
-}
onlyParseToplevel :: Ast Name Programm -> RenameVariablesM ()
onlyParseToplevel (Programm pr) = mapM_ go pr
  where
    go :: Ast Name Toplevel -> RenameVariablesM ()
    go e =
      case e of
        Native _ name args ret    -> void $ addFunction name (map fst args) ret
        Function _ name args ret _ -> void $ addFunction name (map fst args) ret
        Global (SDef c name ty _)  -> void $ addGlobal c name ty False
        Global (ADef name ty)    -> void $ addGlobal Normal name ty True


----------------------------------------
