
{-# LANGUAGE OverloadedStrings #-}


import Data.Map (Map)
import qualified Data.Map as Map


import Data.Set (Set)
import qualified Data.Set as Set

import Data.Monoid
import Control.Arrow


import Control.Monad.State
import Control.Monad.Writer

import Data.Tuple
import Data.Int
import Data.String
import Data.List

import Jass.Ast
import Jass.Printer

import qualified Hot.Types as Hot

import System.IO




child2base :: Map Name Name
child2base = Map.fromList [("ability","agent"),("agent","handle"),("aidifficulty","handle"),("alliancetype","handle"),("attacktype","handle"),("blendmode","handle"),("boolexpr","agent"),("buff","ability"),("button","agent"),("camerafield","handle"),("camerasetup","handle"),("conditionfunc","boolexpr"),("damagetype","handle"),("defeatcondition","agent"),("destructable","widget"),("dialog","agent"),("dialogevent","eventid"),("effect","agent"),("effecttype","handle"),("event","agent"),("eventid","handle"),("fgamestate","gamestate"),("filterfunc","boolexpr"),("fogmodifier","agent"),("fogstate","handle"),("force","agent"),("gamecache","agent"),("gamedifficulty","handle"),("gameevent","eventid"),("gamespeed","handle"),("gamestate","handle"),("gametype","handle"),("group","agent"),("hashtable","agent"),("igamestate","gamestate"),("image","handle"),("item","widget"),("itempool","handle"),("itemtype","handle"),("leaderboard","agent"),("lightning","handle"),("limitop","eventid"),("location","agent"),("mapcontrol","handle"),("mapdensity","handle"),("mapflag","handle"),("mapsetting","handle"),("mapvisibility","handle"),("multiboard","agent"),("multiboarditem","agent"),("pathingtype","handle"),("placement","handle"),("player","agent"),("playercolor","handle"),("playerevent","eventid"),("playergameresult","handle"),("playerscore","handle"),("playerslotstate","handle"),("playerstate","handle"),("playerunitevent","eventid"),("quest","agent"),("questitem","agent"),("race","handle"),("racepreference","handle"),("raritycontrol","handle"),("rect","agent"),("region","agent"),("sound","agent"),("soundtype","handle"),("startlocprio","handle"),("terraindeformation","handle"),("texmapflags","handle"),("texttag","handle"),("timer","agent"),("timerdialog","agent"),("trackable","agent"),("trigger","agent"),("triggeraction","handle"),("triggercondition","agent"),("ubersplat","handle"),("unit","widget"),("unitevent","eventid"),("unitpool","handle"),("unitstate","handle"),("unittype","handle"),("version","handle"),("volumegroup","handle"),("weapontype","handle"),("weathereffect","handle"),("widget","agent"),("widgetevent","eventid"), ("integer", "real")]

basetypes :: [Name]
basetypes = ["handle", "code", "real", "string"]


base2children :: Map Name [Name]
base2children = Map.fromListWith (++) . map (second return) . map swap $ Map.toList child2base

data Min n = NInf
           | Min n
           deriving (Show)

data Max n = PInf
           | Max n
           deriving (Show)

instance Ord n => Monoid (Min n) where
    mempty = NInf
    (Min a) `mappend` NInf = Min a
    NInf `mappend` (Min a) = Min a
    (Min a) `mappend` (Min b) = Min $ min a b

instance Ord n => Monoid (Max n) where
    mempty = PInf
    (Max a) `mappend` PInf = Max a
    PInf `mappend` (Max a) = Max a
    (Max a) `mappend` (Max b) = Max $ max a b

go :: Name -> State Int32 (Tree Name)
go name = do
    case Map.lookup name base2children of
        Nothing -> do
            cnt <- get
            modify succ
            return $ Leaf cnt name
        
        Just children -> do
            cnt <- get
            modify succ
            children' <- mapM go children
            let bounds = foldMap getBounds children'
            return $ Branch name cnt bounds children'

bin :: [a] -> Ast Name Expr -> (a -> Int32) -> (a -> Ast Name Stmt) -> Ast Name Stmt
bin xs v cond stmt = go xs
  where
    go [x] = stmt x
    go [x,y] =
        let c = Call ("<") [v, i $ cond y]
        in If c [stmt x] [] $ Just [stmt y]
    go xs =
        let len = length xs
            (as, b:bs) = genericSplitAt (len`div`2) xs
            c = Call "<" [v, i $ cond b]
        in If c [go as] [] $ Just [go $ b:bs]

    i = Int . fromString . show

convert :: [Tree Name] -> Ast Name Toplevel
convert ts = Function Normal "_convert" [("integer", "toType"), ("integer", "toReg"), ("integer", "fromType"), ("integer", "fromReg")] "nothing" [
        bin (filter isBranch $ concatMap allChildren' ts) toType' ((offsetMap Map.!) . getId) convTo
    ]
  where
    
    offsetMap :: Map Int32 Int32
    offsetMap = Map.fromList $ zip [1,36,42,58,59,88,91,93] [1..]

    toReg = Var $ SVar "toReg"
    fromReg = Var $ SVar "fromReg"
    fromType = Var $ SVar "fromType"
    toType = Var $ SVar "toType"

    toType' = Var $ AVar "_toTypeOffset" toType

    scope = Var $ SVar "_scope"

    convTo :: Tree Name -> Ast Name Stmt
    convTo t =
        let children = tail $ allChildren' t
        in bin children fromType getId (convFrom t)

    convFrom :: Tree Name -> Tree Name -> Ast Name Stmt
    convFrom to t = Call ("_set_" <> valueOf to) [scope, toReg, Call ("_get_" <> valueOf t) [Var $ SVar "_scope", fromReg]]
    
valueOf t =
  case t of
    Leaf i a -> a
    Branch a i _ _ -> a

getId t =
  case t of
    Leaf id _ -> id
    Branch _ id _ _ -> id


type MinMax = (Min Int32, Max Int32)

data Tree a = Leaf Int32 a
            | Branch a Int32 MinMax [Tree a]
            deriving (Show)

getBounds' t =
  case t of
    Branch _ b _ _ -> (Min b, Max b) <> getBounds t
    _ -> getBounds t

getBounds :: Tree a -> MinMax
getBounds t =
  case t of
    Leaf b _ -> (Min b, Max b)
    Branch _ _ b _ -> b


allChildren :: Tree a -> [(Int32, a)]
allChildren t =
  case t of
    Leaf i a -> [(i, a)]
    Branch a i _ children -> (i,a):concatMap allChildren children

allChildren' t =
  case t of
    Leaf{} -> [t]
    Branch _ _ _ children -> t:concatMap allChildren' children

isLeaf Leaf{} = True
isLeaf _ = False

isBranch = not . isLeaf



ancestors x =
  case Map.lookup x child2base of
    Nothing -> []
    Just p -> p:ancestors p

ancestors' x = x:ancestors x


main =
    let (a, b) = evalState ((,) <$> go "handle" <*> go "real") 1
    in hPutBuilder stdout . pretty $ Programm
        [ Global (ADef "_toTypeOffset" "integer")
        , convert [a, b]
        ]

