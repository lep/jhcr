
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


import Data.Map (Map)
import qualified Data.Map as Map


import Data.Set (Set)
import qualified Data.Set as Set

import Data.Monoid
import Data.Semigroup (Semigroup)
import Control.Arrow


import Control.Monad.State
import Control.Monad.Writer

import Data.Tuple
import Data.Int
import Data.String
import Data.List

import Jass.Ast hiding (foldMap)
import Jass.Printer

import qualified Hot.Types as Hot

import Data.ByteString.Builder

import System.IO

import Text.Megaparsec (parse, errorBundlePretty)
import Jass.Parser
import Jass.Ast (Ast)
import Data.Composeable


child2base :: Map Name Name
child2base = Map.fromList [("ability","agent"),("agent","handle"),("aidifficulty","handle"),("alliancetype","handle"),("attacktype","handle"),("blendmode","handle"),("boolexpr","agent"),("buff","ability"),("button","agent"),("camerafield","handle"),("camerasetup","handle"),("conditionfunc","boolexpr"),("damagetype","handle"),("defeatcondition","agent"),("destructable","widget"),("dialog","agent"),("dialogevent","eventid"),("effect","agent"),("effecttype","handle"),("event","agent"),("eventid","handle"),("fgamestate","gamestate"),("filterfunc","boolexpr"),("fogmodifier","agent"),("fogstate","handle"),("force","agent"),("gamecache","agent"),("gamedifficulty","handle"),("gameevent","eventid"),("gamespeed","handle"),("gamestate","handle"),("gametype","handle"),("group","agent"),("hashtable","agent"),("igamestate","gamestate"),("image","handle"),("item","widget"),("itempool","handle"),("itemtype","handle"),("leaderboard","agent"),("lightning","handle"),("limitop","eventid"),("location","agent"),("mapcontrol","handle"),("mapdensity","handle"),("mapflag","handle"),("mapsetting","handle"),("mapvisibility","handle"),("multiboard","agent"),("multiboarditem","agent"),("pathingtype","handle"),("placement","handle"),("player","agent"),("playercolor","handle"),("playerevent","eventid"),("playergameresult","handle"),("playerscore","handle"),("playerslotstate","handle"),("playerstate","handle"),("playerunitevent","eventid"),("quest","agent"),("questitem","agent"),("race","handle"),("racepreference","handle"),("raritycontrol","handle"),("rect","agent"),("region","agent"),("sound","agent"),("soundtype","handle"),("startlocprio","handle"),("terraindeformation","handle"),("texmapflags","handle"),("texttag","handle"),("timer","agent"),("timerdialog","agent"),("trackable","agent"),("trigger","agent"),("triggeraction","handle"),("triggercondition","agent"),("ubersplat","handle"),("unit","widget"),("unitevent","eventid"),("unitpool","handle"),("unitstate","handle"),("unittype","handle"),("version","handle"),("volumegroup","handle"),("weapontype","handle"),("weathereffect","handle"),("widget","agent"),("widgetevent","eventid"), ("integer", "real")]

basetypes :: [Name]
basetypes = ["handle", "code", "real", "string"]


base2children' :: Map Name [Name]
base2children' = Map.fromListWith (++) . map (second return) . map swap $ Map.toList child2base

data Min n = NInf
           | Min n
           deriving (Show)

data Max n = PInf
           | Max n
           deriving (Show)

instance Ord n => Semigroup (Min n) where
    (Min a) <> NInf = Min a
    NInf    <> (Min a) = Min a
    (Min a) <> (Min b) = Min $ min a b

instance Ord n => Monoid (Min n) where
    mempty = NInf
    

instance Ord n => Semigroup (Max n) where
    (Max a) <> PInf = Max a
    PInf    <> (Max a) = Max a
    (Max a) <> (Max b) = Max $ max a b

instance Ord n => Monoid (Max n) where
    mempty = PInf


go :: Map Name [Name] -> Name -> State Int32 (Tree Name)
go base2children name = do
    case Map.lookup name base2children of
        Nothing -> do
            cnt <- get
            modify succ
            return $ Leaf cnt name
        
        Just children -> do
            cnt <- get
            modify succ
            children' <- mapM (go base2children) children
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

    
--offsetMap :: Map Int32 Int32
--offsetMap = Map.fromList $ zip [1,36,42,58,59,88,91,93] [1..]

offsets :: [Tree Name] -> [Int32]
offsets ts = concatMap allChildren'' ts


initFn :: [Tree Name] -> Ast Name Toplevel
initFn ts = Function Normal "_init" [] "nothing" [
    Set (AVar "_toTypeOffset" $ i tyid) $ i off
    | (tyid, off) <- zip (offsets ts) [1..]
    ]
  where
    i = Int . fromString . show

convert :: [Tree Name] -> Ast Name Toplevel
convert ts = Function Normal "_convert" [("integer", "toType"), ("integer", "toReg"), ("integer", "fromType"), ("integer", "fromReg")] "nothing" [
        Set (SVar "toType") $ Var $ AVar "_toTypeOffset" $ Var $ SVar "toType",
        bin (filter isBranch $ concatMap allChildren' ts) toType ((offsetMap Map.!) . getId) convTo
    ]
  where


    toReg = Var $ SVar "toReg"
    fromReg = Var $ SVar "fromReg"
    fromType = Var $ SVar "fromType"
    toType = Var $ SVar "toType"

    toType' = Var $ AVar "_toTypeOffset" toType

    scope = Var $ SVar "Scopes#_scope"

    convTo :: Tree Name -> Ast Name Stmt
    convTo t =
        let children = tail $ allChildren' t
        in bin children fromType getId (convFrom t)

    convFrom :: Tree Name -> Tree Name -> Ast Name Stmt
    convFrom to t = Call ("Table#_set_" <> valueOf to) [scope, toReg, Call ("Table#_get_" <> valueOf t) [Var $ SVar "Scopes#_scope", fromReg]]
    
    offsetMap = Map.fromList $ zip (offsets ts) [1..]
    
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

allChildren'' t =
  case t of
    Leaf{} -> []
    Branch _ a _ children -> a:concatMap allChildren'' children

isLeaf Leaf{} = True
isLeaf _ = False

isBranch = not . isLeaf



ancestors x =
  case Map.lookup x child2base of
    Nothing -> []
    Just p -> p:ancestors p

ancestors' x = x:ancestors x

newtype MonoidMap k v = MonoidMap { getMonoidMap :: Map k v }

singleton :: (Ord k, Monoid v) => k -> v -> MonoidMap k v
singleton k v = MonoidMap $ Map.singleton k v

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
    (MonoidMap a) <> (MonoidMap b) = MonoidMap $ Map.unionWith (<>) a b

instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
    mempty = MonoidMap mempty

script2typehierachy :: Ast v x -> MonoidMap Name [Name]
script2typehierachy x =
  case x of
    Typedef a b -> singleton b [a]
    _ -> composeFold script2typehierachy x


main = do
    x <- parse programm "common.j" <$> readFile "common.j"
    case x of
        Left err -> putStrLn $ errorBundlePretty err
        Right j -> do
            let base2children = getMonoidMap $ script2typehierachy j <> singleton "real" ["integer"]
            --print $ Map.map sort base2children
            --putStrLn ""
            --print $ Map.map sort base2children'
            putStrLn "// scope Convert"
            let (a, b) = evalState ((,) <$> go base2children "handle" <*> go base2children "real") 1
            hPutBuilder stdout . pretty $ Programm
                [ Global (ADef "_toTypeOffset" "integer")
                , convert [a, b]
                , initFn [a, b]
                ]

