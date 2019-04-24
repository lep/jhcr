{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import System.Environment

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Semigroup)

import Control.Monad.State

import Data.Tuple
import Data.Int
import Data.String
import Data.List

import Jass.Ast hiding (foldMap)
import Jass.Printer

import Data.ByteString.Builder

import System.IO

import Text.Megaparsec (parse, errorBundlePretty)
import Jass.Parser
import Jass.Ast (Ast)
import Data.Composeable

data Min n = NInf
           | Min n
           deriving (Show)

data Max n = PInf
           | Max n
           deriving (Show)

instance Ord n => Semigroup (Min n) where
    (Min a) <> NInf    = Min a
    NInf    <> (Min a) = Min a
    (Min a) <> (Min b) = Min $ min a b
    NInf    <> NInf    = NInf

instance Ord n => Monoid (Min n) where
    mempty = NInf
    

instance Ord n => Semigroup (Max n) where
    (Max a) <> PInf    = Max a
    PInf    <> (Max a) = Max a
    (Max a) <> (Max b) = Max $ max a b
    PInf    <> PInf    = PInf

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


offsets :: [Tree Name] -> [Int32]
offsets ts = concatMap allChildren'' ts


initFn :: [Tree Name] -> Ast Name Toplevel
initFn ts = Function Normal "_init" [] "nothing" [
    Set (AVar "_toTypeOffset" $ i tyid) $ i off
    | (tyid, off) <- zip (offsets ts) [1..]
    ]
  where
    i = Int . fromString . show

gTypeBin :: Foldable t => t (Tree Name) -> Ast Name Stmt
gTypeBin ts = bin (concatMap allChildren ts) ty fst (mkCall . snd)
  where
    mkCall tyname = Call "hom" [Var $ SVar tyname, Var $ SVar "macro"]
    ty = Var $ SVar "ty"

convert :: [Tree Name] -> Ast Name Toplevel
convert ts = Function Normal "_convert" [("integer", "_toType"), ("integer", "_toReg"), ("integer", "_fromType"), ("integer", "_fromReg"), ("integer", "_ctx")] "nothing" [
        Set (SVar "_toType") $ Var $ AVar "_toTypeOffset" $ Var $ SVar "_toType",
        bin (filter isBranch $ concatMap allChildren' ts) toType ((offsetMap Map.!) . getId) convTo
    ]
  where
    toReg = Var $ SVar "_toReg"
    fromReg = Var $ SVar "_fromReg"
    fromType = Var $ SVar "_fromType"
    toType = Var $ SVar "_toType"

    scope = Var $ SVar "Context#_locals[_ctx]"

    convTo :: Tree Name -> Ast Name Stmt
    convTo t =
        let children = tail $ allChildren' t
        in bin children fromType getId (convFrom t)

    convFrom :: Tree Name -> Tree Name -> Ast Name Stmt
    convFrom to t = Call ("Table#_set_" <> valueOf to) [scope, toReg, Call ("Table#_get_" <> valueOf t) [Var $ SVar "Context#_locals[_ctx]", fromReg]]
    
    offsetMap = Map.fromList $ zip (offsets ts) [1..]

valueOf :: Tree a -> a
valueOf t =
  case t of
    Leaf _ a -> a
    Branch a _ _ _ -> a

getId :: Tree a -> Int32
getId t =
  case t of
    Leaf id _ -> id
    Branch _ id _ _ -> id


type MinMax = (Min Int32, Max Int32)

data Tree a = Leaf Int32 a
            | Branch a Int32 MinMax [Tree a]
            deriving (Show)


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

allChildren' :: Tree a -> [Tree a]
allChildren' t =
  case t of
    Leaf{} -> [t]
    Branch _ _ _ children -> t:concatMap allChildren' children

allChildren'' :: Tree a -> [Int32]
allChildren'' t =
  case t of
    Leaf{} -> []
    Branch _ a _ children -> a:concatMap allChildren'' children

isLeaf :: Tree a -> Bool
isLeaf Leaf{} = True
isLeaf _ = False

isBranch :: Tree a -> Bool
isBranch = not . isLeaf


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

mkJassTypes :: (Semigroup var, IsString var, Foldable t) => t (Tree var) -> Ast var Programm
mkJassTypes ty = Programm $ map mkGlobal $ concatMap allChildren ty
  where
    mkGlobal (id, name) = Global $ SDef Const ("_" <> name) "integer" $ Just (Int $ show id)

main :: IO ()
main = do
    [commonj] <- getArgs
    x <- parse programm commonj . (<>"\n") <$> readFile commonj
    case x of
        Left err -> putStrLn $ errorBundlePretty err
        Right j -> do
            let base2children = getMonoidMap $ script2typehierachy j <> singleton "real" ["integer"]
            let types@(a:b:_) = evalState (mapM (go base2children) ["handle", "real", "string", "boolean", "code", "nothing"]) 1

            printHaskell types
            printRuntime [a, b]
            printGBinTy $ take 4 types
            printJassTypes types
  where
    printJassTypes ty = do
        f <- openFile "runtime/types.j" WriteMode
        hPutStrLn f "// scope Types"
        hPutBuilder f . pretty $ mkJassTypes ty
        hClose f

    printGBinTy ty = do
        f <- openFile "runtime/g-type-bin.j" WriteMode
        hPutStrLn f "#define hom(type, m) m(type)"
        hPutBuilder f . printStmt $ gTypeBin ty
        hClose f

    printHaskell ty = do
        f <- openFile "Hot/Types.hs" WriteMode
        hPutStrLn f $ unlines
          [ "{-# LANGUAGE OverloadedStrings #-}"
          , "module Hot.Types (types) where"
          , "import Data.Map (Map)"
          , "import qualified Data.Map as Map"
          , "import Data.Int"
          , "types :: Map String Int16"
          , ("types = Map.fromList " <> ) . show . map swap $ concatMap allChildren ty
          ]
        hClose f
    
    
    printRuntime ty = do
        f <- openFile "runtime/convert.j" WriteMode
        hPutStrLn f "// scope Convert"
        hPutBuilder f . pretty $ Programm
            [ Global (ADef "_toTypeOffset" "integer")
            , convert ty
            , initFn ty
            ]
        hClose f
