{- M A P   D A T A   S T R U C T U R E -}


module Map
  ( Map (..)
  , find
  , insert
  , update
  , fromList
  , toList
  , isElem
  , isNull
  , len
  , keys
  , vals
  , sortByKeys
  , pretty
  )
  where


import Data.Semigroup


data Map k v = Map [(k, v)] deriving Eq


instance (Show a, Show b) => Show (Map a b) where
  show = pretty


instance (Eq a) => Functor (Map a) where
  fmap _ (Map []) = Map []
  fmap f (Map xs) = Map xs'
    where xs' = map (\(k, v) -> (k, f v)) xs


instance (Eq a) => Semigroup (Map a b) where
  (<>) (Map xs) (Map ys) = fromList $ xs ++ ys


instance (Eq a) => Monoid (Map a b) where
  mempty = Map []
  mappend = (<>)


find :: Eq a => a -> Map a b -> Maybe b
find _ (Map []) = Nothing
find key (Map ((k, v):xs)) = if key == k
                             then Just v
                             else find key (Map xs)

insert :: Eq a => a -> b -> Map a b -> Map a b
insert key value m@(Map xs) =
  Map $ (key, value) : xs'
  where xs' = if isElem key m
              then filter (\(k, v) -> k /= key) xs
              else xs

update :: Eq a => a -> b -> Map a b -> Map a b
update = insert

fromList :: Eq a => [(a, b)] -> Map a b
fromList xs = foldr (\(k,v) -> insert k v) (Map []) xs

toList :: Map a b -> [(a, b)]
toList (Map xs) = xs

isElem :: Eq a => a -> Map a b -> Bool
isElem key (Map xs) = elem key $ keys (Map xs)

isNull :: Map a b -> Bool
isNull (Map xs) = null xs

len :: Map a b -> Int
len (Map xs) = length xs

keys :: Map a b -> [a]
keys (Map xs) = map fst xs

vals :: Map a b -> [b]
vals (Map xs) = map snd xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = (sort smallers) ++ [x] ++ (sort greaters)
  where smallers = filter (< x) xs
        greaters = filter (>= x) xs

sortByKeys :: Ord a => Map a b -> Map a b
sortByKeys m@(Map xs) =
  let keys = sort $ map fst xs
  in fromList $ [(k, snd x) | k <- keys, x <- xs, k == fst x]

pretty :: (Show a, Show b) => Map a b -> String
pretty (Map []) = ""
pretty (Map ((k,v):xs)) =
  mconcat [show k, "\t", show v, "\n", pretty (Map xs)]
