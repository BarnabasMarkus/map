
module Map where


data Map k v = Map [(k, v)] deriving (Show, Eq)

find :: Eq a => a -> Map a b -> Maybe b
find _ (Map []) = Nothing
find key (Map ((k, v):xs)) = if key == k 
                             then Just v
                             else find key (Map xs)

insert :: Eq a => a -> b -> Map a b -> Map a b
insert key value map@(Map xs) = 
  fromList $ (key, value) : xs'
  where xs' = if isElem key map
              then filter (\(k, v) -> k /= key) xs
              else xs

update :: Eq a => a -> b -> Map a b -> Map a b
update = insert

fromList :: Eq a => [(a, b)] -> Map a b
fromList xs = Map xs

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

{--
TODO: works only with unique values, remove duplicates
sortByVals :: (Eq a, Ord b) => Map a b -> Map a b
sortByVals m@(Map xs) = 
  let vals = sort $ map snd xs
  in fromList $ [ (fst x, v) | v <- vals, x <- xs, v == snd x]
--}

pretty :: (Show a, Show b) => Map a b -> IO ()
pretty (Map xs) = 
  mapM_ (putStrLn . (\(k,v) -> show k ++ " : " ++ show v )) xs

info :: [String]
info = ["Simple map implementation in haskell"
       ,""
       ,"Functions and examples:"
       ,""
       ,"* find"
       ,"  find 2 myLittleMap"
       ,""
       ,"* insert"
       ,"  insert 2 'X' myLittleMap"
       ,""
       ,"* update (same as insert)"
       ,"  update 2 'X' myLittleMap"
       ,""
       ,"* fromList"
       ,"  let myLittleMap = fromList [(1, 'a'), (2, 'b'), (3, 'c')]"
       ,""
       ,"* toList"
       ,"  toList myLittleMap"
       ,""
       ,"* isElem"
       ,"  isElem 2 myLittleMap"
       ,""
       ,"* isNull"
       ,"  isNull myLittleMap"
       ,""
       ,"* len"
       ,"  len myLittleMap"
       ,""
       ,"* keys"
       ,"  keys myLittleMap"
       ,""
       ,"* vals"
       ,"  vals myLittleMap"
       ,""
       ,"* sortByKeys"
       ,"  sortByKeys myLittleMap"
       ,""
       ,"* pretty"
       ,"  pretty myLittleMap"
       ,""
       ,"* getInfo"
       ,""
       ,"...etc"
       ,""
       ]

getInfo :: IO ()
getInfo = mapM_ putStrLn info


