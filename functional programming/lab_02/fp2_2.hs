import Prelude hiding (foldr, foldl, map, concat, filter, reverse)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ a []  = a
foldr f a (x : xs) = f x (foldr f a xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ a [] = a
foldl f a (x : xs) = foldl f (f a x) xs

map :: (a -> b) -> [a] -> [b]
map f  = foldr (\x xs -> (f x) : xs) []

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = foldr (\ x xs -> (f x) ++ xs) [] 

concat :: [a] -> [a] -> [a]
concat a b = foldr (:) b a

filter :: (a -> Bool) -> [a] -> [a]  
filter f list = foldr act [] list
    where act el res = if (f el) then el : res else res

maxBy :: (a -> Integer) -> [a] -> a
maxBy f list = let hl = head list in
    foldr (\h max -> if (f h) > (f max) then h else max) hl list

minBy :: (a -> Integer) -> [a] -> a
minBy f list = let hl = head list in
    foldl (\min h -> if (f h) < (f min) then h else min) hl list

reverse :: [a] -> [a]
reverse list = foldl (\t h -> h:t) [] list

elementAt :: Integer -> [a] -> a
elementAt n list | n == 0 = head list
				 | otherwise = checkResult (foldl (\ lst el -> tail lst) list [1..n])
	where checkResult result = case result of 
		[] 	-> error "Index is out of bounds of list"
		(h : t) -> h 

indexOf :: String -> [String] -> Integer
indexOf str list = foldr act (-1) (zip list [0..])
	where act (el, i) res = if str == el then i else res