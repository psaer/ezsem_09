data BinaryTree = EmptyTree
                | Leaf Integer
                | Node Integer BinaryTree BinaryTree deriving Show

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x
insert (Leaf k) x = insert (Node k EmptyTree EmptyTree) x
insert (Node k l r) x | x < k = Node k (insert l x) r
      | x > k = Node k l (insert r x)
      | k == x = Node k l r

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf l) x = if (l == x) then EmptyTree else Leaf l
remove (Node v l r) x | x < v = Node v (remove l x) r
                      | x > v = Node v l (remove r x)
                      | otherwise = concat l r
                  where concat EmptyTree t = t
                        concat (Leaf l) t = Node l EmptyTree t
                        concat (Node v l r) t = Node v l (concat r t)

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l)  x | (l == x) = True
                            | otherwise = False
containsElement (Node v l r) x | x < v = containsElement l x
                               | x > v = containsElement r x
                               | otherwise = v == x                          

nearestGE:: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Not found"
nearestGE (Leaf val) target = if val >= target then val else error "Not found"
nearestGE (Node val left right) target | val < target = nearestGE right target
                                       | (isEmpty left) || (rightMost left < target) = val
                                       | otherwise = case left of 
                                        (Leaf v) -> v
                                        otherwise -> nearestGE left target

rightMost:: BinaryTree -> Integer
rightMost (Leaf v) = v
rightMost (Node v l r) = 
    case r of 
        (EmptyTree) -> v
        (Leaf v') -> v'
        (Node v' l' r') -> rightMost r

isEmpty:: BinaryTree -> Bool
isEmpty bt = 
    case bt of 
        (EmptyTree) -> True
        otherwise -> False                                             


treeFromList :: [Integer] -> BinaryTree
treeFromList = foldl insert EmptyTree

listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Leaf l) = [l]
listFromTree (Node v l r) = (listFromTree l) ++ [v] ++ (listFromTree r)
