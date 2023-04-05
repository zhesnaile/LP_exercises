data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ lc rc) = 1 + size lc + size rc

height :: Tree a -> Int
height Empty = 0
height (Node _ lc rc) = 1 + child_height
    where
        l_height = height lc
        r_height = height rc
        child_height = max l_height r_height

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node x lcx rcx) (Node y lcy rcy) = x == y && equal lcx lcy && equal rcx rcy
equal _ _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node x lcx rcx) (Node y lcy rcy)
    | x /= y = False
    | (lcx `equal` lcy && rcx `equal` rcy ) || (lcx `equal` rcy && rcx `equal` lcy) = True
    | otherwise = (isomorphic lcx rcy && isomorphic rcx lcy) || (isomorphic lcx lcy && isomorphic rcx rcy)
isomorphic _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x lc rc) = [x] ++ preOrder lc ++ preOrder rc

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x lc rc) = postOrder lc ++ postOrder rc ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x lc rc) = inOrder lc ++ [x] ++ inOrder rc

breadthFirst :: Tree a -> [a]
breadthFirst x = breadthFirstRec [x]
    where
        breadthFirstRec :: [Tree a] -> [a]
        breadthFirstRec [] = []
        breadthFirstRec (Empty:ts) = breadthFirstRec ts
        breadthFirstRec ((Node x l r):ts) = x : breadthFirstRec (ts ++ [l, r])

build :: Eq a => [a] -> [a] -> Tree a 
build [] [] = Empty
build (x:pl) il = Node x 
        (build leftPreorder leftInorder)
        (build rightPreorder rightInorder)
    where 
        leftInorder = takeWhile (/=x) il
        rightInorder = tail (dropWhile (/=x) il)
        leftPreorder = take (length leftInorder) pl
        rightPreorder = drop (length leftInorder) pl

overlap :: (a->a->a) -> Tree a -> Tree a -> Tree a
overlap _ tree1 Empty = tree1
overlap _ Empty tree2 = tree2
overlap op (Node x1 l1 r1) (Node x2 l2 r2) = Node (op x1 x2) 
                                                (overlap op l1 l2)
                                                (overlap op r1 r2)

main :: IO()
main = do
    let t7 = Node 7 Empty Empty
    let t6 = Node 6 Empty Empty
    let t5 = Node 5 Empty Empty
    let t4 = Node 4 Empty Empty
    let t3 = Node 3 t6 t7
    let t2 = Node 2 t4 t5
    let t1 = Node 1 t2 t3
    let t1' = Node 1 t3 t2
    print $ size t1
    print $ height t1
    print $ equal t2 t3
    print $ isomorphic t1 t1'
    print $ preOrder t1
    print $ postOrder t1
    print $ inOrder t1
    print $ breadthFirst t1
    print $ build [1,2,4,5,3] [4,2,5,1,3]
    print $ overlap (+) t2 t3
    print $ overlap (+) t1 t3
{- --}