data Queue a = Queue [a] [a]
    deriving (Show)

instance Eq a => Eq (Queue a)
    where
        (Queue out1 in1) == (Queue out2 in2) = out1 ++ reverse in1 == out2 ++ reverse in2


create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push x (Queue os is) = Queue os (x:is)


pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] is) = Queue (tail (reverse is)) []
pop (Queue os is) = Queue (tail os) is


top :: Queue a -> a
top (Queue [] is) = last is
top (Queue os is) = head os


empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _  = False


main :: IO()
main = do
    let c = push 3 (push 2 (push 1 create))
    print c
    print $ top c
    print $ pop c
    print $ empty $ pop c
    print $ empty $ pop $ pop $ c
    print $ empty $ pop $ pop $ pop c
    
    let c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
    let c2 = push 4 (push 3 (push 2 create))
    print c1
    print c2
    print $ c1 == c2
