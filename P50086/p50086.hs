data Queue a = Queue [a] [a]
    deriving (Show)

instance Eq a => Eq (Queue a)
    where
        (Queue out1 in1) == (Queue out2 in2) = out1 ++ reverse in1 == out2 ++ reverse in2


instance Functor Queue where
    fmap f (Queue os is) = Queue (map f os) (map f is)


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


translation :: Num b => b -> Queue b -> Queue b
translation x = fmap (+ x)


flattenQueue :: Queue a -> [a]
flattenQueue (Queue os is) = os ++ reverse is


instance Applicative Queue where
    pure x = Queue [x] []
    qf <*> q = Queue l []
        where
            l = flattenQueue qf <*> flattenQueue q

instance Monad Queue where
    return = pure
    q >>= f = Queue l []
        where
            l = flattenQueue q >>= flattenQueue . f

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter op q = do
    x <- q
    if op x then return x
    else Queue [] []


main::IO()
main = do
    let c = push 3 (push 2 (push 1 create))
    print $ translation (-2) c
    let c = push 3 (push 2 (push 1 create))
    print $ kfilter (>1) c