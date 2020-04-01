data Tree a = Tree a (Tree a) (Tree a)

lookupTree :: Tree a -> Int -> a
lookupTree tree index =
    go tree (msb - 1)
    where
        n = index + 1
        msb = Bits.finiteBitSize n - Bits.countLeadingZeros n - 1
        go (Tree v _   _  ) (-1) = v
        go (Tree _ lhs rhs) level =
            if Bits.testBit n level then
                go rhs (level - 1)
            else
                go lhs (level - 1)

generateTree :: (Int -> a) -> Tree a
generateTree f = go 0
    where
        go index = Tree (f index) (go $ index * 2 + 1) (go $ index * 2 + 2)

memoize :: ((Int -> a) -> Int -> a) -> (Int -> a)
memoize f = f_memo
    where
        f_memo = f (lookupTree tree)
        tree = generateTree f_memo

-------------------------------------------------------------------------------

-- EXAMPLE

fib f n
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = f (n-1) + f (n-2)

fib' = memoize fib
