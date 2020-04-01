main :: IO ()
main = do
    line <- getLine
    print $ solve (read line)

solve :: Int -> Int
solve x =
    let
        coin500 = x `div` 500
        remainder = x `mod` 500
        coin5 = remainder `div` 5
    in
        coin500 * 1000 + coin5 * 5
