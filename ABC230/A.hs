import Text.Printf

main :: IO ()
main = do
    line <- getLine
    let n = read line :: Int
    let m = if n >= 42 then n + 1 else n
    printf "AGC%03d\n" m
