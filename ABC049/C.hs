{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Bits as Bits

main :: IO ()
main = do
    input <- B.getLine
    if solve input then
        putStrLn "YES"
    else
        putStrLn "NO"

solve :: B.ByteString -> Bool
solve input =
    solveReversed (B.reverse input)

solveReversed :: B.ByteString -> Bool
solveReversed goal
    | "remaerd" `B.isPrefixOf` goal = solveReversed (B.drop 7 goal)
    | "resare"  `B.isPrefixOf` goal = solveReversed (B.drop 6 goal)
    | "maerd"   `B.isPrefixOf` goal = solveReversed (B.drop 5 goal)
    | "esare"   `B.isPrefixOf` goal = solveReversed (B.drop 5 goal)
    | B.null goal                   = True
    | otherwise                     = False
