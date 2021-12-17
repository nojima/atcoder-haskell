{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE StrictData, BangPatterns #-}
{-# LANGUAGE TupleSections, MultiWayIf, LambdaCase #-}

import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Either as Either

main :: IO ()
main = do
  contents <- ByteString.getContents
  let (Right input) = Parsec.parseOnly word contents
  let ans = solve input
  ByteString.putStrLn (if ans then "Yes" else "No")

solve :: ByteString -> Bool
solve s =
  let
    t = ByteString.concat ["oxx" | i <- [1..10]]
  in
    ByteString.isInfixOf s t

word :: Parsec.Parser ByteString
word = Parsec.takeWhile (not . Parsec.isSpace)
