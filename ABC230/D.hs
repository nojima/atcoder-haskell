{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE StrictData, BangPatterns #-}
{-# LANGUAGE TupleSections, MultiWayIf, LambdaCase, BlockArguments #-}

import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import           Data.List (sortOn, foldl')

data Wall = Wall Int Int
  deriving (Eq, Ord)

main :: IO ()
main = do
  answer <- parseStdin $
    solve <$> int
          <*> int
          <*> Parsec.many1 (Wall <$> int <*> int)
  print answer

solve :: Int -> Int -> [Wall] -> Int
solve n d walls =
  let
    wallsSorted =
      sortOn (\(Wall l r) -> Wall r l) walls

    step (!min_alive, !n_punch) (Wall l r)
      | l < min_alive = (min_alive, n_punch)
      | otherwise     = (r + d, n_punch + 1)

    (_, !min_punch) =
      foldl' step (0, 0) wallsSorted
  in
    min_punch

-------------------------------------------------------------------------------
-- Parser

int :: Parsec.Parser Int
int = Parsec.skipSpace *> Parsec.decimal

parseStdin :: Parsec.Parser a -> IO a
parseStdin parser = do
  contents <- ByteString.getContents
  case Parsec.parseOnly parser contents of
    Right ret -> return ret
    Left errorMessage -> error errorMessage
