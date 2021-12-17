{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE StrictData, BangPatterns #-}
{-# LANGUAGE TupleSections, MultiWayIf, LambdaCase, BlockArguments #-}

import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import           Data.Ord (comparing)
import           Data.List (sortOn, foldl')
import qualified Data.Vector.Unboxed as Vector
import           Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Algorithms.Intro as Vector

main :: IO ()
main = do
  answer <- parseStdin $
    solve <$> int
          <*> int
          <*> many1 ((,) <$> int <*> int)
  print answer

solve :: Int -> Int -> Vector (Int, Int) -> Int
solve n d walls =
  let
    wallsSorted =
      Vector.modify
        (Vector.sortBy (comparing \(l, r) -> (r, l)))
        walls

    step (!min_alive, !n_punch) (!l, !r)
      | l < min_alive = (min_alive, n_punch)
      | otherwise     = (r + d, n_punch + 1)

    (_, !min_punch) =
      Vector.foldl' step (0, 0) wallsSorted
  in
    min_punch

-------------------------------------------------------------------------------
-- Parser

int :: Parsec.Parser Int
int = Parsec.skipSpace *> Parsec.decimal

many1 :: Vector.Unbox a => Parsec.Parser a -> Parsec.Parser (Vector a)
many1 parser = Vector.fromList <$> Parsec.many1 parser

parseStdin :: Parsec.Parser a -> IO a
parseStdin parser = do
  contents <- ByteString.getContents
  case Parsec.parseOnly parser contents of
    Right ret -> return ret
    Left errorMessage -> error errorMessage
