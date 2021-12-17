{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
{-# LANGUAGE StrictData, BangPatterns #-}
{-# LANGUAGE TupleSections, MultiWayIf, LambdaCase, BlockArguments #-}

import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Char8 (ByteString)
import           Data.Int ( Int64 )

main :: IO ()
main = do
  contents <- ByteString.getContents
  let parser =
        solve <$> int <*> int <*> int
              <*> int <*> int <*> int <*> int
  let (Right answer) = Parsec.parseOnly parser contents
  mapM_ ByteString.putStrLn answer

solve :: Int64 -> Int64 -> Int64
      -> Int64 -> Int64 -> Int64 -> Int64
      -> [ByteString]
solve n a b p q r s = do
  x <- [p..q]
  return $ ByteString.pack do
      y <- [r..s]
      return $ if isFilled a b x y then '#' else '.'

isFilled :: Int64 -> Int64 -> Int64 -> Int64 -> Bool
isFilled a b x y =
  let
    x' = x - a
    y' = y - b
  in
    x' == y' || x' == -y'

int :: Parsec.Parser Int64
int = Parsec.skipSpace *> Parsec.decimal
