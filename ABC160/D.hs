{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import System.IO as IO
import Data.Function ((&))
import Data.Monoid ((<>))

main :: IO ()
main = do
    input <- getInput
    solve input
        & map (\n -> Builder.intDec n <> Builder.byteString "\n")
        & mconcat
        & Builder.hPutBuilder IO.stdout

getInput :: IO (Int, Int, Int)
getInput =
    let
        parser = (,,) <$> parseInt <*> parseInt <*> parseInt
    in
    runParser parser <$> B.getContents

solve :: (Int, Int, Int) -> [Int]
solve (n, x, y) =
    let
        -- 頂点 i と頂点 j の距離 (i < j)
        distance i j =
            let
                -- 辺(x, y)を経由しないときの距離
                d1 = j - i
                -- i -> x -> y -> j という経路を使うときの距離
                d2 = abs (x - i) + 1 + abs (j - y)
                -- i -> y -> x -> j という経路を使うときの距離
                d3 = abs (y - i) + 1 + abs (j - x)
            in
                d1 `min` d2 `min` d3

        allPairDistances = do
            i <- [1..n]
            j <- [(i+1)..n]
            return $ distance i j

        distanceFrequency =
            frequency allPairDistances
    in
        map (\k -> Map.findWithDefault 0 k distanceFrequency) [1..n-1]

frequency :: [Int] -> Map.Map Int Int
frequency xs =
    let
        increment = Map.alter $ \v ->
            case v of
                Nothing    -> Just 1
                Just count -> Just (count + 1)
    in
        foldr increment Map.empty xs

-------------------------------------------------------------------------------

newtype Parser a =
    Parser (B.ByteString -> (a, B.ByteString))

instance Functor Parser where
    fmap f (Parser parser) =
        Parser $ \input ->
            let (v, remainder) = parser input in
                (f v, remainder)

instance Applicative Parser where
    pure v =
        Parser $ \input ->
            (v, input)

    (Parser parser1) <*> (Parser parser2) =
        Parser $ \input ->
            let
                (f, remainder1) = parser1 input
                (v, remainder2) = parser2 remainder1
            in
                (f v, remainder2)

instance Monad Parser where
    return = pure

    (Parser parser1) >>= f =
        Parser $ \input ->
            let
                (v, remainder) = parser1 input
                Parser parser2 = f v
            in
                parser2 remainder

consumeSpace :: Parser ()
consumeSpace =
    Parser $ \input ->
        ((), B.dropWhile Char.isSpace input)

parseInt_ :: Parser Int
parseInt_ =
    Parser $ \input ->
        Maybe.fromJust (B.readInt input)

parseInt :: Parser Int
parseInt =
    consumeSpace *> parseInt_

runParser :: Parser a -> B.ByteString -> a
runParser (Parser parser) input =
    fst $ parser input

-------------------------------------------------------------------------------
