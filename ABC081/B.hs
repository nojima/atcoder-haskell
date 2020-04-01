{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import Control.Monad

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- getInput
    print (solve input)

getInput :: IO [Int]
getInput =
    runParser parser <$> B.getContents
    where
        parser = do
            n <- parseInt
            replicateM n parseInt

solve :: [Int] -> Int
solve xs =
    minimum $ map f xs
    where
        f n | n `mod` 2 == 0 = 1 + f (n `div` 2)
            | otherwise      = 0


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

lexeme :: Parser a -> Parser a
lexeme parser =
    consumeSpace *> parser

parseInt_ :: Parser Int
parseInt_ =
    Parser $ \input ->
        Maybe.fromJust (B.readInt input)

parseInt :: Parser Int
parseInt =
    lexeme parseInt_

parseChar :: Parser Char
parseChar =
    Parser $ \input ->
        Maybe.fromJust (B.uncons input)

runParser :: Parser a -> B.ByteString -> a
runParser (Parser parser) input =
    fst $ parser input

-------------------------------------------------------------------------------
