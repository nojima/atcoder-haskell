{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- getInput
    print (solve input)

getInput :: IO (Char, Char, Char)
getInput =
    runParser parser <$> B.getContents
    where
        parser = (,,) <$> parseChar <*> parseChar <*> parseChar

solve :: (Char, Char, Char) -> Int
solve (a, b, c) =
    f a + f b + f c
    where
        f '1' = 1
        f _   = 0

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
