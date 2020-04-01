{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

-------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- getInput
    B.putStrLn (solve input)

getInput :: IO (Int, Int)
getInput =
    let
        parser = (,) <$> parseInt <*> parseInt
    in
        runParser parser <$> B.getContents

solve :: (Int, Int) -> B.ByteString
solve (a, b) =
    if a * b `mod` 2 == 0 then
        "Even"
    else
        "Odd"

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

runParser :: Parser a -> B.ByteString -> a
runParser (Parser parser) input =
    fst $ parser input

-------------------------------------------------------------------------------
