{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import Debug.Trace
import Control.Monad

main :: IO ()
main = do
    input <- runParser parseInput <$> B.getContents
    print $ solve input

data Input = Input Int Int [Int]

parseInput :: Parser Input
parseInput = do
    k <- parseInt
    n <- parseInt
    a <- replicateM n parseInt
    return $ Input k n a

solve :: Input -> Int
solve (Input k n a) =
    let
        d1 = (k - maximum a) + minimum a
        d2 = maximum (differences a)
    in
        k - max d1 d2

differences :: [Int] -> [Int]
differences []         = []
differences [_]        = []
differences (x1:x2:xs) = (x2-x1) : differences (x2:xs)

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
