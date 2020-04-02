{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import Control.Monad (replicateM)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Int (Int64)

main :: IO ()
main =
    print =<< runParser solve <$> B.getContents

solve :: Parser Int64
solve = do
    nEatRed     <- parseInt
    nEatGreen   <- parseInt
    nRed        <- parseInt
    nGreen      <- parseInt
    nWhite      <- parseInt
    redApples   <- replicateM nRed   (fromIntegral <$> parseInt)
    greenApples <- replicateM nGreen (fromIntegral <$> parseInt)
    whiteApples <- replicateM nWhite (fromIntegral <$> parseInt)
    let
        apples = concat
            [ take nEatRed   (sortOn Down redApples)
            , take nEatGreen (sortOn Down greenApples)
            , whiteApples
            ]
        bestApples = take (nEatRed + nEatGreen) (sortOn Down apples)
    return $ sum bestApples

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
