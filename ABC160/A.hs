main :: IO ()
main = do
    input <- getLine
    if solve input then
        putStrLn "Yes"
    else
        putStrLn "No"

solve :: String -> Bool
solve input =
    case input of
        _:_:c3:c4:c5:c6:_ ->
            c3 == c4 && c5 == c6
        _ -> error "unexpected input"
