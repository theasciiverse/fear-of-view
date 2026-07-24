{-# LANGUAGE LambdaCase #-}

module CommonUI where

subCharAscii :: Bool -> Char -> Char
subCharAscii True = \case
    c | c `elem` "·┌┐└┘┼├┬┤┴╷╴╵╶" -> '+'
    '│'                           -> '|'
    '║'                           -> '}'
    '─'                           -> '-'
    '═'                           -> '='
    c                             -> c
subCharAscii False = id

drawBorderWith :: (Int -> Int -> String -> IO ()) -> Int -> Int -> IO ()
drawBorderWith add w h = do
    add 0 0 $ '┌':replicate (w-2) '─' <> "┐"
    sequence_ [ add y' 0 "│" >> add y' (w-1) "│"
        | y' <- [1..h-2] ]
    add (h-1) 0 ('└':replicate (w-2) '─' <> "┘")
