module Exit where

data Exit
    = Exit Bool
    | KeyExit Bool
    | Entrance
    | UnseenBoundary
    | SeenBoundary
    deriving (Eq,Ord)

