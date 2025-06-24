module Exit where

data Exit
    = Exit
    | KeyExit
    | Entrance
    | UnseenBoundary
    | SeenBoundary
    deriving (Eq,Ord)

