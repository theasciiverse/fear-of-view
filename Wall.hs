module Wall where

import qualified Pos as P

data Wall
    = BasicWall
    | Pillar
    | Hedge
    | ThickHedge
    | Window
    | BrokenWindow
    | CloakWall P.Dir Int
    | UmbrellaWall P.Dir
    | TentWall
    deriving (Eq, Ord)

wallDestructionCost :: Wall -> Maybe Int
wallDestructionCost Hedge            = Nothing
wallDestructionCost BrokenWindow     = Nothing
wallDestructionCost (CloakWall _ _)  = Nothing
wallDestructionCost Window           = Just 0
wallDestructionCost TentWall         = Just 0
wallDestructionCost (UmbrellaWall _) = Just 0
wallDestructionCost _                = Just 1
