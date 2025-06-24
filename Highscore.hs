module Highscore where

import           Data.Function (on)
import           Data.List     (sort)

import qualified Data.Set      as S

import           Equipment

type Username = String

data Highscore = Highscore
    { score     :: Int
    , maxRound  :: Int
    , maxLevel  :: Int
    , name      :: Maybe Username
    , equipment :: S.Set Equipment
    } deriving Eq

instance Ord Highscore where
    (<=) = (<=) `on` (\hs -> (-score hs, maxRound hs, maxLevel hs))

type Highscores = [Highscore]

maxHighscores :: Int
maxHighscores = 10

add :: Highscore -> Highscores -> Highscores
add hs = take maxHighscores . sort . (hs:)

empty :: Highscores
empty = []
