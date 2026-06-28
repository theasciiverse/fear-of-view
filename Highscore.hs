module Highscore where

import           Data.Function (on)
import           Data.List     (sort)

import qualified Data.Set      as S

import           Equipment
import qualified HighscoreV1   as HS1

type Username = String

data Highscore = Highscore
    { score     :: Int
    , maxRound  :: Int
    , maxLevel  :: Int
    , name      :: Maybe Username
    , equipment :: S.Set Equipment
    , gems      :: Int
    } deriving Eq

instance Ord Highscore where
    (<=) = (<=) `on` (\hs -> (-score hs, maxRound hs, maxLevel hs, -gems hs))

type Highscores = [Highscore]

maxHighscores :: Int
maxHighscores = 10

add :: Highscore -> Highscores -> Highscores
add hs = take maxHighscores . sort . (hs:)

empty :: Highscores
empty = []

-- backwards-compatibility shim
fromV1 :: HS1.Highscore -> Highscore
fromV1 (HS1.Highscore s mr ml n e) = Highscore s mr ml n e 0
