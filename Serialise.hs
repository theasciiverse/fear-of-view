{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Serialise where

import           Codec.Serialise
import           GHC.Generics
import           System.Random.Internal
import           System.Random.SplitMix

import           Creature
import           Equipment
import           Exit
import           Item
import           Power
import           Wall

import qualified Board                  as B
import qualified BoardConf              as BC
import qualified Fov                    as F
import qualified Game                   as G
import qualified Highscore              as HS
import qualified HighscoreV1            as HS1
import qualified Inventory              as I
import qualified Pos                    as P
import qualified RollFrom               as RF
import qualified Tutorial               as T

instance Serialise P.Pos
deriving instance Generic P.WPos
instance Serialise P.WPos
deriving instance Generic P.Dir
instance Serialise P.Dir
deriving instance Generic Creature
instance Serialise Creature
deriving instance Generic Item
instance Serialise Item
deriving instance Generic Equipment
instance Serialise Equipment
deriving instance Generic Wall
instance Serialise Wall
deriving instance Generic Exit
instance Serialise Exit
deriving instance Generic PowerType
instance Serialise PowerType
deriving instance Generic Power
instance Serialise Power
deriving instance Generic a => Generic (RF.RollFrom a)
instance (Generic a, Serialise a) => Serialise (RF.RollFrom a)
deriving instance Generic BC.BoardConf
instance Serialise BC.BoardConf
deriving instance Generic BC.Diffable
instance Serialise BC.Diffable
deriving instance Generic a => Generic (BC.RollFromDiff a)
instance (Generic a, Serialise a) => Serialise (BC.RollFromDiff a)
deriving instance Generic F.Fov
instance Serialise F.Fov
deriving instance Generic B.Status
instance Serialise B.Status
deriving instance Generic B.Board
instance Serialise B.Board
deriving instance Generic B.Alert
instance Serialise B.Alert
deriving instance Generic B.Transition
instance Serialise B.Transition
deriving instance Generic InvItem
instance Serialise InvItem
deriving instance Generic I.Inventory
instance Serialise I.Inventory

deriving instance Generic T.Type
instance Serialise T.Type
deriving instance Generic T.Beat
instance Serialise T.Beat

instance Serialise SMGen where
    encode = encode . show
    decode = read <$> decode

deriving instance Generic StdGen
instance Serialise StdGen

deriving instance Generic G.Game
instance Serialise G.Game

deriving instance Generic HS.Highscore
instance Serialise HS.Highscore
deriving instance Generic HS1.Highscore
instance Serialise HS1.Highscore
