module Fov where

import qualified Data.Set as S

import           Group
import qualified Pos      as P

data Fov = Fov { pFov :: S.Set P.Pos, wpFov :: S.Set P.WPos }
    deriving Eq

empty :: Fov
empty = Fov S.empty S.empty

null :: Fov -> Bool
null (Fov ps wps) = S.null ps && S.null wps

unions :: [Fov] -> Fov
unions fovs = Fov (S.unions $ pFov <$> fovs) (S.unions $ wpFov <$> fovs)

union :: Fov -> Fov -> Fov
union a b = unions [a,b]

intersection :: Fov -> Fov -> Fov
intersection (Fov ps wps) (Fov ps' wps') = Fov (ps `S.intersection` ps') (wps `S.intersection` wps')

filter :: (P.Pos -> Bool) -> (P.WPos -> Bool) -> Fov -> Fov
filter pf wpf (Fov ps wps) = Fov (S.filter pf ps) (S.filter wpf wps)

map :: (P.Pos -> P.Pos) -> (P.WPos -> P.WPos) -> Fov -> Fov
map pf wpf (Fov ps wps) = Fov (pf `S.map` ps) (wpf `S.map` wps)

(\\) :: Fov -> Fov -> Fov
(Fov ps wps) \\ (Fov ps' wps') = Fov (ps S.\\ ps') (wps S.\\ wps')

rebase :: P.Pos -> Fov -> Fov
rebase v (Fov ps wps) = Fov ((v +^) `S.map` ps) ((v +^) `S.map` wps)

insertP :: P.Pos -> Fov -> Fov
insertP p (Fov ps wps) = Fov (S.insert p ps) wps
