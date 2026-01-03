{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Board where

import           Control.Monad         (foldM, guard, when)
import           Control.Monad.Random  (Rand, StdGen)
import           Control.Monad.Writer  (Writer, WriterT, lift, mapWriterT,
                                        runWriter, tell)
import           Data.Bifunctor        (second)
import           Data.Function         (on)
import           Data.Functor          (($>))
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe            (fromMaybe, mapMaybe)
import           Data.Monoid           (Any (..), Sum (..))
import           Safe                  (atMay, headMay, minimumMay)

import qualified Data.HashSet          as HS
import qualified Data.List             as L
import qualified Data.Map.Strict       as M
import qualified Data.Ord              as O
import qualified Data.Set              as S

import qualified BoardConf             as BC
import qualified Pos                   as P
import qualified Power                 as Pow
import qualified RollFrom              as RF

import           AStar
import           Creature
import           Exit
import           Group
import           Item
import           Rand
import           Wall

w,h :: Int
w = 5
h = 5

inBounds :: P.Pos -> Bool
inBounds (P.Pos x y) = 0 <= x && x < w && 0 <= y && y < h

inBoundsW :: P.WPos -> Bool
inBoundsW = all inBounds . P.adjPoss

poss :: S.Set P.Pos
poss = S.fromList [ P.Pos x y | x <- [0..w-1], y <- [0..h-1] ]

wPoss :: S.Set P.WPos
wPoss = S.fromList [ wp | p <- S.toList poss, up <- [True,False], let wp = P.WPos p up, all inBounds $ P.adjPoss wp ]

boundaryWPoss :: S.Set P.WPos
boundaryWPoss = S.fromList $
     [ P.WPos (P.Pos x y) True | x <- [0..w-1], y <- [-1,h-1] ]
     <> [ P.WPos (P.Pos x y) False | x <- [-1,w-1], y <- [0..h-1] ]

isBoundaryWPos :: P.WPos -> Bool
isBoundaryWPos (P.WPos (P.Pos _ y) True)  = y `elem` [-1,h-1]
isBoundaryWPos (P.WPos (P.Pos x _) False) = x `elem` [-1,w-1]

treasuresPerBoard :: Int
treasuresPerBoard = 4

-- XXX If all active, status line is 58 long -- watch out with renaming!
data Status = Dazzled | Smoke | Haste | Ghost | Foresight deriving (Eq, Ord, Show)

type Tagged = S.Set P.WPos

data Board = Board
    { visible    :: S.Set P.Pos
    , unrevealed :: S.Set P.Pos
    , creatures  :: M.Map P.Pos Creature
    , items      :: M.Map P.Pos Item
    , walls      :: M.Map P.WPos Wall
    , exits      :: M.Map P.WPos Exit
    , powers     :: M.Map P.Pos Pow.Power
    , found      :: [Item]
    , timer      :: Int
    , possItems  :: [Item]
    , safe       :: Bool
    , statuses   :: M.Map Status Int
    , expected   :: M.Map P.Pos (Maybe Creature)
    , tagged     :: Tagged
    , diffs      :: BC.BoardConfDiffs
    , conf       :: BC.BoardConf
    }

new :: BC.BoardConf -> Board
new = Board S.empty poss M.empty M.empty M.empty initBoundary M.empty [] 0 [] True M.empty M.empty S.empty M.empty where
     initBoundary = M.fromSet (const UnseenBoundary) boundaryWPoss

empty :: Board
empty = new BC.emptyBoardConf

modVisible :: (S.Set P.Pos -> S.Set P.Pos) -> Board -> Board
modVisible f bd = bd { visible = f $ visible bd }
setVisible :: S.Set P.Pos -> Board -> Board
setVisible = modVisible . const

modUnrevealed :: (S.Set P.Pos -> S.Set P.Pos) -> Board -> Board
modUnrevealed f bd = bd { unrevealed = f $ unrevealed bd }
setUnrevealed :: S.Set P.Pos -> Board -> Board
setUnrevealed = modUnrevealed . const

modCreatures :: (M.Map P.Pos Creature -> M.Map P.Pos Creature) -> Board -> Board
modCreatures f bd = bd { creatures = f $ creatures bd }

modItems :: (M.Map P.Pos Item -> M.Map P.Pos Item) -> Board -> Board
modItems f bd = bd { items = f $ items bd }

modWalls :: (M.Map P.WPos Wall -> M.Map P.WPos Wall) -> Board -> Board
modWalls f bd = bd { walls = f $ walls bd }

modExits :: (M.Map P.WPos Exit -> M.Map P.WPos Exit) -> Board -> Board
modExits f bd = bd { exits = f $ exits bd }

modPowers :: (M.Map P.Pos Pow.Power -> M.Map P.Pos Pow.Power) -> Board -> Board
modPowers f bd = bd { powers = f $ powers bd }

modTimer :: (Int -> Int) -> Board -> Board
modTimer f bd = bd { timer = f $ timer bd }
setTimer :: Int -> Board -> Board
setTimer = modTimer . const

modFound :: ([Item] -> [Item]) -> Board -> Board
modFound f bd = bd { found = f $ found bd }

setSafe :: Bool -> Board -> Board
setSafe s bd = bd { safe = s }

modStatuses :: (M.Map Status Int -> M.Map Status Int) -> Board -> Board
modStatuses f bd = bd { statuses = f $ statuses bd }

modExpected :: (M.Map P.Pos (Maybe Creature) -> M.Map P.Pos (Maybe Creature)) -> Board -> Board
modExpected f bd = bd { expected = f $ expected bd }
setExpected :: M.Map P.Pos (Maybe Creature) -> Board -> Board
setExpected = modExpected . const

modTagged :: (Tagged -> Tagged) -> Board -> Board
modTagged f bd = bd { tagged = f $ tagged bd }

modConf :: (BC.BoardConf -> BC.BoardConf) -> Board -> Board
modConf f bd = bd { conf = f $ conf bd }

wPosVisible :: S.Set P.Pos -> P.WPos -> Bool
wPosVisible vis wp = any (`S.member` vis) $ P.adjPoss wp

visibleWPoss, invisibleWPoss :: Tagged -> S.Set P.Pos -> S.Set P.WPos
visibleWPoss tags vis = tags `S.union` S.filter (wPosVisible vis) wPoss
invisibleWPoss tags vis = S.filter (not . wPosVisible vis) wPoss S.\\ tags

invisible :: Board -> S.Set P.Pos
invisible bd = poss S.\\ visible bd

treasuresLeft :: Board -> Int
treasuresLeft bd = treasuresPerBoard - length (found bd)

onSpawnTreasure :: Item -> Board -> Board
onSpawnTreasure tr bd = (if treasuresLeft bd > 1 then setUnrevealed poss else id)
    $ modFound (<>[tr]) bd

setPlayerDead :: Board -> Board
setPlayerDead = modCreatures . M.map $ \case
    Player -> DeadPlayer
    c      -> c

powerTypeAt :: P.Pos -> Pow.PowerType
powerTypeAt (P.Pos x y)
    | 2*x > w = powerTypeAt (P.Pos (w-1-x) y)
    | 2*y > h = powerTypeAt (P.Pos x (h-1-y))
powerTypeAt (P.Pos 0 0) = Pow.Smoke
powerTypeAt (P.Pos 0 1) = Pow.Dazzle
powerTypeAt (P.Pos 0 2) = Pow.Teleport
powerTypeAt (P.Pos 1 0) = Pow.Ghost
powerTypeAt (P.Pos 1 1) = Pow.Heal
powerTypeAt (P.Pos 1 2) = Pow.Haste
powerTypeAt (P.Pos 2 0) = Pow.Teleport
powerTypeAt (P.Pos 2 1) = Pow.Foresight
powerTypeAt (P.Pos 2 2) = Pow.Undo
powerTypeAt _ = Pow.Heal -- impossible

treasureAt :: Board -> P.Pos -> Item
treasureAt bd p@(P.Pos x y)
    | treasuresLeft bd == 1 = Gem $ powerTypeAt p
    | 2*x > w = treasureAt bd (P.Pos (w-1-x) y)
    | 2*y > h = treasureAt bd (P.Pos x (h-1-y))
treasureAt _ (P.Pos 2 2) = Junk
treasureAt _ (P.Pos 1 1) = Potion
treasureAt bd (P.Pos 1 0) | Just i <- headMay $ possItems bd = i
treasureAt bd (P.Pos 0 y) | Just i <- possItems bd `atMay` (y+1) = i
treasureAt bd (P.Pos 2 0) = treasureAt bd $ P.Pos 0 2
treasureAt _ _ = ScoreTreasure

sortOn' :: (a -> Int) -> [a] -> [a]
#if !MIN_VERSION_base(4,8,0)
sortOn' = L.sortOn
#else
sortOn' = L.sortBy . O.comparing
#endif

setFov :: Bool -> Board -> Rand StdGen Board
setFov hasKey bd0 = setSafe False <$> iter bd0 where
    iter bd =
        let oldVis = visible bd
            newVis = fov bd
            oldWVis = visibleWPoss (tagged bd) oldVis
            newWVis = visibleWPoss (tagged bd) newVis
        in if newVis == oldVis
        then seeBoundary hasKey $ destroyInvisStuffs bd
        else do
            bd' <- destroyInvisStuffs . updateVis . destroyInvisWalls . updateVis <$>
                createAtW (newWVis S.\\ oldWVis) bd
            newVis' <- (sortOn' pdist <$>) . shuffle . S.toList $ visible bd' S.\\ oldVis
            let newInvis' = S.toList $ oldVis S.\\ visible bd'
            bd'' <- revealVis <$> (expectAtMaybe newInvis' =<< createAt newVis' bd')
            -- created orbs may lead to new vision, so need to iterate
            iter bd''
    destroyInvisWalls, destroyInvisStuffs :: Board -> Board
    destroyInvisStuffs bd = modCreatures delPInvis . modItems delPInvis $ bd where
        delPInvis :: M.Map P.Pos a -> M.Map P.Pos a
        delPInvis = (M.\\ M.fromSet (const ()) (invisible bd))
    destroyInvisWalls bd = modWalls delWPInvis bd where
        delWPInvis :: M.Map P.WPos a -> M.Map P.WPos a
        delWPInvis = (M.\\ M.fromSet (const ()) (invisibleWPoss (tagged bd) (visible bd)))
    expectAtMaybe
        | expectant bd0 = expectAt
        | otherwise = const pure
    createAt :: [P.Pos] -> Board -> Rand StdGen Board
    createAt = flip $ foldM createP
    createAtW :: S.Set P.WPos -> Board -> Rand StdGen Board
    createAtW = flip $ foldM createW
    bc = conf bd0
    createP :: Board -> P.Pos -> Rand StdGen Board
    createW :: Board -> P.WPos -> Rand StdGen Board
    createP bd p = modExpected (M.delete p) <$> createP' (modUnrevealed (S.delete p) bd) p
    createP' bd p
        | S.null (unrevealed bd) && treasuresLeft bd > 0 =
            let treasure = treasureAt bd p
            in pure . onSpawnTreasure treasure . modItems (M.insert p treasure) $ bd
        | safe bd = pure bd
        | p `M.member` creatures bd = pure bd
        | expectant bd = pure $ case expected bd M.!? p of
            Just (Just c) -> modCreatures (M.insert p c) bd
            _             -> bd
        | otherwise =
            maybe bd (($ bd) . modCreatures . M.insert p) <$> RF.roll (BC.creatureRoll bc)
    createW bd wp
        | safe bd
        , ppos:_ <- playerPoss bd
        , wp `elem` (P.wposInDir ppos <$> P.dirs) = pure bd
        | otherwise =
            maybe bd (($ bd) . modWalls . M.insert wp) <$> RF.roll (BC.wallRoll bc)
    revealVis bd
        -- | Needed when all poss revealed on spawning treasure
        | S.null (unrevealed bd) && treasuresLeft bd > 0 = setUnrevealed (poss `S.difference` visible bd) bd
        -- | Needed only when we created a treasure
        | otherwise = modUnrevealed (`S.difference` visible bd) bd
    pdist :: P.Pos -> Int
    pdist | ppos:_ <- playerPoss bd0 = P.distSquared ppos
        | otherwise = const 0

seeBoundary :: Bool -> Board -> Rand StdGen Board
seeBoundary hasKey bd = do
    v <- newVisBdd
    pure $ modExits (flip (foldr set) v) bd
    where
    newVisBdd = (sortOn' (negate . pdist) <$>) . shuffle . S.toList . S.filter (wPosVisible $ visible bd) .
             S.filter ((== Just UnseenBoundary) . (exits bd M.!?)) $ boundaryWPoss
    set :: P.WPos -> M.Map P.WPos Exit -> M.Map P.WPos Exit
    set wp exs = M.insert wp ex exs where
        ex  | needKeyExit && S.size (S.map posOfBoundary $ S.delete wp unseen) == 1 = KeyExit True
            | S.null . S.delete wp $ S.filter (sameWall wp) unseen =
                case S.toList $ S.filter (not . sameWall wp) unseen of
                    [] -> Exit True
                    wp':_ | needKeyExit && all (sameWall wp') (S.delete wp unseen) -> KeyExit True
                    _ -> SeenBoundary
            | otherwise = SeenBoundary
        unseen = S.filter ((== Just UnseenBoundary) . (exs M.!?)) boundaryWPoss
        sameWall = (==) `on` P.exitDir
        needKeyExit = hasKey && not (any (\case {KeyExit _ -> True; _ -> False}) $ M.elems exs)
        posOfBoundary wp' = P.posInDir wp' (P.negDir $ P.exitDir wp')
    pdist :: P.WPos -> Int
    pdist | ppos:_ <- playerPoss bd = P.distSquaredToWPos ppos
        | otherwise = const 0


fov, playerFov :: Board -> S.Set P.Pos
fov bd = addOrbFovs plFov plFov where
    plFov = fovs $ playerPoss bd <> cameraPoss bd
    fovs = S.unions . map (fovAt bd)
    addOrbFovs :: S.Set P.Pos -> S.Set P.Pos -> S.Set P.Pos
    addOrbFovs v news
        | S.null news = v
        | otherwise = let
            orbPoss = [ p | (p, i) <- M.assocs $ M.restrictKeys (items bd) news, isOrb i ]
            orbFovs = fovs orbPoss
            isOrb (ItemInvItem Orb) = True
            isOrb (RollingOrb _ _)  = True
            isOrb _                 = False
        in addOrbFovs (v `S.union` orbFovs) (orbFovs S.\\ v)
playerFov bd = S.unions . map (fovAt bd) $ playerPoss bd


playerPoss :: Board -> [ P.Pos ]
playerPoss bd = [ p | (p,c) <- M.assocs $ creatures bd, c `elem` [Player, DeadPlayer] ]

cameraPoss :: Board -> [ P.Pos ]
cameraPoss bd = [ p | (p, ItemInvItem (Camera _)) <- M.assocs $ items bd ]

fovAt :: Board -> P.Pos -> S.Set P.Pos
fovAt bd base = S.filter inRange $ poss S.\\ blocked where
    blocked = S.unions [ wallCone (short wl) base wp
        | (wp,wl) <- M.assocs $ walls bd, wl `notElem` [Window, BrokenWindow]
        ] where
        short Pillar           = True
        short (UmbrellaWall _) = True
        short _                = False
    inRange
        | Just r <- sightRadius = (<= r*r) . ((smokeFac*smokeFac)*) . P.distSquared base
        | otherwise = const True
        where
        sightRadius = minimumMay $ mapMaybe rad (M.assocs (statuses bd))
        rad (Dazzled,_) = Just 0
        rad (Smoke,n)   = Just $ max 0 (maxSmoke - n) + smokeFac
        rad _           = Nothing

wallCone :: Bool -> P.Pos -> P.WPos -> S.Set P.Pos
wallCone short base wp = (poss `S.intersection`) $ (base +^) `S.map` wallCone' (neg base +^ wp) where
    wallCone' (P.WPos p False) = flipPos `S.map` wallCone' (P.WPos (flipPos p) True)
    wallCone' (P.WPos (P.Pos x y) True)
        | x < 0 = (\(P.Pos x' y') -> P.Pos (-x') y') `S.map` wallCone' (P.WPos (P.Pos (-x) y) True)
        | y < 0 = (\(P.Pos x' y') -> P.Pos x' (-y')) `S.map` wallCone' (P.WPos (P.Pos x (-y-1)) True)
        | otherwise =
            S.fromList [ P.Pos x' y'
                | x' <- [-w..w], y' <- [y+1..h]
                , ineq x' y'
                ] where
        -- (y+1/2)/(x-w/2) <= y'/x' <= (y+1/2)/(x+w/2)
        ineq x' y' | short = abs (x'*(8*y+4) - 8*y'*x) <= 3*y' -- w = 3/4
            | otherwise    = abs (x'*(2*y+1) - 2*y'*x) <= y' -- w = 1
    flipPos (P.Pos x y) = P.Pos y x

smokeFac, maxSmoke :: Int
smokeFac = 4
maxSmoke = 20

updateVis :: Board -> Board
updateVis bd = setVisible (fov bd) bd

data Move = Move {mvCreature :: Creature, mvFrom :: P.Pos, mvDir :: P.Dir}

data Alert
    = AlertMoveCreature Creature P.Pos P.Dir
    | AlertMoveCreatureVia Creature P.Pos
    | AlertMoveItem Item P.Pos P.Dir
    | AlertUseItem Item P.Pos P.Dir
    | AlertHighlight (S.Set P.Pos) (S.Set P.WPos)
data Transition = Transition { transBase :: Board, transAlerts :: [Alert] }

data MoveResults = MoveResults
    { damage     :: Sum Int
    , exitings   :: [P.WPos]
    , alarmings  :: [P.Dir]
    , alerts     :: [Alert]
    , someAction :: Any
    }
instance Semigroup MoveResults where
    MoveResults a b c d e <> MoveResults a' b' c' d' e' = MoveResults (a<>a') (b<>b') (c<>c') (d<>d') (e<>e')
instance Monoid MoveResults where
    mempty = MoveResults mempty mempty mempty mempty mempty
action :: MoveResults
action = mempty { someAction = Any True }

movePlayers :: P.Dir -> Board -> Writer MoveResults Board
movePlayers dir bd = foldM (flip $ tryMoveCreature dir) bd (playerPoss bd)

throughWall :: Board -> Creature -> Maybe Wall -> Bool
throughWall _ _ Nothing                     = True
throughWall _ GhostMonster _                = True
throughWall bd Player _ | ghostly bd        = True
throughWall _ Player (Just Hedge)           = True
throughWall _ (InflatedBalloon _) (Just _)  = False
throughWall _ _ (Just BrokenWindow)         = True
throughWall _ _ _                           = False

creatureCanMove :: Board -> Creature -> P.Pos -> P.Dir -> Bool
creatureCanMove bd c p dir =
    let p' = P.dirPos dir +^ p
        wp = P.wposInDir p dir
    in inBounds p' && (c == Player || M.notMember p' (creatures bd)) && throughWall bd c (walls bd M.!? wp)

creatureCanTryMove :: Board -> Creature -> P.Pos -> P.Dir -> Bool
creatureCanTryMove bd c p dir =
    let p' = P.dirPos dir +^ p
        wp = P.wposInDir p dir
    in and
        [ inBounds p'
        , creatures bd M.!? p' `elem` [Nothing, Just Player]
        , throughWall bd c $ walls bd M.!? wp
        , c /= SmartMonster || p' `S.member` playerFov bd
        ]

enterAt :: P.WPos -> Board -> Board
enterAt wp@(P.WPos (P.Pos x y) _) = modVisible (S.insert p') . modCreatures (M.insert p' Player) . modExits (M.insert wp Entrance) where
    p' = P.Pos (max 0 x) (max 0 y)

oppositeWPos :: P.WPos -> P.WPos
oppositeWPos (P.WPos (P.Pos x y) True)  = P.WPos (P.Pos x (h-2-y)) True
oppositeWPos (P.WPos (P.Pos x y) False) = P.WPos (P.Pos (w-2-x) y) False

tryMoveCreature :: P.Dir -> P.Pos -> Board -> Writer MoveResults Board
tryMoveCreature dir p bd
    | Just Player <- creatures bd M.!? p
    , not $ ghostly bd
    , Just wl <- walls bd M.!? wp
    , Just cost <- wallDestructionCost wl
    = do
        tell $ action { damage = Sum cost }
        pure . modTagged (S.delete wp) . modWalls (M.update damageWall wp) $ bd
    | Just c <- creatures bd M.!? p, creatureCanMove bd c p dir = do
        let p' = P.dirPos dir +^ p
        bd' <- case creatures bd M.!? p' of
            Just (InflatedBalloon _) | c == Player ->
                tryMoveCreature dir p' bd
            _ -> pure bd
        let creatureDestroyed = maybe False isMonster $ creatures bd' M.!? p'
            throughPainful = c == Player && not (ghostly bd) &&
                (walls bd M.!? wp) `elem` [Just Hedge, Just BrokenWindow]
            dmg = sum $ [2 | creatureDestroyed] <> [1 | throughPainful]
            modMove = modCreatures $ M.insert p' c . M.delete p
            modBalloon
                | Just (InflatedBalloon charges) <- creatures bd' M.!? p'
                , charges > 0
                , p' `M.notMember` items bd'
                = modItems . M.insert p' . ItemInvItem $ Balloon charges
                | otherwise = id
        tell $ action { damage = Sum dmg, alerts = move c }
        pure . modBalloon . modMove $ bd'
    | Just Player <- creatures bd M.!? p
    , Just e <- exits bd M.!? wp = case e of
        Exit True     -> breakExit Exit
        KeyExit True  -> breakExit KeyExit
        Exit False    -> leave
        KeyExit False -> leave
        _             -> pure bd
    | otherwise = pure bd
    where
    wp = P.wposInDir p dir
    move c = [AlertMoveCreature c p dir]
    damageWall = \case
        ThickHedge -> Just Hedge
        Window     -> Just BrokenWindow
        _          -> Nothing
    breakExit :: (Bool -> Exit) -> Writer MoveResults Board
    breakExit etp = do
        tell action { alarmings = [dir] }
        pure . modConf (BC.apply (diffs bd) dir) $ modExits (M.insert wp $ etp False) bd
    leave = do
        tell $ action { exitings = [wp], alerts = move Player }
        pure $ modCreatures (M.delete p) bd

collectItems :: Board -> Writer [Item] Board
collectItems bd
    | p:_ <- playerPoss bd
    = collectItemsAt p bd
    | otherwise = pure bd

collectItemsAt :: P.Pos -> Board -> Writer [Item] Board
collectItemsAt p bd
    | Just item <- items bd M.!? p = let
        takeUmbrella
            | UmbrellaHandle d _ <- item =
                let wp' = P.wposInDir p d
                in modTagged (S.delete wp') . modWalls (M.delete wp')
            | otherwise = id
        getItem
            | UmbrellaHandle d _ <- item = P.wposInDir p d `M.member` walls bd
            | otherwise = True
        in do
            when getItem $ tell [item]
            pure . modItems (M.delete p) . takeUmbrella $ bd
    | otherwise = pure bd

grabItem :: P.Dir -> Board -> Writer [Item] Board
grabItem d bd
    | p:_ <- playerPoss bd
    = collectItemsAt (p +^ P.dirPos d) bd
    | otherwise = pure bd


findPathDir, chaseDir :: Creature -> Board -> P.Pos -> P.Pos -> Rand StdGen (Maybe P.Dir)
findPathDir c bd from goal = do
    let firsts = headMay `mapMaybe` aStarAll graph (P.sqDist goal) (== goal) from
        dirs = (P.posDir . (+^ neg from)) `mapMaybe` firsts
    randElem dirs
    where
    graph :: P.Pos -> HS.HashSet P.Pos
    graph p = HS.fromList [ p +^ P.dirPos d | d <- P.dirs,  creatureCanTryMove bd c p d ]
chaseDir c bd from goal =
    let diff = from +^ neg goal
        ok = creatureCanTryMove bd c from
        dirs = filter ok <$> P.dirsTowardsZero diff
    in headMay . concat <$> mapM shuffle dirs

npcs :: Board -> M.Map P.Pos Creature
npcs = M.filter (/= Player) . creatures

incStatus :: Status -> Int -> Board -> Board
incStatus st n = modStatuses $ M.alter f st where
    f (Just n') = Just $ n + n'
    f _         = Just n
hasted,isHasteRound,ghostly,expectant,preexpectant :: Board -> Bool
hasted = M.member Haste . statuses
ghostly = M.member Ghost . statuses
expectant = M.member Foresight . statuses
-- |preexpectant -- will we be expectant after moving?
preexpectant bd
    | Just n <- statuses bd M.!? Foresight = n > 0 || isHasteRound bd
    | otherwise = False
isHasteRound bd
    | Just n <- statuses bd M.!? Haste = n `mod` 2 == 1
    | otherwise = False

beginExpect :: Board -> Rand StdGen Board
beginExpect bd
    | expectant bd = pure bd
    | otherwise = (expectAt . S.toList $ invisible bd) bd

expectAt :: [P.Pos] -> Board -> Rand StdGen Board
expectAt ps bd0 = foldM expectP bd0 ps
    where
    expectP bd p = ($ bd) . modExpected . M.insert p <$> RF.roll (BC.creatureRoll $ conf bd0)

npcsAct :: Board -> WriterT [Alert] (Rand StdGen) Board
npcsAct bd0
    | isHasteRound bd0 = pure bd0
    | otherwise = do
    actors <- lift . shuffle . M.toList $ npcs bd0
    bd' <- foldM npcAct bd0 actors
    fastActors <- lift . shuffle . filter (isFast . snd) . M.toList $ npcs bd'
    foldM npcAct bd' fastActors <* mapM_ (tell . (\(p,c) -> [AlertMoveCreatureVia c p])) fastActors
    where
    npcAct :: Board -> (P.Pos, Creature) -> WriterT [Alert] (Rand StdGen) Board
    npcAct bd (p,c)
        | ppos:_ <- playerPoss bd =
            (forceRand =<< lift (pathAlg c c bd p ppos)) >>= \case
                Just dir ->
                    mapWriterT (pure . runIdentity . (second alerts <$>)) $ tryMoveCreature dir p bd
                _ -> pure bd
        | otherwise = pure bd
        where
        -- Force non-calm monsters to move randomly when next to player
        forceRand :: Maybe P.Dir -> WriterT [Alert] (Rand StdGen) (Maybe P.Dir)
        forceRand md | c == CalmMonster = pure md
        forceRand (Just dir) | not (creatureCanMove bd c p dir) = randAvailableDir
        forceRand Nothing | c == SmartMonster = randAvailableDir
        forceRand md = pure md
        randAvailableDir = lift . randElem $
            filter (\d -> creatureCanMove bd c p d && creatureCanTryMove bd c p d) P.dirs

    pathAlg BasicMonster = chaseDir
    pathAlg CalmMonster  = chaseDir
    pathAlg GhostMonster = chaseDir
    pathAlg FastMonster  = chaseDir
    pathAlg SmartMonster = findPathDir
    pathAlg _            = \_ _ _ _ -> pure Nothing

    isFast FastMonster = True
    isFast _           = False

tryUseInvItem :: InvItem -> P.Dir -> Board -> WriterT [Alert] Maybe Board
tryUseInvItem e d bd
    | p:_ <- playerPoss bd = do
        tell [AlertUseItem (ItemInvItem e) p d]
        let wp = P.wposInDir p d
        lift . guard $ canUseOnWall e || (walls bd M.!? wp) `elem` [Nothing, Just BrokenWindow]
        let p' = p +^ P.dirPos d
        lift . guard $ inBounds p'
        case e of
            Cloak -> pure $ modWalls (M.insert wp $ CloakWall d 1) bd
            Umbrella charges -> do
                guard . M.notMember p' $ items bd
                let wp' = P.wposInDir p' d
                guard $ inBoundsW wp'
                guard $ (walls bd M.!? wp') `elem` [Nothing, Just BrokenWindow]
                let addUmbrellaWall = modWalls (M.insert wp' $ UmbrellaWall d)
                if p' `M.member` creatures bd
                    then do
                        let (bd', mr) = runWriter $ tryMoveCreature d p' bd
                        guard . getAny $ someAction mr
                        pure $ addUmbrellaWall bd'
                    else pure . modItems (M.insert p' . UmbrellaHandle d $ charges - 1) . addUmbrellaWall $ bd
            Balloon charges -> let inflate = modCreatures (M.insert p' (InflatedBalloon $ charges - 1))
                in if p' `M.member` creatures bd
                    then do
                        let (bd', mr) = runWriter $ tryMoveCreature d p' bd
                        guard . getAny $ someAction mr
                        pure $ inflate bd'
                    else pure $ inflate bd
            Orb -> ($ bd) <$> dropItem p' (RollingOrb d True)
            Flash ->
                let flashPoss = wallCone True p wp
                    flashWPoss = wp `S.insert` invisibleWPoss S.empty (poss S.\\ flashPoss)
                    delPoss = flip (foldr M.delete) flashPoss
                    delWPoss = flip (foldr M.delete) flashWPoss
                    delWPossS = flip (foldr S.delete) flashWPoss
                    newUnrevealed bd' = unrevealed bd' `S.difference` flashPoss
                    reveal, destroyTreasure :: Board -> Board
                    reveal = modUnrevealed (`S.difference` flashPoss) . destroyTreasure . modVisible (`S.union` flashPoss)
                    -- | Pretend we created and immediately destroyed a treasure on flashing all unrevealed
                    destroyTreasure bd'
                        | null $ newUnrevealed bd'
                        , trPos:_ <- sortOn' (negate . P.distSquared p) . S.toList $ unrevealed bd'
                        = onSpawnTreasure (treasureAt bd' trPos) bd'
                        | otherwise = bd'
                in tell [AlertHighlight flashPoss flashWPoss] $>
                    (reveal . modItems delPoss . modCreatures delPoss . modTagged delWPossS $ modWalls delWPoss bd)
            Tent -> do
                guard . M.notMember p' $ creatures bd
                pure .
                    modWalls (flip (foldr (M.alter (Just . fromMaybe TentWall))) (filter inBoundsW $ P.wposInDir p' <$> P.dirs))
                    . modCreatures (M.delete p . M.insert p' Player) $ bd
            Spraypaint _ -> do
                guard $ wp `M.member` walls bd
                pure $ modTagged (S.insert wp) bd
            Camera _ -> -- handled in Game
                pure bd
            -- _ -> ($ bd) <$> dropItem p' (ItemInvItem e)
    | otherwise = lift Nothing
    where
    dropItem p i = do
            guard . M.notMember p $ items bd
            guard . M.notMember p $ creatures bd
            pure $ modItems (M.insert p i)
    canUseOnWall Flash          = True
    canUseOnWall (Spraypaint _) = True
    canUseOnWall _              = False

doPhysics :: Board -> Writer [Alert] Board
doPhysics bd0
    | isHasteRound bd0
    = pure $ modStatuses (decayStatuses True) bd0
    | otherwise
    = modStatuses (decayStatuses False)
    . modItems decayCameras
    . decayCloaks
    . tickTimer
    <$> rollOrbs bd0
    where
    decayCloaks bd = foldr decayCloakWall bd . M.assocs
        . M.filter (\case {CloakWall _ _ -> True; _ -> False}) $ walls bd
    decayCloakWall :: (P.WPos, Wall) -> Board -> Board
    decayCloakWall (wp, CloakWall d n)
        | n > 0 = modWalls . M.insert wp $ CloakWall d (n-1)
        | otherwise = modTagged (S.delete wp)
            . modWalls (M.delete wp)
            . modItems (M.insert (P.posInDir wp d) $ ItemInvItem Cloak)
    decayCloakWall _ = id
    decayCameras = M.mapMaybe $ \case
        ItemInvItem (Camera n) | n > 0 -> Just . ItemInvItem . Camera $ n-1
            | otherwise -> Nothing
        i -> Just i
    decayStatuses hasteOnly = M.filterWithKey active . M.mapWithKey decay where
        decay Haste n = n-1
        decay _ n | hasteOnly = n
        decay _ n = n-1
        active Foresight n = n >= 0
        active _ n         = n > 0
    rollOrbs bd = foldM rollOrb bd (M.assocs $ items bd) where
        rollOrb :: Board -> (P.Pos, Item) -> Writer [Alert] Board
        rollOrb bd' (p, RollingOrb dir True)
            = pure $ modItems (M.insert p (RollingOrb dir False)) bd'
        rollOrb bd' (p, orb@(RollingOrb dir False))
            | let p' = p +^ P.dirPos dir
            , inBounds p'
            , p' `M.notMember` items bd'
            , P.wposInDir p dir `M.notMember` walls bd'
            = tell [AlertMoveItem orb p dir] $> modItems (M.delete p . M.insert p' orb) bd'
            | otherwise
            = pure $ modItems (M.insert p (ItemInvItem Orb)) bd'
        rollOrb bd' _ = pure bd'
    tickTimer = rollOver . modTimer (+1) where
        rollOver bd | timer bd >= turnsPerSide
            = modConf (BC.modRolls decSides decSides) . setTimer 0 $ bd
        rollOver bd = bd
        decSides = RF.modSides (+ (-1))
        turnsPerSide = 8

addPower :: Bool -> Board -> Board
addPower overUsable bd
    | p:_ <- playerPoss bd = modPowers (M.alter (add p) p) bd
    | otherwise = bd
    where
    add p Nothing    = Just . Pow.upgrade $ Pow.new (powerTypeAt p) overUsable
    add _ (Just pow) = Just $ Pow.upgrade pow

canUndo :: Board -> Bool
canUndo bd
    | Just c <- creatures bd M.!? undoPos
    , c `elem` [Player, DeadPlayer]
    , Just pow <- powers bd M.!? undoPos
    = Pow.activatableTimes pow > 0
    | otherwise = False
    where
    undoPos = P.Pos 2 2
