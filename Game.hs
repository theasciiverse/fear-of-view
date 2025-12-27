{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import           Prelude               hiding (round)

import           Control.Monad         (unless, when)
import           Control.Monad.Random  (Rand, StdGen, initStdGen, mkStdGen,
                                        runRand)
import           Control.Monad.State   (MonadIO, MonadState, StateT, evalStateT,
                                        get, gets, modify, put)
import           Control.Monad.Writer  (Writer, WriterT, mapWriterT, runWriter,
                                        runWriterT, tell)
import           Data.Functor          (($>))
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (getAny, getSum)

import qualified Data.IntMap.Strict    as IM
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

import           Creature
import           Equipment
import           Exit
import           Group
import           Item
import           Rand
import           Wall

import qualified Board                 as B
import qualified BoardConf             as BC
import qualified Command               as C
import qualified Highscore             as HS
import qualified Inventory             as I
import qualified Pos                   as P
import qualified Power                 as Pow
import qualified RollFrom              as RF
import qualified Tutorial              as T

data Game = Game
    { board        :: B.Board
    , transitions  :: [ B.Transition ]
    , inventory    :: I.Inventory
    , equipment    :: S.Set Equipment
    , selectedSlot :: Maybe I.Slot
    , score        :: Int
    , life         :: Int
    , maxLife      :: Int
    , junk         :: Int
    , level        :: Int
    , round        :: Int
    , roundItems   :: [Item]
    , levels       :: IM.IntMap BC.BoardConf
    , prev         :: Maybe Game
    , unseenBeats  :: S.Set T.Type
    , prevHS       :: Maybe HS.Highscore
    , gen          :: StdGen
    }

maxLevel, maxScore, initLife, charmBonus :: Int
maxLevel = 3
maxScore = 25
initLife = 5
charmBonus = 1

initCreatureSides, initWallSides :: Int
initCreatureSides = 36
initWallSides = 30

baseLevels :: IM.IntMap BC.BoardConf
baseLevels = IM.fromList
    [ (1, BC.BoardConf 1
        (RF.RollFrom initCreatureSides $ replicate 5 BasicMonster)
        (RF.RollFrom initWallSides $ replicate 6 Hedge))
    , (2, BC.BoardConf 2
        (RF.RollFrom initCreatureSides $ replicate 5 BasicMonster)
        (RF.RollFrom initWallSides $ replicate 6 BasicWall <> [Pillar]))
    , (3, BC.BoardConf 3
        (RF.RollFrom initCreatureSides $ replicate 5 BasicMonster)
        (RF.RollFrom initWallSides $ replicate 6 BasicWall <> [Window, Window]))
    ]

new :: MonadIO m => m Game
new = do
    g <- initStdGen
    pure $ Game { board = B.empty, transitions = [], inventory = I.empty, equipment = S.empty, selectedSlot = Nothing, score = 0, life = 0, maxLife = 0, junk = 0, level = 0, round = 0, roundItems = [], levels = baseLevels, prev = Nothing, unseenBeats = S.fromList T.allTypes, prevHS = Nothing, gen = g }

modBoard :: (B.Board -> B.Board) -> Game -> Game
modBoard f game = game { board = f $ board game }
setBoard :: B.Board -> Game -> Game
setBoard = modBoard . const

modScore, modLife, modMaxLife, modJunk, modLevel, modRound :: (Int -> Int) -> Game -> Game
modScore f game = game { score = f $ score game }
modLife f game = game { life = f $ life game }
modMaxLife f game = game { maxLife = f $ maxLife game }
modJunk f game = game { junk = f $ junk game }
modLevel f game = game { level = f $ level game }
modRound f game = game { round = f $ round game }
setScore, setLife, setMaxLife, setJunk, setLevel, setRound :: Int -> Game -> Game
setScore = modScore . const
setLife = modLife . const
setMaxLife = modMaxLife . const
setJunk = modJunk . const
setLevel = modLevel . const
setRound = modRound . const

setRoundItems :: [Item] -> Game -> Game
setRoundItems is game = game { roundItems = is }

modLevels :: (IM.IntMap BC.BoardConf -> IM.IntMap BC.BoardConf) -> Game -> Game
modLevels f game = game { levels = f $ levels game }

modPrev :: (Maybe Game -> Maybe Game) -> Game -> Game
modPrev f game = game { prev = f $ prev game }
setPrev :: Maybe Game -> Game -> Game
setPrev = modPrev . const

modLifeWithMax, modScoreWithMax :: (Int -> Int) -> Game -> Game
modLifeWithMax f game = modLife (min (maxLife game) . f) game
modScoreWithMax f = modScore $ min maxScore . f

modTransitions :: ([B.Transition] -> [B.Transition]) -> Game -> Game
modTransitions f game = game { transitions = f $ transitions game }
setTransitions :: [B.Transition] -> Game -> Game
setTransitions = modTransitions . const
pushTransition :: B.Transition -> Game -> Game
pushTransition = modTransitions . (:)

setSelectedSlot :: Maybe I.Slot -> Game -> Game
setSelectedSlot slot game = game { selectedSlot = slot }

modInventory :: (I.Inventory -> I.Inventory) -> Game -> Game
modInventory f game = game { inventory = f $ inventory game }
setInventory :: I.Inventory -> Game -> Game
setInventory = modInventory . const

modEquipment :: (S.Set Equipment -> S.Set Equipment) -> Game -> Game
modEquipment f game = game { equipment = f $ equipment game }
setEquipment :: S.Set Equipment -> Game -> Game
setEquipment = modEquipment . const

modUnseenBeats :: (S.Set T.Type -> S.Set T.Type) -> Game -> Game
modUnseenBeats f game = game { unseenBeats = f $ unseenBeats game }
setUnseenBeats :: S.Set T.Type -> Game -> Game
setUnseenBeats = modUnseenBeats . const

setPrevHS :: Maybe HS.Highscore -> Game -> Game
setPrevHS mhs game = game { prevHS = mhs }

hasEquip :: Equipment -> Game -> Bool
hasEquip e = (e `S.member`) . equipment

preserveSlots :: Game -> Int
preserveSlots game
    | hasEquip Bag game = 1
    | otherwise = 0

canGrab :: Game -> Bool
canGrab = hasEquip Broom

highscore :: Maybe HS.Username -> Game -> HS.Highscore
highscore name game = HS.Highscore
    { HS.score = score game
    , HS.maxRound = round game
    , HS.maxLevel = fix0 $ level game
    , HS.equipment = equipment game
    , HS.name = name
    }
    where
    fix0 0 = 3
    fix0 n = n

type T m = StateT Game m

runT :: MonadIO m => T m a -> m a
runT m = evalStateT m =<< new

nullGen :: StdGen
nullGen = mkStdGen 0

evalR :: MonadState Game m => Rand StdGen a -> m a
evalR m = do
    game <- get
    let (a,g) = runRand m $ gen game
    put $ game {gen = g}
    pure a

modifyR :: MonadState Game m => (Game -> Rand StdGen Game) -> m ()
modifyR f = do
    game <- get
    let (game',g) = runRand (f game) $ gen game
    put $ game' {gen = g}

modBoardM :: Monad m => (B.Board -> m B.Board) -> Game -> m Game
modBoardM f game =  (`setBoard` game) <$> f (board game)

initGame, nextRound :: (MonadState Game m, MonadIO m) => m ()
initGame = do
    beats <- gets unseenBeats
    hs <- gets $ highscore Nothing
    put =<< new
    let mhs = if HS.maxRound hs > 0 then Just hs else Nothing
    modify $ setPrevHS mhs . setUnseenBeats beats
nextRound = do
    modify . modInventory . I.clearAllBut =<< gets preserveSlots
    modify $ modRound (+1) . setLevel 1 . setPrev Nothing
    charmed <- gets $ hasEquip Charm
    let l = initLife + if charmed then charmBonus else 0
    modify $ setLife l . setMaxLife l
    modify . setRoundItems =<< evalR (take 3 <$> shuffle findableTreasures)
    enterNewBoard True =<< evalR (randMemberUnsafe B.boundaryWPoss)
    endTurn

enterNewBoard :: MonadState Game m => Bool -> P.WPos -> m ()
enterNewBoard doRecharge e = do
    powers <- gets $ (if doRecharge then M.map Pow.recharge else id) . B.powers . board
    bc <- gets currentLevelBC
    diffs <- evalR $ BC.genDiffs bc
    is <- gets roundItems
    modify . setBoard $ (B.enterAt e $ B.new bc) { B.powers = powers, B.diffs = diffs, B.possItems = is}
    setFov
    where
    currentLevelBC game = fromMaybe BC.emptyBoardConf . (IM.!? level game) $ levels game


setFov :: MonadState Game m => m ()
setFov = modifyR . modBoardM . B.setFov =<< gets (hasEquip Key)

addJunk :: MonadState Game m => m ()
addJunk = do
    modify (modJunk (+1))
    (gets enough >>=) . flip when $ do
        modify $ setJunk 0
        es <- gets equipment
        evalR (randMember (S.fromList allEquipment S.\\ es)) >>= \case
            Nothing -> modify $ modScoreWithMax (+1)
            Just e  -> do
                modify . modEquipment $ S.insert e
                onAdd e
    where
    enough g = junk g > S.size (equipment g) + 1
    onAdd Charm = modify $ modMaxLife (+charmBonus)
    onAdd Siphon = modify . modBoard $ B.modPowers (M.map $ Pow.setOverUsable True)
    onAdd _     = pure ()

collect :: MonadState Game m => [Item] -> m ()
collect = mapM_ collect' where
    collect' Gem                = modify . modBoard . B.addPower =<< gets (hasEquip Siphon)
    collect' ScoreTreasure           = modify $ modScoreWithMax (+1)
    collect' Potion           = modify $ modLifeWithMax (+4)
    collect' MiniPotion           = modify $ modLifeWithMax (+2)
    collect' Junk             = addJunk
    collect' (UmbrellaHandle _ 0) = pure ()
    collect' (UmbrellaHandle _ charges) = collect' (ItemInvItem $ Umbrella charges)
    collect' CameraBoxed        = modify . modInventory $ snd . I.add (Camera initCameraCharge)
    collect' (RollingOrb _ _)   = collect' (ItemInvItem Orb)
    collect' (ItemInvItem (Camera 0))      = pure ()
    collect' (ItemInvItem e)      = modify . modInventory $ snd . I.add e

clearTrans :: MonadState Game m => m ()
clearTrans = modify $ setTransitions []

modBoardTellM :: MonadState Game m => (B.Board -> WriterT a m B.Board) -> m a
modBoardTellM f = do
    (bd,a) <- runWriterT . f =<< gets board
    modify (setBoard bd) $> a
modBoardTell :: MonadState Game m => (B.Board -> Writer a B.Board) -> m a
modBoardTell = modBoardTellM . (mapWriterT (pure . runIdentity) .)

exitLevel :: MonadState Game m => P.WPos -> m ()
exitLevel exitPos = do
    modify (modLevel $ (`mod` 4) . (+1))
    lev <- gets level
    when (lev <= maxLevel) $
        enterNewBoard False $ B.oppositeWPos exitPos

doAlarm :: MonadState Game m => P.Dir -> m ()
doAlarm dir = do
    diffs <- gets $ B.diffs . board
    lev <- gets level
    modify . modLevels $ IM.adjust (BC.apply diffs dir) lev

movePlayer :: MonadState Game m => P.Dir -> m ()
movePlayer dir = do
    bd <- gets board
    mr <- modBoardTell $ B.movePlayers dir
    when (getAny . B.someAction $ mr) $ do
        modify $ modLifeWithMax (+ (-(getSum $ B.damage mr)))
        showAlerts bd $ B.alerts mr
        envAct
        mapM_ doAlarm $ B.alarmings mr
        mapM_ exitLevel $ B.exitings mr
        endTurn

showAlerts :: MonadState Game m => B.Board -> [B.Alert] -> m ()
showAlerts bd alerts = unless (null alerts) $ modify . pushTransition $ B.Transition bd alerts

useInvSlotInDir :: MonadState Game m => I.Slot -> P.Dir -> m ()
useInvSlotInDir slot dir =
    gets ((M.!? slot) . I.invItems . inventory) >>= \case
        Nothing -> (gets canGrab >>=) . flip when $ do
            is <- modBoardTell (B.grabItem dir)
            unless (null is) $ collect is >> envAct >> endTurn
        Just e -> do
            bd <- gets board
            case runWriterT $ B.tryUseInvItem e dir bd of
                Nothing -> pure ()
                Just (bd', alerts) -> do
                    showAlerts bd alerts
                    modify $ setBoard bd'
                    modify . modInventory . I.modInvItems $ useUp slot
                    envAct
                    endTurn
    where
    useUp :: I.Slot -> M.Map I.Slot InvItem -> M.Map I.Slot InvItem
    useUp s inv
        | Just (Spraypaint n) <- inv M.!? s
            , n > 1 = M.insert s (Spraypaint $ n-1) inv
        | otherwise = M.delete s inv


envAct :: MonadState Game m => m ()
envAct = do
    bd <- gets board
    modBoardTellM (mapWriterT evalR . B.npcsAct) >>= showAlerts bd
    bd' <- gets board
    modBoardTell B.doPhysics >>= showAlerts bd'
    collect =<< modBoardTell B.collectItems
    setFov
    -- | collect again, in case vision spawned something on our loc
    collect =<< modBoardTell B.collectItems

undoTurns :: Int
undoTurns = 5

endTurn :: MonadState Game m => m ()
endTurn = do
    undoCharges <- gets $ maybe 0 Pow.activatableTimes . (M.!? undoPos) . B.powers . board
    modify . eraseBefore $ undoTurns * undoCharges
    modify . setPrev . Just =<< get
    gets ((<=0) . life) >>= flip when (modify $ modBoard B.setPlayerDead)
    where
    eraseBefore n | n <= 0 = setPrev Nothing
    eraseBefore n = modPrev (eraseBefore (n-1) <$>)
    undoPos = P.Pos 2 2

powerOn :: Game -> Maybe Pow.Power
powerOn game | let bd = board game, p:_ <- B.playerPoss bd = B.powers bd M.!? p
powerOn _ = Nothing

activatePower :: MonadState Game m => m ()
activatePower = do
    bd <- gets board
    case True of
        _ | p:_ <- B.playerPoss bd
          , Just pow <- B.powers bd M.!? p
          , Pow.activatable pow
          -> do
              when (Pow.tp pow == Pow.Foresight) . modifyR $ modBoardM B.beginExpect
              -- |XXX: Be careful of the subtle interaction with the undo power
              -- when changing this.
              modify $ modBoard (B.modPowers $ M.alter (Pow.deplete =<<) p) . doPower (Pow.tp pow) p
              unless (Pow.tp pow == Pow.Undo) envAct
              endTurn
        _ -> pure ()
    where
    doPower Pow.Heal _ = modLifeWithMax (+2)
    doPower Pow.Smoke _ = modBoard $ B.incStatus B.Smoke (B.maxSmoke + 1)
    doPower Pow.Haste _ = modBoard $ B.incStatus B.Haste 8
    doPower Pow.Dazzle _ = modBoard $ B.incStatus B.Dazzled 5
    doPower Pow.Ghost _ = modBoard $ B.incStatus B.Ghost 6
    doPower Pow.Foresight _ = modBoard $ B.incStatus B.Foresight 10
    doPower Pow.Teleport p = modBoard $ B.setSafe True . B.modCreatures (M.delete p . M.insert (centre +^ centre +^ neg p) Player)
    doPower Pow.Undo _ = \game -> let (game',trans) = runWriter $ prevTurn undoTurns game in game' { transitions = reverse trans } where
        prevTurn :: Int -> Game -> Writer [B.Transition] Game
        prevTurn n g | n < 0 = pure g
        prevTurn n g = maybe (pure g) ((tell [B.Transition (board g) []] >>) . prevTurn (n-1)) $ prev g
    centre = P.Pos 2 2

data PlayState = Playing | Dead | RoundEnded | Won | Tutorialising T.Beat deriving Eq
playState :: Game -> PlayState
playState game
    | level game == 0 = RoundEnded
    | life game <= 0 = Dead
    | score game >= maxScore = Won
    | Just b <- S.lookupMin $ triggeredBeats game = Tutorialising b
    | otherwise = Playing

doCommand :: (MonadState Game m, MonadIO m) => C.Command -> m ()
doCommand c = gets playState >>= \case
    Dead -> case c of
        C.Accept -> initGame
        _        -> pure ()
    Won -> case c of
        C.Accept -> initGame
        _        -> pure ()
    RoundEnded -> case c of
        C.Accept -> nextRound
        C.SkipTutorial -> modify (setUnseenBeats $ S.fromList T.allTypes) >> nextRound
        _        -> pure ()
    Tutorialising b -> case c of
        C.Accept       -> modify . modUnseenBeats . S.delete $ T.tp b
        C.SkipTutorial -> modify $ setUnseenBeats S.empty
        _              -> pure ()
    Playing -> case c of
        C.Dir dir -> do
            gets selectedSlot >>= \case
                Nothing -> movePlayer dir
                Just slot -> do
                    modify $ setSelectedSlot Nothing
                    useInvSlotInDir slot dir
        C.UseInv slot -> do
            gets selectedSlot >>= modify . \case
                Just s | s == slot -> setSelectedSlot Nothing
                Just s' -> modInventory (I.swap slot s') . setSelectedSlot Nothing
                Nothing -> setSelectedSlot $ Just slot
        C.UsePower -> activatePower
#ifdef DEBUG
        C.DebugAddPower -> modify . modBoard . B.addPower =<< gets (hasEquip Siphon)
        C.DebugAddJunk -> addJunk
        C.DebugAddItems -> collect findableTreasures
        C.DebugExit -> exitLevel =<< evalR (randElemUnsafe $ S.toList B.boundaryWPoss)
#endif
        _ -> pure ()

triggeredBeats :: Game -> S.Set T.Beat
triggeredBeats game = S.unions . S.map triggered $ unseenBeats game where
    triggered T.TMeta = S.singleton T.Meta
    triggered T.TMeta2 = S.singleton T.Meta2
    triggered T.TMovement | p:_ <- B.playerPoss bd = S.singleton $ T.Movement p
    triggered T.TTrapped | p:_ <- B.playerPoss bd
        , all (trappedDir p) P.dirs
        , any ((`M.member` B.walls bd) . P.wposInDir p) P.dirs
        = S.singleton $ T.Trapped p
    triggered T.TSeeMonster = uncurry T.SeeMonster `S.map` (S.fromList . M.assocs . M.filter isMonster $ B.creatures bd)
    triggered T.TSeeExit = T.SeeExit `S.map` M.keysSet (M.filter (== Exit True) $ B.exits bd)
    triggered T.TSeeItem = uncurry T.SeeItem `S.map` (S.fromList . M.assocs . M.filter (`notElem` [Potion, MiniPotion, ScoreTreasure, Junk, Gem]) $ B.items bd)
    triggered T.TSeePotion = T.SeePotion `S.map` M.keysSet (M.filter (== Potion) $ B.items bd)
    triggered T.TSeeMiniPotion = T.SeeMiniPotion `S.map` M.keysSet (M.filter (== MiniPotion) $ B.items bd)
    triggered T.TSeeScore = T.SeeScore `S.map` M.keysSet (M.filter (== ScoreTreasure) $ B.items bd)
    triggered T.TSeeJunk = T.SeeJunk `S.map` M.keysSet (M.filter (== Junk) $ B.items bd)
    triggered T.TSeeGem = T.SeeGem `S.map` M.keysSet (M.filter (== Gem) $ B.items bd)
    triggered T.THurt | life game < maxLife game = sng $ T.Hurt (life game) (maxLife game)
    triggered T.TCollectItem | 1 `M.member` I.invItems (inventory game) = sng T.CollectItem
    triggered T.TCollectItem2 | 1 `M.member` I.invItems (inventory game) = sng T.CollectItem2
    triggered T.TCollectGem | p:_ <- B.playerPoss bd, p `M.member` B.powers bd = sng T.CollectGem
    triggered T.TCollectScore | score game == 1 = sng T.CollectScore
    triggered T.TTimer | (RF.sides . BC.creatureRoll $ B.conf bd) < initCreatureSides - 2 = sng T.Timer
    triggered T.TMustBreak | p:_ <- B.playerPoss bd
        , any ((== Just (Exit True)) . (B.exits bd M.!?) . P.wposInDir p) P.dirs
        = S.singleton T.MustBreak
    triggered T.TTutEnd | S.size (unseenBeats game) == 1 = sng T.TutEnd
    triggered _ = S.empty
    trappedDir p dir = or [ not $ B.inBounds p'
        , p' `M.member` B.creatures bd
        , P.wposInDir p dir `M.member` B.walls bd ]
        where p' = p +^ P.dirPos dir
    sng = S.singleton
    bd = board game
