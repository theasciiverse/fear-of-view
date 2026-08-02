{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module TermDraw where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forM, forM_, mplus, unless, void, when)
import           Control.Monad.State (get, lift, liftIO, put, runStateT)
import           Data.Bifunctor      (bimap)
import           Data.List           (intersperse, (\\))
import           Data.Maybe          (fromMaybe, isJust, listToMaybe,
                                      maybeToList)
import           Safe                (atMay)

#if !MIN_VERSION_base(4,20,0)
-- foldl' started being exported from prelude in base-4.20.0
import           Data.Foldable       (foldl')
#endif

import qualified Data.Map.Strict     as M
import qualified Data.Set            as S

import           Creature
import           CStyle
import           Equipment
import           Exit
import           Geometry
import           Group
import           Item
import           Wall
import           Window

import qualified Board               as B
import qualified BoardConf           as BC
import qualified CPos                as CP
import qualified Fov                 as F
import qualified Game                as G
import qualified Highscore           as HS
import qualified HighscoreFile       as HSF
import qualified Inventory           as I
import qualified Pos                 as P
import qualified Power               as Pow
import qualified RollFrom            as RF
import qualified TermM               as TM
import qualified Tutorial            as T

invItemGlyph :: InvItem -> Glyph
invItemGlyph e = Glyph c $ CStyle cyan True where
    c = case e of
        Orb          -> 'o'
        Cloak        -> '['
        Umbrella _   -> '/'
        Balloon _    -> '&'
        Flash        -> '='
        Camera _     -> ')'
        Tent         -> 'A'
        Spraypaint _ -> ':'

drawStyledStrs :: TM.TermM m => Window -> CP.CPos -> [(String, CStyle)] -> m ()
drawStyledStrs win (CP.CPos x0 y) = void . (`runStateT` x0) . mapM draw where
    draw (s,style) = do
        x <- get
        lift $ TM.drawStr win style (CP.CPos x y) s
        put $ x + length s

twoCharNum :: Int -> String
twoCharNum n
    | 0 <= n && n < 10 = ' ' : show n
    | otherwise = take 2 $ show n

-- |Apply style only to the number, not spacing
twoCharNumStyled :: Int -> CStyle -> [(String, CStyle)]
twoCharNumStyled n style
    | 0 <= n && n < 10 = [(" ", style0), (show n, style)]
    | otherwise = [(take 2 $ show n, style)]

drawStatus :: TM.TermM m => G.Game -> m ()
drawStatus (G.Game { G.maxLife = maxLife, G.score = score, G.level = level, G.round = rnd, G.junk = junk, G.equipment = equipment, G.board = bd }) = do
    let win = StatusWin
    unless (M.null statuses) $ drawStyledStrs win (CP.CPos 0 0) statusStrs
    drawStyledStrs win (CP.CPos 0 1) numStrs
    where
    statuses = B.statuses bd
    life = B.life bd
    -- XXX: keep in sync with magic numbers in highlightTut
    numStrs =
        [ ("Life: ", style0)
        ] <> twoCharNumStyled life (lifeStyle life) <>
        [ ("/", style0)
        , (show maxLife, if maxLife == G.initLife then style0 else equipStyle Charm)
        , ("   Score: ", style0)
        , (twoCharNum score <> "/" <> show G.maxScore, style0)
        , ("~", scoreStyle)
        , ("   Level: " <> twoCharNum rnd <> ":", style0)
        , (showLevel level, levelStyle level)
        ] <>
        [ ("   Junk: ", style0)
        , (twoCharNum junk <> "/" <> show (S.size equipment + 1), style0)
        , ("%", junkStyle)
        ]
    statusStrs = intersperse ("   ", style0)
        [ (show stat <> " " <> twoCharNum n, statStyle status)
        | status@(stat,n) <- M.assocs statuses
        , n > 0 ]
    statStyle (B.Dazzled,_) = CStyle cyan True
    statStyle (B.Ghost,_) = styleBold
    statStyle (B.Smoke,_) = CStyle blue True
    statStyle (B.Haste,n) = CStyle (if n `mod` 2 == 1 then red else yellow) True
    statStyle (B.Foresight,_) = CStyle yellow False

exitChar :: Bool -> P.Dir -> Char
exitChar True = const 'x'
exitChar False = \case
    P.DUp    -> '^'
    P.DDown  -> 'v'
    P.DRight -> '>'
    P.DLeft  -> '<'

drawBoard :: TM.TermM m => B.Board -> m ()
drawBoard = drawAlertedBoard . baseAlerted

data AlertedBoard = AlertedBoard
    { base          :: B.Board
    , creatureMoves :: M.Map P.WPos Creature
    , itemMoves     :: M.Map P.WPos Item
    , itemUses      :: M.Map P.WPos Item
    , highlightPs   :: S.Set P.Pos
    , highlightWPs  :: S.Set P.WPos
    }
baseAlerted :: B.Board -> AlertedBoard
baseAlerted bd = AlertedBoard bd M.empty M.empty M.empty S.empty S.empty

drawTrans :: TM.TermM m => B.Transition -> m ()
drawTrans (B.Transition bd0 alerts) =
    let alerted = foldl' applyAlert (baseAlerted bd0) alerts
    in do
        drawAlertedBoard alerted
        TM.wRefresh BoardWin
        TM.wErase BoardWin
        liftIO (threadDelay 50000)
    where
    applyAlert alerted (B.AlertMoveCreature c p d) =
        alerted { creatureMoves = M.insert (P.wposInDir p d) c $ creatureMoves alerted
        , base = B.modCreatures (M.delete p) $ base alerted
        }
    applyAlert alerted (B.AlertMoveCreatureVia c p) =
        alerted { base = B.modCreatures (M.insert p c) $ base alerted
        }
    applyAlert alerted (B.AlertMoveItem i p d) =
        alerted { itemMoves = M.insert (P.wposInDir p d) i $ itemMoves alerted
        , base = B.modItems (M.delete p) $ base alerted
        }
    applyAlert alerted (B.AlertUseItem i p d) =
        alerted { itemUses = M.insert (P.wposInDir p d) i $ itemUses alerted
        , base = B.modItems (M.delete p) $ base alerted
        }
    applyAlert alerted (B.AlertHighlight ps wps) =
        alerted { highlightPs = ps `S.union` highlightPs alerted
        , highlightWPs = wps `S.union` highlightWPs alerted
        }

char,bold,dim,boldDim :: Char -> Glyph
char c = Glyph c style0
bold c = Glyph c styleBold
dim c = Glyph c $ CStyle (onBlue white) False
boldDim c = Glyph c $ CStyle (onBlue white) True

levelStyle :: Int -> CStyle
levelStyle 1 = CStyle green False
levelStyle 2 = CStyle white True
levelStyle 3 = CStyle yellow True
levelStyle _ = style0

showLevel :: Int -> String
showLevel l | l > 0 = (['A'..] !! (l-1)):""
showLevel _ = "-"

scoreStyle, junkStyle :: CStyle
scoreStyle = CStyle yellow True
junkStyle = CStyle blue True

lifeStyle :: Int -> CStyle
lifeStyle = \case
    n | n <= 0          -> CStyle (onRed black) True
    1                   -> CStyle red True
    2                   -> CStyle red False
    3                   -> CStyle yellow True
    4                   -> CStyle yellow False
    n | n <= G.initLife -> style0
    _                   -> CStyle green False

creatureGlyph :: Creature -> Glyph
creatureGlyph (Player life)             = Glyph '@' $ lifeStyle life
creatureGlyph BasicMonster              = Glyph 'm' $ CStyle yellow False
creatureGlyph (CalmMonster False)       = Glyph 'p' $ CStyle blue True
creatureGlyph (CalmMonster True)        = Glyph 'P' $ CStyle blue True
creatureGlyph (GhostMonster False)      = Glyph 'g' $ CStyle white True
creatureGlyph (GhostMonster True)       = Glyph 'G' $ CStyle white True
creatureGlyph (SmartMonster False)      = Glyph 's' $ CStyle red True
creatureGlyph (SmartMonster True)       = Glyph 'S' $ CStyle cyan True
creatureGlyph (FastMonster False)       = Glyph 'f' $ CStyle magenta True
creatureGlyph (FastMonster True)        = Glyph 'F' $ CStyle magenta True
creatureGlyph (InflatedBalloon charges) = Glyph '0' . CStyle magenta $ charges > 0

itemGlyph :: Item -> Glyph
itemGlyph (Gem pow)             = Glyph '*' $ powerStyle pow
itemGlyph Potion                = Glyph '!' $ CStyle green True
itemGlyph ScoreTreasure         = Glyph '~' scoreStyle
itemGlyph Junk                  = Glyph '%' junkStyle
itemGlyph (UmbrellaHandle d ch) = Glyph (if d `elem` [P.DUp, P.DDown] then '|' else '-') $ CStyle magenta (ch > 0)
itemGlyph CameraBoxed           = Glyph ')' $ CStyle cyan False
itemGlyph (RollingOrb _ _)      = Glyph 'o' $ CStyle cyan False
itemGlyph (ItemInvItem e)       = invItemGlyph e

powerGlyph :: Pow.Power -> Glyph
powerGlyph pow = case Pow.charges pow of
    0 | Pow.overUsable pow -> Glyph ',' . powerStyle $ Pow.tp pow
    0                      -> Glyph ',' style0
    _                      -> Glyph '"' $ powerStyle $ Pow.tp pow

powerStyle :: Pow.PowerType -> CStyle
powerStyle Pow.Heal      = CStyle green True
powerStyle Pow.Dazzle    = CStyle cyan True
powerStyle Pow.Smoke     = CStyle blue True
powerStyle Pow.Haste     = CStyle red True
powerStyle Pow.Teleport  = CStyle green False
powerStyle Pow.Undo      = CStyle magenta True
powerStyle Pow.Ghost     = CStyle white True
powerStyle Pow.Foresight = CStyle yellow False

wallGlyphVert :: Wall -> Glyph
wallGlyphVert = \case
    BasicWall -> bold wallchar
    Pillar -> char '|'
    Hedge -> Glyph wallchar $ CStyle green True
    ThickHedge -> Glyph '║' $ CStyle green False
    Window -> Glyph wallchar $ CStyle cyan False
    BrokenWindow -> Glyph ';' $ CStyle cyan False
    (CloakWall _ _) -> Glyph wallchar $ CStyle cyan True
    (UmbrellaWall d) -> Glyph (if d == P.DRight then '>' else '<') $ CStyle magenta True
    TentWall -> Glyph wallchar $ CStyle red True
    where wallchar = '│'

wallGlyphHoriz :: Wall -> (Glyph,Glyph)
wallGlyphHoriz = \case
    Pillar               -> (char '-', char '-')
    (UmbrellaWall P.DUp) -> (umb '/', umb '\\')
    (UmbrellaWall _)     -> (umb '\\', umb '/')
    BasicWall            -> doublet $ bold wallchar
    Hedge                -> doublet . Glyph wallchar $ CStyle green True
    ThickHedge           -> doublet . Glyph '═' $ CStyle green False
    Window               -> doublet . Glyph wallchar $ CStyle cyan False
    BrokenWindow         -> (Glyph '.' $ CStyle cyan False , Glyph ',' $ CStyle cyan False)
    (CloakWall _ _)      -> doublet . Glyph wallchar $ CStyle cyan True
    TentWall -> doublet . Glyph wallchar $ CStyle red True
    where
    wallchar = '─'
    umb c = Glyph c $ CStyle magenta True
    doublet gl = (gl,gl)

exitGlyphVert :: CStyle -> P.WPos -> Exit -> Glyph
exitGlyphVert st wp = \case
    Exit locked    -> bold $ exitChar locked (P.exitDir wp)
    KeyExit locked -> Glyph (exitChar locked (P.exitDir wp)) $ equipStyle Key
    Entrance       -> exitGlyphVert st wp SeenBoundary
    UnseenBoundary -> char '·'
    SeenBoundary   -> Glyph '│' st

exitGlyphHoriz :: CStyle -> P.WPos -> Exit -> (Glyph,Glyph)
exitGlyphHoriz st wp = \case
    Exit locked    -> (bold $ exitChar locked (P.exitDir wp), Glyph '─' st)
    KeyExit locked -> (Glyph (exitChar locked (P.exitDir wp)) $ equipStyle Key, Glyph '─' st)
    Entrance       -> exitGlyphHoriz st wp SeenBoundary
    UnseenBoundary -> doublet $ char '·'
    SeenBoundary   -> doublet $ Glyph '─' st
    where doublet gl = (gl,gl)

-- positions in board window
posCPosL, posCPosR, posIntCPos :: P.Pos -> CP.CPos
posCPosL (P.Pos x y) = CP.CPos (3*(x+1) - 2) $ 2*(B.h - y) - 1
posCPosR = (CP.CPos 1 0 <>) . posCPosL
posIntCPos p = posCPosL p <> CP.CPos 2 (-1)
wposCPos, wposCPosR :: P.WPos -> CP.CPos
wposCPos (P.WPos p up) = posCPosL p <> if up then CP.CPos 0 (-1) else CP.CPos 2 0
wposCPosR = (CP.CPos 1 0 <>) . wposCPos

drawAlertedBoard :: TM.TermM m => AlertedBoard -> m ()
drawAlertedBoard AlertedBoard{ base = bd, creatureMoves = cmvs, itemMoves = imvs, itemUses = iuses, highlightPs = hps, highlightWPs = hwps } =
    let wallsV :: M.Map P.WPos Glyph
        wallsH :: M.Map P.WPos (Maybe Glyph,Maybe Glyph)
        wallsV = highlightV `M.union` mvingV `M.union` wObscuredV `M.union` bdWallsV `M.union` borderV `M.union` bgWallsV
        wallsH = highlightH `M.union` mvingH `M.union` wObscuredH `M.union` bdWallsH `M.union` borderH `M.union` bgWallsH
        cells, items, creatures, obscured, powers :: M.Map P.Pos (Maybe Glyph,Maybe Glyph)
        layer = M.unionWith $ \(l,r) (l',r') -> (l `mplus` l', r `mplus` r')
        cells = highlightCells `layer` obscured `layer` creatures `layer` items `layer` powers
        filterV = M.filterWithKey $ const . not . P.up
        filterH = M.filterWithKey $ const . P.up
        filterSV = S.filter $ not . P.up
        filterSH = S.filter P.up
        mapH :: (Glyph -> a) -> (Glyph,Glyph) -> (a,a)
        mapH f = bimap f f
        glyphsV f m = M.mapWithKey f $ filterV m
        glyphsH f m = M.mapWithKey ((mapH Just .) . f) $ filterH m
        borderV = glyphsV (\wp -> markObscure wp . exitGlyphVert levBoundSt wp) $ B.exits bd
        borderH = glyphsH (\wp -> mapH (markObscure wp) . exitGlyphHoriz levBoundSt wp) $ B.exits bd
        wpFov = F.wpFov $ B.visible bd
        pFov = F.pFov $ B.visible bd
        markObscure wp
            | wp `S.member` wpFov = id
            | otherwise = modColour onBlue
        bdWallsV = glyphsV (const wallGlyphVert) $ B.walls bd
        bdWallsH = glyphsH (const wallGlyphHoriz) $ B.walls bd
        -- |black on black background grid, ignored by CursesUI
        bgWallsV = M.fromSet (const . Glyph '│' $ CStyle black False) . filterSV $ B.wPossIncBdd
        bgWallsH = M.fromSet (const . biGlyph . Glyph '─' $ CStyle black False) . filterSH $ B.wPossIncBdd
        levBoundSt = levelStyle . BC.level $ B.conf bd
        items = M.mapWithKey itemGlyphs $ B.items bd
        itemGlyphs p i = (Nothing,) . Just . powerBG p $ itemGlyph i
        biGlyph gl = (Just gl, Just gl)
        powerBG :: P.Pos -> Glyph -> Glyph
        powerBG p
            | Just pow <- B.powers bd M.!? p, Pow.charges pow > 0 = modColour onMagenta
            -- | Just pow <- B.powers bd M.!? p, Pow.overUsable pow = modColour onYellow
            | otherwise = id
        powers = (Nothing,) . Just . powerGlyph <$> B.powers bd
        creatures = (,Nothing) . Just . creatureGlyph' <$> B.creatures bd
        creatureGlyph' (Player life) = Glyph '@' $ (lifeStyle life) { cstyleBold = not $ B.ghostly bd }
        creatureGlyph' c = creatureGlyph c
        obscured = M.fromSet (\p -> (Just . dim $ expectChar p,) . Just . powerBG p . obsStyle $ obsChar p) $ B.poss S.\\ pFov where
            expectChar p
                | B.preexpectant bd
                , Just (Just c) <- B.expected bd M.!? p = glyphChar $ creatureGlyph' c
                | otherwise = ' '
            obsStyle
                | S.size (B.unrevealed bd) == 1 = boldDim
                | otherwise = dim
            obsChar p
                | p `S.member` B.unrevealed bd = expectedTreasureChar p
                | otherwise = ' '
            expectedTreasureChar = glyphChar . itemGlyph . B.treasureAt bd
        wObscuredV = M.fromSet (const $ dim ' ') . filterSV $ obscuredWPoss
        wObscuredH = M.fromSet (const . biGlyph $ dim ' ') . filterSH $ obscuredWPoss
        obscuredWPoss = B.invisibleWPoss (B.tagged bd) (B.visible bd)
        mvingV = M.map creatureGlyph' (filterV cmvs) `M.union` M.map itemGlyph (filterV $ imvs `M.union` iuses)
        mvingH = M.map ((,Nothing) . Just . creatureGlyph') (filterH cmvs)
            `M.union` M.map ((Nothing,) . Just . itemGlyph) (filterH imvs)
            `M.union` M.map ((,Nothing) . Just . itemGlyph) (filterH iuses)
        highlightCells = M.fromSet (const . biGlyph $ bold '#') hps
        highlightV = M.fromSet (const $ bold '#') $ filterSV hwps
        highlightH = M.fromSet (const . biGlyph $ bold '#') $ filterSH hwps

        -- Positions with wall-intersection to top-right
        intersections :: M.Map P.Pos CompositeGlyph
        intersections = iHighlighted `M.union` iWalls `M.union` iObscured
            where
            iWalls = M.mapMaybeWithKey obscureI . M.unionsWith S.union $ M.map mk . M.fromList <$>
                [ [ (p, (d,s)), (p <> if up then P.Pos (-1) 0 else P.Pos 0 (-1),(P.negDir d,s)) ]
                | (P.WPos p up, Just s) <- M.toList $
                    M.map wallI (B.walls bd) `M.union` M.map exitCol (B.exits bd)
                , let d = if up then P.DRight else P.DUp
                ]
                where
                mk :: (P.Dir, CStyle) -> CompositeGlyph
                mk (d,s) = S.singleton $ Glyph (mk' d) s
                    where
                    mk' P.DUp    = '╷'
                    mk' P.DRight = '╴'
                    mk' P.DDown  = '╵'
                    mk' P.DLeft  = '╶'
                wallI BasicWall        = Just $ CStyle white True
                wallI Hedge            = Just $ CStyle green True
                wallI ThickHedge       = Just $ CStyle green False
                wallI Pillar           = Nothing
                wallI Window           = Nothing
                wallI BrokenWindow     = Nothing
                wallI (UmbrellaWall _) = Nothing
                wallI (CloakWall _ _)  = Just $ CStyle cyan True
                wallI TentWall         = Just $ CStyle red True
                exitCol UnseenBoundary = Nothing
                exitCol _              = Just levBoundSt
                obscureI :: P.Pos -> CompositeGlyph -> Maybe CompositeGlyph
                obscureI p cg
                    | p `S.member` M.keysSet iObscured = if isBdI p
                        then Just $ modColour onBlue `S.map` cg
                        else Nothing
                    | otherwise = Just cg
            iObscured = M.fromSet (S.singleton . dim . (\p -> if isBdI p then '·' else ' '))
                . (`S.filter` iPoss) $ \p ->
                    2 > (length . filter (`S.member` wpFov) $ intAdjWps p)
            iHighlighted = M.fromSet (const . S.singleton $ bold '#') . (`S.filter` iPoss) $ \p ->
                (if isCornerI p then 1 else 2) < (length . filter (`S.member` hwps) $ intAdjWps p)
            iPoss = S.fromList [ P.Pos x y | x <- [-1..B.w-1], y <- [-1..B.h-1] ]
            isBdI (P.Pos x y) = x == -1 || x == B.w-1 || y == -1 || y == B.h-1
            isCornerI (P.Pos x y) = (x,y) `elem` [(-1,-1),(-1,B.h-1),(B.w-1,B.h-1),(B.w-1,-1)]
            intAdjWps p = [P.WPos p True, P.WPos p False, P.WPos (p +^ P.dirPos P.DRight) True, P.WPos (p +^ P.dirPos P.DUp) False]

        horizCPosMap :: M.Map P.Pos (Maybe Glyph, Maybe Glyph) -> M.Map CP.CPos Glyph
        horizCPosMap m = M.mapMaybe fst (M.mapKeys posCPosL m) `M.union` M.mapMaybe snd (M.mapKeys posCPosR m)
        horizWCPosMap :: M.Map P.WPos (Maybe Glyph, Maybe Glyph) -> M.Map CP.CPos Glyph
        horizWCPosMap m = M.mapMaybe fst (M.mapKeys wposCPos m) `M.union` M.mapMaybe snd (M.mapKeys wposCPosR m)

        glyphs :: M.Map CP.CPos Glyph
        glyphs = M.unions
            [ horizCPosMap cells
            , M.mapKeys wposCPos wallsV
            , horizWCPosMap wallsH
            ]
        compGlyphs :: M.Map CP.CPos CompositeGlyph
        compGlyphs = M.mapKeys posIntCPos intersections
    in do
        sequence_ $ M.mapWithKey (TM.drawGlyph BoardWin) glyphs
        sequence_ $ M.mapWithKey (TM.drawCompositeGlyph BoardWin) compGlyphs

drawInv :: TM.TermM m => Maybe I.Slot -> Int -> Bool -> I.Inventory -> Maybe Pow.Power -> m ()
drawInv sel preserve hasHook (I.Inventory inv) pow = do
    let win = InvWin
    let str x y st = TM.drawStr win st (CP.CPos x y)
    let firstEmpty = listToMaybe $ I.slots \\ M.keys inv
    str 0 0 styleBold "Inventory:"
    sequence_ $
        [ do
            str 0 slot style (show slot)
            case me of
                Nothing -> when hookSlot $ str 4 slot style "[Hook]"
                Just e -> do
                    TM.drawGlyph win (CP.CPos 2 slot) (invItemGlyph e)
                    str 4 slot style $ show e
        | slot <- I.slots
        , let me = inv M.!? slot
              hookSlot = hasHook && Just slot == firstEmpty
              style = CStyle col b where
                col | slot <= preserve = yellow
                    | hookSlot = cstyleCol $ equipStyle Hook
                    | otherwise = white
                b = sel == Just slot
        ] <>
        [ str 0 y style powStr >> bang
        | p@(Pow.Power tp charges maxCharges overUsable) <- maybeToList pow
        , let Glyph c style = powerGlyph p
        , let y = length I.slots + 2
        , let powStr = "0 " <> [c] <> " " <> show tp <> " " <> show charges <> "/" <> show maxCharges
        , let bang | overUsable && charges == 0 = str (length powStr) y (equipStyle Siphon) "!"
                | otherwise = pure ()
        ]

equipStyle :: Equipment -> CStyle
equipStyle Bag    = CStyle yellow False
equipStyle Charm  = CStyle green True
equipStyle Hook   = CStyle red False
equipStyle Key    = CStyle blue True
equipStyle Siphon = CStyle yellow True

drawEquip :: TM.TermM m => S.Set Equipment -> m ()
drawEquip es | S.null es = pure ()
drawEquip es = do
    let win = EquipWin
    let str x y st = TM.drawStr win st (CP.CPos x y)
    str 0 0 styleBold "Equipment:"
    forM_ (zip [1..] (S.toList es)) $ \(y,e) ->
        str 0 y (equipStyle e) $ show e

drawMessage :: TM.TermM m => G.Game -> m ()
drawMessage game = do
    let win = MessageWin
        (text,style) = case G.playState game of
            G.Dead            -> ("You died with " <> show (G.score game) <>
                " points on level " <> show (G.round game) <>
                ":" <> showLevel (G.level game) <> ".  [Space]", styleBold)
            G.Won             -> ("Congratulations, you win!  [Space]", styleBold)
            G.Tutorialising b -> (T.text b <> "  [Spc/T]", CStyle magenta True)
            _                 -> ("", style0)
    TM.drawStr win style (CP.CPos 0 0) text

delSideGlyph :: Glyph
delSideGlyph = Glyph 'x' $ CStyle red False

drawLevelInfo :: TM.TermM m => B.Board -> m ()
drawLevelInfo bd = do
    drawRoll 0 (BC.creatureRoll bc) G.initCreatureSides creatureGlyph
    drawRoll 1 (BC.wallRoll bc) G.initWallSides wallGlyphVert
    drawDiffsLine 2
    drawFound
    where
    win = LevelInfoWin
    nullGlyph = char '-'
    drawRoll :: TM.TermM m => Int -> RF.RollFrom a -> Int -> (a -> Glyph) -> m ()
    drawRoll m roll initSides f = sequence_ $
        [ TM.drawGlyph win (CP.CPos n m) $ maybe nullGlyph f (RF.vals roll `atMay` n)
        | n <- [0 .. RF.sides roll - 1] ]
        <> [ TM.drawGlyph win (CP.CPos n m) delSideGlyph | n <- [RF.sides roll .. initSides - 1] ]
    bc = B.conf bd
    diffs = B.diffs bd
    drawDiffsLine y
        | null possibleExitDirs = pure ()
        | otherwise = TM.drawStr win style0 (CP.CPos 0 y) introStr >> sequence_
        [ do
            draw b . char' $ exitChar False dir
            draw (b+1) (char ':')
            forM (zip [0..] rdfs) (\(x,rdf) -> draw (b+3+x) $ rdfGlyph rdf)
        | (n,dir) <- zip [0..] P.dirs
        , dir `elem` possibleExitDirs
        , let b = length introStr + 8*n
        , let char'
                | dir `elem` exitDirs = bold
                | dir `elem` keyExitDirs = \c -> Glyph c $ equipStyle Key
                | otherwise = char
        , Just rdfs <- [diffs M.!? dir]
        ]
        where
        introStr = "Seal:  "
        draw x = TM.drawGlyph win (CP.CPos x y)
        rdfGlyph (BC.Add (BC.DiffableCreature c))    = creatureGlyph c
        rdfGlyph (BC.Swap _ (BC.DiffableCreature c)) = creatureGlyph c
        rdfGlyph (BC.Add (BC.DiffableWall wl))       = wallGlyphVert wl
        rdfGlyph (BC.Swap _ (BC.DiffableWall wl))    = wallGlyphVert wl
        exitsWith ex = P.exitDir <$> M.keys (M.filter (== ex) (B.exits bd))
        exitDirs = exitsWith (Exit True)
        keyExitDirs = exitsWith (KeyExit True)
        possibleExitDirs = exitDirs <> keyExitDirs <> exitsWith UnseenBoundary
    drawFound = sequence_
        [ TM.drawGlyph win (CP.CPos (G.initWallSides + 4 + n) 1) gl
        | n <- [0..B.treasuresPerBoard-1]
        , let gl = maybe ((if n == B.treasuresPerBoard - 1 then bold else char) '?')
                itemGlyph $ B.found bd `atMay` n
        ]

data HSInfo
    = HSRank Int
    | HSAlive
    | HSDead
    | HSWon

drawMainScreen :: TM.TermM m => G.Game -> m ()
drawMainScreen game = do
    let win = MainWin
        curHs = twiddle $ G.highscore Nothing game where
            twiddle hs = hs { HS.maxLevel = 0, HS.maxRound = HS.maxRound hs + 1 }
        centre style y s
            | y >= scrH = pure ()
            | otherwise = TM.drawStr win style (CP.CPos ((scrW - length s) `div` 2) y) s
        centreStyled y ss
            | y >= scrH = pure ()
            | otherwise = drawStyledStrs win (CP.CPos ((scrW - sum (length . fst <$> ss)) `div` 2) y) ss
        drawTitle = sequence_ [ drawStyledStrs win (CP.CPos titleX y) s
            | (y,s) <-
                [ (0, [ ("┌────────┐", bdSt), ("     ", bgSt) ])
                , (1, [ ("│", bdSt), (" Fe@r of", styleBold)
                    , ("│", bdSt), ("View ", bgSt) ])
                , (2, [ ("└───  ───┘", bdSt), ("     ", bgSt) ])
                ]
            ] where
            titleX = (scrW - length "| Fe@r of|View") `div` 2
            bdSt = CStyle yellow True
            bgSt = CStyle (onBlue white) True
        drawVersion = TM.drawStr win style0 (CP.CPos 0 $ scrH-1) $ "v" <> CURRENT_PACKAGE_VERSION
        wonStyle = CStyle magenta True

    ascii <- TM.asciiOnly
    isBear <- TM.isBear

    let drawHS :: TM.TermM m => Bool -> Int -> HSInfo -> HS.Highscore -> m ()
        drawHS showName y info hs =
            drawStyledStrs win (CP.CPos (scoreX showName) y) $
            (case info of
                HSRank rank -> [(twoCharNum rank, styleBold)]
                HSAlive     -> [(" @", styleBold)]
                HSDead      -> [(" ",style0), ("@", CStyle (onRed white) True)]
                HSWon       -> [(" ",style0), ("@", wonStyle)]
            ) <>
            [ ("   ", style0)
            ] <>
            [ (take 8 (fromMaybe "[anon]" (HS.name hs) <> repeat ' ') <> "  ", CStyle cyan True)
            | showName
            ] <>
            [ (twoCharNum (HS.score hs) <> "~",
                if HS.score hs == G.maxScore then wonStyle else scoreStyle)
            , ("  ", style0)
            , (twoCharNum (HS.maxRound hs) <> ":", style0)
            , let lev = HS.maxLevel hs in (showLevel lev, levelStyle lev)
            , ("  ", style0)
            , let n = HS.gems hs in (twoCharNum n <> "*", CStyle magenta False)
            , ("  ", style0)
            ] <>
            [ if e `elem` HS.equipment hs
                then (take 1 $ show e, equipStyle e)
                else ("-", style0)
            | e <- allEquipment ]
        scoreX showName = min ((scrW - l) `div` 2) (scrW - l - 2 - aKeyL)
            where l = sum $ [length "99   99~  99:C  99*"]
                    <> [11 | showName] <> [2 + length allEquipment]
        additionalKeys =
            [ " More keys:" ]
            <> [ "T: Hints" | T.TMeta `S.notMember` G.unseenBeats game ]
            <> [ "-: " <> "ASCII " <> (if ascii then "[x]" else "[ ]")
               , "Q: Exit" ]
            <> concat [
                [ "─────────────"
                , "Alt + Enter:"
                , "   Fullscreen"
                , "Alt + -/+:"
                , "   Zoom"
                ] | isBear ]
        aKeyL = maximum $ length <$> additionalKeys

    let keysLine y = centre style0 y "Keys: cursors / WASD / HJKL; 0-9"

    drawTitle
    drawVersion
    if HS.maxRound curHs > 1
        then do
            centre styleBold 3 "Game in progress:"
            drawHS False 4 HSAlive curHs
            centre styleBold 6 "Press Space to continue"
        else case G.prevHS game of
                Just prev -> do
                    centre style0 3 "Last game:"
                    let info | HS.score prev >= G.maxScore = HSWon
                            | otherwise = HSDead
                    drawHS False 4 info prev
                    centre styleBold 6 "Press Space to start new game"
                Nothing -> centre styleBold 4 "Press Space to start"

    hss <- liftIO HSF.get
    let recent = G.showRecentHS game
    let ourHSs = filter ((== HS.name curHs) . HS.name) hss
    let avOver = 10
    if length ourHSs < avOver
        then keysLine 7
        else
            let (avd,avm) = (`divMod` avOver) . sum $ HS.score <$> take avOver ourHSs
            in centreStyled 7
                [ (show avOver <> " game average: ", style0)
                , (show avd <> "." <> show avm <> "~  ", scoreStyle)
                , ("R: " <> if recent then "Hide" else "Show", CStyle (onBlue white) False)
                ]

    unless (null hss) $ do
        let someNamed = any (isJust . HS.name) hss
            showHss
                | recent = ourHSs
                | otherwise = HS.sort hss

        drawStyledStrs win (CP.CPos (scoreX someNamed - 1) 9) $
            [ (if recent then "Last " else "Rank ", styleBold) ] <>
            [ ("  Name    ", CStyle cyan True) | someNamed ] <>
            [ ("Score ", scoreStyle)
            , ("Level ", style0)
            , ("Gems ", CStyle magenta False)
            , ("Equip", style0) ]
        sequence_ [ drawHS someNamed (10+i) (HSRank $ 1+i) hs | (i,hs) <- zip [0..9] showHss ]

    sequence_ [ TM.drawStr win
            (CStyle (onBlue white) $ n == 0)
            (CP.CPos (scrW - 2 - aKeyL) $ 10 + n) . take aKeyL $ s <> repeat ' '
        | (s,n) <- zip additionalKeys [0..]
        ]

tutBox :: T.Beat -> Maybe (CP.CPos, [Glyph])
tutBox = tutBox' where
    tutBox' (T.Movement p lf) = Just (boardOffset <> posCPosL p, [creatureGlyph $ Player lf])
    tutBox' (T.Trapped p lf) = Just (boardOffset <> posCPosL p, [creatureGlyph $ Player lf])
    tutBox' (T.SeeMonster p c) = Just (boardOffset <> posCPosL p, [creatureGlyph c])
    tutBox' (T.SeeExit wp) = Just (boardOffset <> wposCPos wp, [bold . exitChar True $ P.exitDir wp])
    tutBox' (T.MustBreak wp) = Just (boardOffset <> wposCPos wp, [bold . exitChar True $ P.exitDir wp])
    tutBox' (T.SeeItem p i) = Just (boardOffset <> posCPosR p, [itemGlyph i])
    tutBox' (T.SeePotion p) = Just (boardOffset <> posCPosR p, [itemGlyph Potion])
    tutBox' (T.SeeScore p) = Just (boardOffset <> posCPosR p, [itemGlyph ScoreTreasure])
    tutBox' (T.SeeJunk p) = Just (boardOffset <> posCPosR p, [itemGlyph Junk])
    tutBox' (T.SeeGem p) = Just (boardOffset <> posCPosR p, [itemGlyph . Gem $ B.powerTypeAt p])
    tutBox' T.CollectItem = Just (invOffset <> CP.CPos 0 1, [char '1'])
    tutBox' (T.CollectGem pTp) = Just (invOffset <> CP.CPos 0 10, [Glyph '0' $ powerStyle pTp])
    tutBox' T.CollectScore = Just (statusOffset <> CP.CPos 21 1, (char <$> ("1/" <> show G.maxScore)) <> [Glyph '~' scoreStyle])
    tutBox' (T.Hurt l ml) = Just (statusOffset <> CP.CPos 7 1, char <$> show l <> "/" <> show ml)
    tutBox' T.Timer = Just (levelInfoOffset <> CP.CPos (G.initWallSides - 5) 1, replicate 5 delSideGlyph)
    --tutBox' T.MustBreak = Just (levelInfoOffset <> CP.CPos 0 2, char <$> "Seal:")
    tutBox' _                  = Nothing
    winOffset win = CP.CPos x y where WinDim x y _ _ = geometry M.! win
    boardOffset = winOffset BoardWin
    invOffset = winOffset InvWin
    statusOffset = winOffset StatusWin
    levelInfoOffset = winOffset LevelInfoWin

highlightTut :: TM.TermM m => T.Beat -> m ()
highlightTut = maybe (pure ()) (uncurry TM.drawHighlightBoxChars) . tutBox
