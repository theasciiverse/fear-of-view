{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module TermDraw where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forM, forM_, mplus, unless, void)
import           Control.Monad.State (get, lift, liftIO, put, runStateT)
import           Data.Bifunctor      (bimap)
import           Data.Function       (on)
import           Data.List           (intersperse, minimumBy)
import           Data.Maybe          (fromMaybe, isJust, isNothing, maybeToList)
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
        Umbrella     -> '/'
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
drawStatus (G.Game { G.life = life, G.maxLife = maxLife, G.score = score, G.level = level, G.round = rnd, G.junk = junk, G.equipment = equipment, G.board = B.Board { B.statuses = statuses } }) = do
    let win = StatusWin
    unless (M.null statuses) $ drawStyledStrs win (CP.CPos 0 0) statusStrs
    drawStyledStrs win (CP.CPos 0 1) numStrs
    where
    -- XXX: keep in sync with magic numbers in highlightTut
    numStrs =
        [ ("Life: ", style0)
        ] <> twoCharNumStyled life lifeStyle <>
        [ ("/", style0)
        , (show maxLife, if maxLife == G.initLife then style0 else equipStyle Charm)
        , ("   Score: ", style0)
        , (twoCharNum score <> "/" <> show G.maxScore, style0)
        , ("~", scoreStyle)
        , ("   Level: " <> twoCharNum rnd <> ":", style0)
        , (showLevel level, levelStyle level)
        ] <> if junk == 0 && S.null equipment then [] else
        [ ("   Junk: ", style0)
        , (twoCharNum junk <> "/" <> show (S.size equipment + 2), style0)
        , ("%", junkStyle)
        ]
    lifeStyle = case life of
        n | n <= 0 -> CStyle (onRed black) True
        1          -> CStyle red True
        2          -> CStyle red False
        3          -> CStyle yellow True
        4          -> CStyle yellow False
        5          -> style0
        6          -> CStyle green False
        _          -> CStyle green True
    statusStrs = intersperse ("   ", style0)
        [ (show stat <> " " <> twoCharNum n, statStyle status)
        | status@(stat,n) <- M.assocs statuses
        , n > 0 ]
    statStyle (B.Dazzled,_) = CStyle cyan True
    statStyle (B.Ghost,_) = style0
    statStyle (B.Smoke,_) = CStyle blue True
    statStyle (B.Haste,n) = CStyle (if n `mod` 2 == 1 then red else yellow) True
    statStyle (B.Foresight,_) = styleBold

exitChar :: P.Dir -> Char
exitChar = \case
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

char,bold,dim :: Char -> Glyph
char c = Glyph c style0
bold c = Glyph c styleBold
dim c = Glyph c $ CStyle (onBlue white) False

levelStyle :: Int -> CStyle
levelStyle 1 = CStyle green False
levelStyle 2 = CStyle white True
levelStyle 3 = CStyle yellow True
levelStyle _ = style0

showLevel :: Int -> String
showLevel l | l > 0 = (['A'..] !! (l-1)):""
showLevel _ = "-"

scoreStyle, junkStyle :: CStyle
scoreStyle = CStyle yellow False
junkStyle = CStyle blue True

creatureGlyph :: Creature -> Glyph
creatureGlyph Player          = bold '@'
creatureGlyph DeadPlayer      = Glyph '@' $ CStyle (onRed black) True
creatureGlyph BasicMonster    = Glyph 'm' $ CStyle yellow False
creatureGlyph CalmMonster     = Glyph 'p' $ CStyle blue True
creatureGlyph ChaseMonster    = Glyph 'c' $ CStyle yellow True
creatureGlyph GhostMonster    = Glyph 'g' $ CStyle white True
creatureGlyph SmartMonster    = Glyph 's' $ CStyle red True
creatureGlyph FastMonster     = Glyph 'f' $ CStyle magenta True
creatureGlyph (InflatedBalloon charges) = Glyph '0' . CStyle magenta $ charges > 0

itemGlyph :: Item -> Glyph
itemGlyph Gem           = Glyph '*' $ CStyle green True
itemGlyph Potion           = Glyph '!' $ CStyle green True
itemGlyph MiniPotion           = Glyph '!' $ CStyle green False
itemGlyph ScoreTreasure      = Glyph '~' scoreStyle
itemGlyph Junk      = Glyph '%' junkStyle
itemGlyph (UmbrellaHandle d)           = Glyph (if d `elem` [P.DUp, P.DDown] then '|' else '-') $ CStyle magenta True
itemGlyph CameraBoxed           = Glyph ')' $ CStyle cyan False
itemGlyph (RollingOrb _ _)           = Glyph 'o' $ CStyle cyan False
itemGlyph (ItemInvItem e) = invItemGlyph e

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
    Exit           -> bold $ exitChar (P.exitDir wp)
    KeyExit        -> Glyph (exitChar (P.exitDir wp)) $ equipStyle Key
    Entrance       -> char 'x'
    UnseenBoundary -> char '.'
    SeenBoundary   -> Glyph '│' st

exitGlyphHoriz :: CStyle -> P.WPos -> Exit -> (Glyph,Glyph)
exitGlyphHoriz st wp = \case
    Exit           -> (bold $ exitChar (P.exitDir wp), Glyph '─' st)
    KeyExit        -> (Glyph (exitChar (P.exitDir wp)) $ equipStyle Key, Glyph '─' st)
    Entrance       -> (char 'x', Glyph '─' st)
    UnseenBoundary -> doublet $ char '.'
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
        wallsV = highlightV `M.union` mvingV `M.union` wObscuredV `M.union` bdWallsV `M.union` borderV
        wallsH = highlightH `M.union` mvingH `M.union` wObscuredH `M.union` bdWallsH `M.union` borderH
        cells, items, creatures, obscured, powers :: M.Map P.Pos (Maybe Glyph,Maybe Glyph)
        layer = M.unionWith $ \(l,r) (l',r') -> (l `mplus` l', r `mplus` r')
        cells = highlightCells `layer` obscured `layer` creatures `layer` items `layer` powers
        filterV = M.filterWithKey $ const . not . P.up
        filterH = M.filterWithKey $ const . P.up
        filterSV = S.filter $ not . P.up
        filterSH = S.filter P.up
        glyphsV f m = M.mapWithKey f $ filterV m
        glyphsH f m = M.mapWithKey ((bimap Just Just .) . f) $ filterH m
        borderV = glyphsV (exitGlyphVert levBoundSt) $ B.exits bd
        borderH = glyphsH (exitGlyphHoriz levBoundSt) $ B.exits bd
        bdWallsV = glyphsV (const wallGlyphVert) $ B.walls bd
        bdWallsH = glyphsH (const wallGlyphHoriz) $ B.walls bd
        levBoundSt = levelStyle . BC.level $ B.conf bd
        items = M.mapWithKey itemGlyphs $ B.items bd
        itemGlyphs p i = (Nothing,) . Just . powerBG p $ itemGlyph i
        biGlyph gl = (Just gl, Just gl)
        powerBG :: P.Pos -> Glyph -> Glyph
        powerBG p
            | Just pow <- B.powers bd M.!? p, Pow.charges pow > 0 = modColour onRed
            -- | Just pow <- B.powers bd M.!? p, Pow.overUsable pow = modColour onYellow
            | otherwise = id
        powers = (Nothing,) . Just . powerGlyph <$> B.powers bd
        powerGlyph pow = Glyph '"' $ case True of
            _ | Pow.charges pow > 0 -> CStyle red True
            _ | Pow.overUsable pow  -> equipStyle Siphon
            _                       -> style0
        creatures = (,Nothing) . Just . creatureGlyph' <$> B.creatures bd
        creatureGlyph' Player = Glyph '@' $ CStyle col (not $ B.ghostly bd) where
            col | B.isHasteRound bd = red
                | B.hasted bd = yellow
                | otherwise = white
        creatureGlyph' c = creatureGlyph c
        obscured = M.fromSet (\p -> (Just . dim $ expectChar p,) . Just . powerBG p . dim $ obsChar p) $ B.poss S.\\ B.visible bd where
            expectChar p
                | B.preexpectant bd
                , Just (Just c) <- B.expected bd M.!? p = glyphChar $ creatureGlyph' c
                | otherwise = ' '
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
        intersections :: M.Map P.Pos Glyph
        intersections = iHighlighted `M.union` iObscured `M.union` iWalls
        iWalls = M.map (Glyph '·') . M.unionsWith pref $ M.fromList <$>
            [ [ (p,i), (p <> if up then P.Pos (-1) 0 else P.Pos 0 (-1),i) ]
            | (P.WPos p up, Just i) <- M.toList $
                M.map wallI (B.walls bd) `M.union` M.map (const $ Just exitCol) (B.exits bd) ]
            where
            wallI BasicWall        = Just $ CStyle white True
            wallI Hedge            = Just $ CStyle green True
            wallI ThickHedge       = Just $ CStyle green False
            wallI Pillar           = Nothing
            wallI Window           = Nothing
            wallI BrokenWindow     = Nothing
            wallI (UmbrellaWall _) = Nothing
            wallI (CloakWall _ _)  = Just $ CStyle cyan True
            wallI TentWall         = Just $ CStyle red True
            exitCol = levBoundSt
            pref a b = minimumBy (compare `on` cstyleCol) [a,b] -- white < green < magenta
        iObscured = M.fromSet (const $ dim ' ') $ S.filter (\p -> P.x p < B.w-1 && P.y p < B.h-1 &&
             and [ p' `S.member` M.keysSet obscured || not (B.inBounds p')
                 | p' <- (p +^) <$> [ P.Pos x y | x <- [0,1], y <- [0,1] ] ]) B.poss
        iHighlighted = M.fromSet (const $ bold '#') $ S.filter (\p -> P.x p < B.w-1 && P.y p < B.h-1 &&
             and [ p' `S.member` hps
                 | p' <- (p +^) <$> [ P.Pos x y | x <- [0,1], y <- [0,1] ] ]) B.poss

        horizCPosMap :: M.Map P.Pos (Maybe Glyph, Maybe Glyph) -> M.Map CP.CPos Glyph
        horizCPosMap m = M.mapMaybe fst (M.mapKeys posCPosL m) `M.union` M.mapMaybe snd (M.mapKeys posCPosR m)
        horizWCPosMap :: M.Map P.WPos (Maybe Glyph, Maybe Glyph) -> M.Map CP.CPos Glyph
        horizWCPosMap m = M.mapMaybe fst (M.mapKeys wposCPos m) `M.union` M.mapMaybe snd (M.mapKeys wposCPosR m)

        glyphs :: M.Map CP.CPos Glyph
        glyphs = M.unions
            [ horizCPosMap cells
            , M.mapKeys wposCPos wallsV
            , horizWCPosMap wallsH
            , M.mapKeys posIntCPos intersections
            ]
    in do
        sequence_ $ M.mapWithKey (TM.drawGlyph BoardWin) glyphs

drawInv :: TM.TermM m => Maybe I.Slot -> Int -> Bool -> I.Inventory -> Maybe Pow.Power -> m ()
drawInv sel preserve highlightEmpty (I.Inventory inv) pow = do
    let win = InvWin
    let str x y st = TM.drawStr win st (CP.CPos x y)
    str 0 0 styleBold "Inventory:"
    sequence_ $
        [ do
            str 0 slot style (show slot)
            case me of
                Nothing -> pure ()
                Just e -> do
                    TM.drawGlyph win (CP.CPos 2 slot) (invItemGlyph e)
                    str 4 slot style $ show e
        | slot <- I.slots
        , let me = inv M.!? slot
        , let style = CStyle col b where
                col | slot <= preserve = yellow
                    | highlightEmpty && isNothing me = red
                    | otherwise = white
                b = sel == Just slot
        ] <>
        [ str 0 (length I.slots + 2) style $ "0 \" " <> show tp <> " " <> show charges <> "/" <> show maxCharges
        | Pow.Power tp charges maxCharges overUsable <- maybeToList pow
        , let style | overUsable && charges == 0 = equipStyle Siphon
                | otherwise = CStyle red $ charges > 0
        ]

equipStyle :: Equipment -> CStyle
equipStyle Bag      = CStyle yellow False
equipStyle Charm    = CStyle green True
equipStyle GrabHand = CStyle red False
equipStyle Key      = CStyle blue True
equipStyle Siphon   = CStyle yellow True

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

drawLevelInfo :: TM.TermM m => B.Board -> m ()
drawLevelInfo bd = do
    drawRoll 0 (BC.creatureRoll bc) G.initCreatureSides creatureGlyph
    drawRoll 1 (BC.wallRoll bc) G.initWallSides wallGlyphVert
    drawDiffsLine 2
    where
    win = LevelInfoWin
    nullGlyph = char '-'
    drawRoll :: TM.TermM m => Int -> RF.RollFrom a -> Int -> (a -> Glyph) -> m ()
    drawRoll m roll initSides f = sequence_ $
        [ TM.drawGlyph win (CP.CPos n m) $ maybe nullGlyph f (RF.vals roll `atMay` n)
        | n <- [0 .. RF.sides roll - 1]
        ] <> [ TM.drawGlyph win (CP.CPos initSides m) $ char ']' ]
    bc = B.conf bd
    diffs = B.diffs bd
    drawDiffsLine y = TM.drawStr win style0 (CP.CPos 0 y) introStr >> sequence_
        [ do
            draw b . char' $ exitChar dir
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
        introStr = "On exit:  "
        draw x = TM.drawGlyph win (CP.CPos x y)
        rdfGlyph (BC.Add (BC.DiffableCreature c))    = creatureGlyph c
        rdfGlyph (BC.Swap _ (BC.DiffableCreature c)) = creatureGlyph c
        rdfGlyph (BC.Add (BC.DiffableWall wl))       = wallGlyphVert wl
        rdfGlyph (BC.Swap _ (BC.DiffableWall wl))    = wallGlyphVert wl
        exitsWith ex = P.exitDir <$> M.keys (M.filter (== ex) (B.exits bd))
        exitDirs = exitsWith Exit
        keyExitDirs = exitsWith KeyExit
        possibleExitDirs = exitDirs <> keyExitDirs <> exitsWith UnseenBoundary

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
        drawTitle = sequence_ [ drawStyledStrs win (CP.CPos titleX y) s
            | (y,s) <-
                [ (0, [ ("·──·──·──·", bdSt), ("     ", bgSt) ])
                , (1, [ ("│", bdSt), (" Fe@r of", styleBold)
                    , ("│", bdSt), ("View ", bgSt) ])
                , (2, [ ("·──·  ·──·", bdSt), ("     ", bgSt) ])
                ]
            ] where
            titleX = (scrW - length "| Fe@r of|View") `div` 2
            bdSt = CStyle yellow True
            bgSt = CStyle (onBlue white) True
        wonStyle = CStyle magenta True

    ascii <- TM.asciiOnly

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
            ] <>
            [ ("  ", style0) ] <>
            [ if e `elem` HS.equipment hs
                then (take 1 $ show e, equipStyle e)
                else ("-", style0)
            | e <- allEquipment ]
        scoreX showName = min ((scrW - l) `div` 2) (scrW - l - 1 - aKeyL)
            where l = sum $ [length "99   99~  99:C"]
                    <> [11 | showName] <> [2 + length allEquipment]
        additionalKeys =
            [ " More keys:" ]
            <> [ "T: Hints" | T.TMeta `S.notMember` G.unseenBeats game ]
            <> [ "-: " <> "ASCII " <> (if ascii then "[x]" else "[ ]")
               , "Q: Exit" ]
        aKeyL = maximum $ length <$> additionalKeys

    let keysLine y = centre style0 y "Keys: cursors / WASD / HJKL; 0-9"

    drawTitle
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
    keysLine 7

    hss <- liftIO HSF.get
    unless (null hss) $ do
        let someNamed = any (isJust . HS.name) hss
        drawStyledStrs win (CP.CPos (scoreX someNamed - 1) 9) $
            [ ("Rank ", styleBold) ] <>
            [ ("  Name    ", CStyle cyan True) | someNamed ] <>
            [ ("Score ", scoreStyle)
            , ("Level ", style0)
            , ("Equip", style0) ]
        sequence_ [ drawHS someNamed (10+i) (HSRank $ 1+i) hs | (i,hs) <- zip [0..] hss ]

    sequence_ [ TM.drawStr win
            (CStyle (onBlue white) $ n == 0)
            (CP.CPos (scrW - 2 - aKeyL) $ 10 + n) . take aKeyL $ s <> repeat ' '
        | (s,n) <- zip additionalKeys [0..]
        ]

tutBox :: T.Beat -> Maybe (CP.CPos, [Glyph])
tutBox = tutBox' where
    tutBox' (T.Movement p) = Just (boardOffset <> posCPosL p, [creatureGlyph Player])
    tutBox' (T.Trapped p) = Just (boardOffset <> posCPosL p, [creatureGlyph Player])
    tutBox' (T.SeeMonster p c) = Just (boardOffset <> posCPosL p, [creatureGlyph c])
    tutBox' (T.SeeExit wp) = Just (boardOffset <> wposCPos wp, [bold . exitChar $ P.exitDir wp])
    tutBox' (T.SeeItem p i) = Just (boardOffset <> posCPosR p, [itemGlyph i])
    tutBox' (T.SeePotion p) = Just (boardOffset <> posCPosR p, [itemGlyph Potion])
    tutBox' (T.SeeMiniPotion p) = Just (boardOffset <> posCPosR p, [itemGlyph MiniPotion])
    tutBox' (T.SeeScore p) = Just (boardOffset <> posCPosR p, [itemGlyph ScoreTreasure])
    tutBox' (T.SeeJunk p) = Just (boardOffset <> posCPosR p, [itemGlyph Junk])
    tutBox' (T.SeeGem p) = Just (boardOffset <> posCPosR p, [itemGlyph Gem])
    tutBox' T.CollectItem = Just (invOffset <> CP.CPos 0 1, [char '1'])
    tutBox' T.CollectGem = Just (invOffset <> CP.CPos 0 10, [Glyph '0' $ CStyle red True])
    tutBox' T.CollectScore = Just (statusOffset <> CP.CPos 20 1, (char <$> (" 1/" <> show G.maxScore)) <> [Glyph '~' scoreStyle])
    tutBox' (T.Hurt l ml) = Just (statusOffset <> CP.CPos 6 1, char <$> twoCharNum l <> "/" <> show ml)
    tutBox' T.Timer = Just (levelInfoOffset <> CP.CPos (G.initCreatureSides - 4) 0, char <$> "-   ]")
    tutBox' T.SecondRound = Just (levelInfoOffset <> CP.CPos 0 2, char <$> "On exit:")
    tutBox' _                  = Nothing
    winOffset win = CP.CPos x y where WinDim x y _ _ = geometry M.! win
    boardOffset = winOffset BoardWin
    invOffset = winOffset InvWin
    statusOffset = winOffset StatusWin
    levelInfoOffset = winOffset LevelInfoWin

highlightTut :: TM.TermM m => T.Beat -> m ()
highlightTut = maybe (pure ()) (uncurry TM.drawHighlightBoxChars) . tutBox
