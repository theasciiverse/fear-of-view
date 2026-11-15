{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Serialise        (readFileDeserialise,
                                         writeFileSerialise)
import           Control.Exception.Safe (Exception, catch, throw, tryAny)
import           Control.Monad          (void, when)
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.State    (get, gets, lift, liftIO, put)
import           Safe                   (headMay)
import           System.Console.GetOpt  (ArgDescr (..), ArgOrder (..),
                                         OptDescr (..), getOpt, usageInfo)
import           System.Environment     (getArgs)
import           System.Exit            (exitSuccess)
import           System.FilePath        ((</>))

import           GameName
import           KeyBindings            (dvorakViBindings)
import           Serialise              ()

import qualified Command                as Cmd
import qualified Game                   as G
import qualified Highscore              as HS
import qualified HighscoreFile          as HSF
import qualified UIMonad                as UIM

#ifdef CURSES
import qualified CursesUI               as CU
import           CursesUIMInstance      ()
#endif

#ifdef BEAR
import qualified BearUI                 as BU
import           BearUIMInstance        ()
#ifdef CURSES
#define BOTH
import           Control.Monad          (unless)
#endif
#endif

version :: String
version = CURRENT_PACKAGE_VERSION

data Opt
    = StatePath FilePath
    | Username HS.Username
    | AsciiOnly
    | Dvorak
    | Help
    | Version
#ifdef BOTH
    | Curses
#endif
    deriving (Eq, Ord, Show)

options :: [OptDescr Opt]
options =
    [ Option ['s'] ["statefile"] (ReqArg StatePath "PATH") "Path to game state file"
    , Option ['n'] ["username"] (ReqArg Username "NAME") "Optional username for highscores"
    , Option ['a'] ["ascii"] (NoArg AsciiOnly) "Draw only ASCII characters"
    , Option ['d'] ["dvorak"] (NoArg Dvorak) "Use dvorak roguelike keys (htns)"
    , Option ['h'] ["help"] (NoArg Help) "Show usage information"
    , Option ['v'] ["version"] (NoArg Version) "Show version information"
#ifdef BOTH
    , Option ['c'] ["curses"] (NoArg Curses) "Run in textmode"
#endif
    ]

usage :: String
usage = usageInfo header options
    where header = "Usage: " <> gameName <> " [OPTION...]"

parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
        (o,n,[])   -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usage))

main :: IO ()
main = do
    (opts,_) <- parseArgs =<< getArgs
    when (Help `elem` opts) $ putStr usage >> exitSuccess
    when (Version `elem` opts) $ putStrLn version >> exitSuccess
    statePath <- case headMay [ path | StatePath path <- opts ] of
        Just path -> pure path
        Nothing -> do
            dir <- stateDir
            pure $ dir </> "gamestate"
    let ascii = AsciiOnly `elem` opts
    let dvorak = Dvorak `elem` opts
    let username = headMay [ name | Username name <- opts ]

    let ui :: UIM.UIMonad m => m ()
        ui = G.runT $ do
            lift $ UIM.setAsciiOnly ascii
            when dvorak . lift $ sequence_ [ UIM.setUIBinding ch cmd | (ch,cmd) <- dvorakViBindings ]
            liftIO (tryAny $ readFileDeserialise statePath) >>= \case
                Right game -> put game
                Left _     -> G.initGame
            let mainLoop :: UIM.UIMonad uiM => G.T uiM ()
                mainLoop = do
                    liftIO . writeFileSerialise statePath =<< get
                    lift . UIM.draw =<< get
                    G.clearTrans
                    playSt <- gets G.playState
                    let after = do
                            playSt' <- gets G.playState
                            when (playSt == G.Playing && playSt' `elem` [G.Dead, G.Won]) $
                                liftIO . HSF.add =<< gets (G.highscore username)
                            mainLoop
                    runExceptT (mapM_ processCommand =<< (lift . lift) UIM.getInput) >>=
                        either pure (const after)
            mainLoop

#ifdef BEAR
    let doBUI :: BU.UIM () -> IO ()
        doBUI m = void (UIM.doUI m) `catch` (\QuitException -> exitSuccess)

#ifdef CURSES
    unless (Curses `elem` opts) $ doBUI ui
#else
    doBUI ui
#endif
#endif

#ifdef CURSES
    let doCUI :: CU.UIM () -> IO ()
        doCUI m = void (UIM.doUI m) `catch` (\QuitException -> exitSuccess)
    doCUI ui
#endif


data QuitException = QuitException deriving Show
instance Exception QuitException

processCommand :: UIM.UIMonad uiM => Cmd.Command -> ExceptT () (G.T uiM) ()
processCommand Cmd.Quit        = throw QuitException
processCommand Cmd.ForceQuit   = throw QuitException
processCommand Cmd.ToggleAscii = lift . lift $ UIM.toggleAsciiOnly
processCommand Cmd.Redraw      = lift . lift $ UIM.redraw
processCommand Cmd.Suspend     = lift . lift $ UIM.suspend
processCommand cmd             = lift $ G.doCommand cmd
