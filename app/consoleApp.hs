{-# LANGUAGE TypeApplications, BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, MultiWayIf,
             TypeOperators, OverloadedStrings, RecordWildCards, TemplateHaskell, DeriveGeneric #-}
module Main where

import Data.Text (Text, replace, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.RWS
import TextShow as TS ( fromString, Builder, toText, unlinesB, TextShow (showb) )
import System.Console.ANSI.Codes as ANSI
import Data.Massiv.Array (Array, Comp (Seq, Par), Ix2 ((:.)), B, P, IxN ((:>)), Sz (Sz, Sz2), Vector, zipSwapM_, RealWorld, freeze)
import Data.Massiv.Array.Mutable (MArray)
import Data.Massiv.Array.Unsafe (unsafeThaw, unsafeFreeze)
import qualified Data.Massiv.Array as M
import Graphics.Vty.Input
import Graphics.Vty as V
import System.Exit
import Optics
import Optics.State.Operators
import GHC.Float (int2Double)
import Data.Maybe (fromMaybe)

import qualified Text.Megaparsec as P ((<|>))
import Text.Megaparsec as P hiding ((<|>))
import Text.Megaparsec.Char as C

import Data.Void ( Void )

import Control.Applicative (Alternative)
import qualified Control.Applicative as A
import qualified Data.Functor
import Data.Either (fromRight)
import Generics.Deriving.ConNames
import GHC.Generics

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data Command =  Write Text -- Filename
              | Quit
              | WriteQuit Text -- Filename
              | Jump Ix2
              | Fill Ix2 Ix2 Object
              | Help Commandlet
              | Hotkey Text Commandlet
              deriving (Show, Generic)
data Commandlet = Write' | Quit' | WriteQuit' | Jump' | Fill' | Hotkey' | Help' --Useful for help commands. Lets you request it without data
    deriving (Show, Generic)


type Editor logEntry state = RWST ConsoleEnv [logEntry] state IO
type Parser = Parsec Void Text

data Object = Floor
            | Wall
            | Robot
    deriving (Bounded, Eq, Ord, Enum, Read, Generic)

instance Show Object where
    show Floor = "-"
    show Wall  = "█"
    show Robot = "R"--setSGRCode [SetColor Foreground Vivid Red] <> "R" <> setSGRCode [Reset]

data ConsoleEnv = ConsoleEnv {
    _path     :: FilePath   -- ^ path for the output of the file
  , _initsize :: Int        -- ^ size of the board
  , _vty      :: Vty
}
makeLenses ''ConsoleEnv

data ObjState = ObjState {
    _arr        :: Array B Ix2 Object -- ^ The main array
  , _pos        :: (Int, Int)         -- ^ The position of the cursor as indexed by the array
  , _sz         :: Int                -- ^ Current size of the array (the array is always a square)
  , _forceCtrls :: Bool               -- ^ Force the controls to show over the array
  , _helpImg    :: Image              -- ^ The image that controls the help message
  , _hotkeys    :: Map Text (Editor Text ObjState Command)   -- ^ A list of the hotkeys for the given program
}
makeLenses ''ObjState

main = do
    cfg    <- standardIOConfig
    output <- outputForConfig cfg
    vty    <- mkVty cfg
    let arr = (M.makeArrayLinear Par (Sz (10 :. 10)) $ const Floor) :: Array B Ix2 Object
    runEditor (liftIO (refresh vty) >> mainLoop) (initialEnv vty) initialState


initialEnv :: Vty ->  ConsoleEnv
initialEnv vty  = ConsoleEnv {
    _path       = "newBoard.txt"
  , _initsize   = 10
  , _vty        = vty
}

initialState :: ObjState
initialState = ObjState {
    _arr = (M.makeArrayLinear Seq (Sz (10 :. 10)) $ const Floor) :: Array B Ix2 Object
  , _pos = (0,0)
  , _sz  = 10
  , _forceCtrls = False
  , _helpImg    = emptyImage
  , _hotkeys    = Map.empty
}

runEditor :: Editor logEntry state a -> ConsoleEnv -> state -> IO (a, [logEntry])
runEditor = evalRWST

getImgBoard :: Attr -> Editor Text ObjState Image
getImgBoard attr = do
    env <- ask
    cfg <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface $ env^.vty)
    vertCat . map (text' attr . replace "," " " . pack . init . tail . show) . M.toLists <$> zoom arr get

mainLoop :: Editor Text ObjState ()
mainLoop = do
    forever $ do
        ConsoleEnv {..} <- ask
        st <- get
        cfg <- liftIO standardIOConfig;
        (xBnd, yBnd) <- liftIO $ displayBounds (outputIface _vty)

        let !xPad   = xBnd`div`2 -st^.sz
            !yPad   = (yBnd-st^.sz)`div`2
            cntrHlp = pad 0 (yBnd-2) 0 0 $ st^.helpImg -- controls help message/error message. must be lazy

        boardImg <- pad xPad yPad 0 0 <$> (getImgBoard $! defAttr)
        hlpText  <- consoleHelpText

        liftIO $! update _vty (Picture (AbsoluteCursor (2*(st ^. pos % _1) + xPad) (st ^. pos % _2 + yPad ))
                                   [cntrHlp, hlpText, boardImg] ClearBackground )

        event <- liftIO $ nextEvent _vty
        handleEvent $! event
    -- mainLoop


setPos :: (Int, Int) -> Editor Text ObjState ()
setPos (y, x) = do
    env  <- ask
    st   <- get
    marr <- M.thaw $ st ^. arr -- turn the current array mutable
    e    <- M.readM marr (x :. y)
    M.writeM marr (x :. y) (if e == maxBound then minBound else succ e) -- set the pos. to the next elem, wrapping if needed
    arr' <- freeze Par marr
    arr  .= arr' -- freeze then set the array

chngBoardSz :: Int -> Editor Text ObjState ()
chngBoardSz i = do
    env <- ask
    ObjState {..} <- get
    cfg  <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface $ env^.vty)

    let yRatio = int2Double yBnd / int2Double _sz
        xRatio = int2Double xBnd / int2Double _sz

    unless (_sz == 1 && i<0 || yRatio < 1.005 && i>0 || xRatio <2.05 && i>0) $ do -- prevents an array with Sz2 0 0
        marr <- M.newMArray (Sz (_sz + i :. _sz + i)) Floor -- create a new mutable array
        oldMarr <- M.thaw _arr -- turn the old array mutable
        zipSwapM_ (0 :. 0) (marr :: MArray RealWorld B Ix2 Object) oldMarr -- swap the elements
        sz %= (+i) -- increment the size
        newArr <- freeze Par marr
        arr .= newArr
        mvCursorSafePos

mvCursorSafePos :: Editor Text ObjState ()
mvCursorSafePos = do
    st <- get
    when (st ^. pos % _1 > st ^. sz -1) $
        pos%_1 .= st ^. sz -1
    when (st ^. pos % _2 > st ^. sz -1) $
        pos%_2 .= st ^. sz -1
{-# INLINE mvCursorSafePos #-}

handleEvent :: Event -> Editor Text ObjState ()
handleEvent k@(EvKey key modifiers) = do
    ObjState {..} <- get
    when (key == KChar 'c' && modifiers == [MCtrl]) $ do
        ConsoleEnv {..} <- ask
        liftIO $ shutdown _vty >> liftIO exitSuccess

    case key of -- Check for user-defined hotkeys and execute them
        KChar c -> case Map.lookup (T.singleton c) _hotkeys of
            Nothing -> pure ()
            Just a -> do {com <- a ; handleCommand com}
        _       -> pure ()

    --TODO: make this also check the Map for the command
    handleKeySym key

    where
        handleKeySym KLeft = do
            st <- get
            let inBound = st ^. pos % _1 > 0
            when inBound $
                pos % _1 %= subtract 1

        handleKeySym KRight = do
            st  <- get
            env <- ask
            let inBound = st ^. pos % _1 < st ^. sz-1
            when inBound $
                pos % _1 %= (+1)

        handleKeySym KUp = do
            st  <- get
            env <- ask
            let inBound = st ^. pos % _2 > 0
            when inBound $
                pos % _2 %= subtract 1

        handleKeySym KDown = do
            st  <- get
            env <- ask
            let inBound = st ^. pos % _2 < st ^. sz-1
            when inBound $
                pos % _2 %= (+1)
        handleKeySym (KChar 'c') = do
            st  <- get
            env <- ask
            setPos (st ^. pos)
        handleKeySym (KChar '[') = chngBoardSz 1
        handleKeySym (KChar ']') = chngBoardSz (-1)
        handleKeySym (KChar 'H') = forceCtrls %= not
        handleKeySym (KChar ':') = handleUserInput "" emptyImage pCommands handleCommand
        handleKeySym KEnter      = helpImg .= emptyImage -- acknowledge help/error message
        handleKeySym _ = pure ()
handleEvent _ = pure ()
-- | Used for the vim-like command functionality. Acts as a text entry system.
--   The image argument is useful if your usage requires extra information to be shown while
--   input is given. The parser is what will validate the user input when entered
--   The callback is the function to perform an effectful action using the command.
handleUserInput ::  Text -> Image -> Parser a -> (a -> Editor Text ObjState ()) -> Editor Text ObjState ()
handleUserInput !inp !extraImg !parser !callback = do
    st <- get
    ConsoleEnv {..} <- ask
    cfg  <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface _vty)

    let !xPad = xBnd`div`2 -st^.sz
        !yPad = (yBnd-st^.sz)`div`2

    boardImg <- pad xPad yPad 0 0 <$> (getImgBoard $! defAttr)

    let !img = pad 0 (yBnd-1) 0 0 $ text' defAttr $ ":" <> inp
    liftIO $! update _vty (Picture (AbsoluteCursor (T.length inp+1) (yBnd-1))
                                [extraImg, img, boardImg] ClearBackground )

    handleUserInputHelper inp extraImg parser callback --call the helper function
{-# INLINE handleUserInput #-}

-- | Helper function that is ran after the initial prompt is shown
handleUserInputHelper ::  Text -> Image -> Parser a -> (a -> Editor Text ObjState ()) -> Editor Text ObjState ()
handleUserInputHelper !inp !extraImg !parser !callback = do
    st <- get
    ConsoleEnv {..} <- ask
    event <- liftIO (nextEvent _vty)--fromMaybe (EvKey (KFun 63) []) <$> liftIO (nextEventNonblocking _vty)

    cfg  <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface _vty)

    let !xPad = xBnd`div`2 -st^.sz
        !yPad = (yBnd-st^.sz)`div`2

    boardImg <- pad xPad yPad 0 0 <$> (getImgBoard $! defAttr)
    case event of -- unpack the event

        EvKey (KChar chr) [] -> do
            let !inp'  = T.snoc inp chr
                !img   = pad 0 (yBnd-1) 0 0 $ text' defAttr $ ":" <> inp'

            liftIO $! update _vty (Picture (AbsoluteCursor (T.length inp' +1) (yBnd-1))
                                        [extraImg , img, boardImg] ClearBackground )
            handleUserInput inp' extraImg parser callback --recurse
            --end event management
        --backspace
        EvKey KBS [] ->
            unless (T.null inp) $ do
            let !img = pad 0 (yBnd-1) 0 0 $ text' defAttr $ ":" <> T.init inp
            liftIO $! update _vty (Picture (AbsoluteCursor (T.length inp) (yBnd-1))
                                        [extraImg, img, boardImg] ClearBackground )
            handleUserInput (if T.null inp then "" else T.init inp) extraImg parser callback --recurse

        --handle the commands once enter is pressed
        EvKey KEnter [] -> do
            case parse parser "" $! inp of
                Left peb  -> helpImg .= V.string attrH (concatMap (<> ". ") . drop 4 . lines $ errorBundlePretty peb) --shows the "important" line of the parse error
                Right com -> callback com
        --this case is necessary to display the colon upon entering the function for the first time and any time a non-bound key is pressed
        _ -> do
            let !img = pad 0 (yBnd-1) 0 0 $ text' defAttr $ ":" <> inp
            liftIO $! update _vty (Picture (AbsoluteCursor (T.length inp+1) (yBnd-1))
                                        [extraImg, img, boardImg] ClearBackground )
            handleUserInput inp extraImg parser callback --recurse

consoleHelpText :: Editor Text ObjState Image
consoleHelpText = do
    ObjState   {..} <- get
    ConsoleEnv {..} <- ask
    cfg    <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface _vty)

    let yRatio = int2Double (yBnd-8)/ int2Double _sz

    if yRatio > 1 || _forceCtrls then
        return $ translateY (yBnd-5) $
                cfU ' ' xBnd 1
            <-> t "|" <|> cf ' ' (xBnd`div`2 -5) 1 <|> tU "Controls" <|> cf ' ' (xBnd `div`2-5) 1 <|> t "|"
            <-> t "    Arrow keys: Move Cursor          C: Change Object          [: Increase size          ]: Decrease size          0-9: Custom          (:): Enter command"
    else
        return $ translateY (yBnd-1) $
             tH "Controls hidden. Press Shift+H to show. Press again to hide."
    --TODO: Add the controls and the user-made controls (use show? if needed, redefine with generic library for no record clutter)
-- custom parser

(</>) :: Alternative f => f a -> f a -> f a
(</>) = (P.<|>)
{-# INLINE (</>) #-}


pCommands :: Parser Command
pCommands = choice [
     try parseHelp      -- help/h/?
   , parseWrite     -- w
   , parseQuit      -- quit/q
   , parseWriteQuit -- wq
   , parseJump      -- j
   , parseFill      -- f
   , parseHotkey    -- hotkey/hotkeys
   ]
    where
        writeS = Write' <$ (C.string "w" >> notFollowedBy (satisfy (=='q'))) ; quitS = Quit' <$ C.string "quit" </> C.string "q"                     ; writeQuitS = WriteQuit' <$ C.string "wq" ;
        jumpS  = Jump'  <$ C.string "j"  ; fillS = Fill' <$ C.string "f"     ; helpS = try (C.string "help") </> try (C.string "h") </> C.string "?" ; hotkeyS = Hotkey' <$ (try (C.string "hotkeys") </> C.string "hotkey") ;

        parseWrite     = Write     <$> (writeS >> pFilename)                 ; parseQuit   = Quit   <$  (quitS >> space >> eof)
        parseWriteQuit = WriteQuit <$> (writeQuitS >> pFilename)             ; parseJump   = Jump   <$> (jumpS >> pIx2)
        parseFill      = Fill      <$> (fillS *> pIx2) <*> pIx2 <*> pObject  ; parseHotkey = Hotkey <$> (hotkeyS *> pHotKey) <*> parseCommandlet

        parseHelp = Help <$> do
            helpS
            parseCommandlet
        -- parse a commandlet. useful for the hotkeys and help
        parseCommandlet = do
            space1
            choice [
                  try writeS
                , try quitS
                , try writeQuitS
                , try jumpS
                , try fillS
                , try hotkeyS
                , return Help'
                ]
            </> (eof >> return Help')
             --TODO: make this into a templatehaskell expression

        pHotKey :: Parser Text
        pHotKey = do
            space
            T.singleton <$> C.digitChar

{-# INLINE pCommands #-}
-- | Parse a filename
pFilename :: Parser Text
pFilename = do
    space
    name <- T.pack <$> some (alphaNumChar </> C.char '_' </> C.char ' ')
    T.singleton <$> C.char '.'
    extension <- T.pack <$> some alphaNumChar
    return $ name <> "." <> extension
{-# INLINE pFilename #-}

-- | Parse an Ix2
pIx2 :: Parser Ix2
pIx2 = do
    space
    many $ C.char '('
    x <- some numberChar
    space
    C.string ":."
    space
    y <- some numberChar
    many $ C.char ')'
    space
    return (read x :. read y)
{-# INLINE pIx2 #-}

-- | Parse an Object
pObject :: Parser Object
pObject =
    (C.string "Wall" <&> read . T.unpack) </> (C.string "Floor" <&> read . T.unpack) </> (C.string "Robot" <&> read . T.unpack)
{-# INLINE pObject  #-}


-- Handle the effect of the commands
handleCommand :: Command -> Editor Text ObjState ()
handleCommand command = do
    ObjState   {..} <- get
    ConsoleEnv {..} <- ask
    case command of
      Quit              -> liftIO (shutdown _vty) >> liftIO exitSuccess
      Write txt         -> liftIO $ writeArr _arr txt --TODO: COMPLETE (add unsaved confirmation) 
      WriteQuit txt     -> liftIO $ writeArr _arr txt >> shutdown _vty >> liftIO exitSuccess
      Jump (y :. x)     -> pos .= (y,x) >> mvCursorSafePos
      Fill (x1 :. y1) (x2 :. y2) obj -> do
          ObjState {..} <- get
          let xBnd = if x2 > _sz then _sz+1 else x2+1 -- prevent a stupidly big array
              yBnd = if y2 > _sz then _sz+1 else y2+1
          marr    <- M.newMArray (Sz (abs xBnd :. abs yBnd)) obj -- create a new mutable array
          oldMarr <- M.thaw _arr
          zipSwapM_ (x1 :. y1) oldMarr (marr :: MArray RealWorld B Ix2 Object) -- swap the elements
          freeze Seq marr -- required to avoid a memory leak (RealWorld arrays are not GC'd)
          newArr <- freeze Par oldMarr
          arr .= newArr
      Help c            -> do
          let !img = tH (getHelp c) <-> tH "Press enter to acknowledge"
          helpImg .= img
      Hotkey digit cmd  -> handleHotkey digit cmd
{-# INLINE handleCommand #-}

handleHotkey :: Text -> Commandlet -> Editor Text ObjState ()
handleHotkey digit commandlet = do
    ObjState   {..} <- get
    ConsoleEnv {..} <- ask
    cfg  <- liftIO standardIOConfig;
    (xBnd, yBnd) <- liftIO $ displayBounds (outputIface _vty)

    let !pad' = pad 0 (yBnd-5) 0 0

    case commandlet of
      Write'     -> handleUserInput "" (pad' writeImage) pFilename writeCallback
      Quit'      -> quitCallback () --TODO: handle unsaved message
      WriteQuit' -> handleUserInput "" (pad' writeImage) pFilename (\x -> writeCallback x >> quitCallback ())
      Jump'      -> handleUserInput "" (pad' jumpImage) pIx2 jumpCallback
      Fill'      -> handleUserInput "" (pad' fillImage) pFillHotkey fillCallback
      Hotkey'    -> pure ()
      Help'      -> pure ()
    where
        writeImage = tH ("Give a filename to write to when pressing " <> T.pack (show digit))

        writeCallback :: Text -> Editor Text ObjState ()
        writeCallback !filename = do
            ObjState {..} <- get
            hotkeys .= Map.insert digit (return $ Write filename) _hotkeys

        quitCallback :: () -> Editor Text ObjState ()
        quitCallback _ = do
            ObjState {..} <- get ; ConsoleEnv {..} <- ask
            hotkeys .= Map.insert digit (liftIO (shutdown _vty) >> liftIO exitSuccess ) _hotkeys

        jumpImage = tH ("Give a coordinate (y:.x) to jump to when pressing " <> T.pack (show digit))

        jumpCallback :: Ix2 -> Editor Text ObjState ()
        jumpCallback ix = do 
            ObjState {..} <- get
            hotkeys .= Map.insert digit (return $ Jump ix) _hotkeys

        fillImage = tH ("Give size (y:.x) and the block to use ("<>objList<>") when pressing "<> T.pack (show digit))

        pFillHotkey :: Parser (Ix2, Object)
        pFillHotkey = do
            space
            ix2 <- pIx2
            space
            obj <- pObject
            return (ix2, obj)

        fillCallback :: (Ix2, Object) -> Editor Text ObjState ()
        fillCallback (y :. x, obj) = do
            st <- get
            hotkeys .= Map.insert digit (do {ObjState {..} <- get ; return (Fill (snd _pos :. fst _pos) (snd _pos + x :. fst _pos + y) obj)}) (st ^. hotkeys)
{-# INLINE handleHotkey #-}
--TODO: COMPLETE HANDLEHOTKEY

--general utilities
attrU = Attr (SetTo underline) Default Default Default
attrH = Attr Default (SetTo black) (SetTo white) Default
t = text' defAttr ; tU = text' attrU ; tH = text' attrH
cf = charFill defAttr ; cfU = charFill attrU

class Help a where
    getHelp :: a -> Text
instance Help Commandlet where
    getHelp Write'     = "Write file to location. Must contain an extension"
    getHelp Quit'      = "Exit the program. Hotkey Ctrl+C"
    getHelp WriteQuit' = "Write a file to location, then quit"
    getHelp Jump'      = "Jump to a position on the board"
    getHelp Fill'      = "Fill a section of the board with an object. Valid objects include " <> objList
    getHelp Help'      = "Valid commands: w (name), q, wq (name), j (y:.x), f (y1:.x1) (y2:.x2) (obj), ? (cmd), hotkey 0-9 (obj)"
    getHelp Hotkey'    = "Define a hotkey (0-9) which performs an action on the cursor position or requires a default value"

objList = T.dropEnd 2 . T.concat $ map ((<> ", ") . T.pack) (conNames Floor)
objList' = conNames Floor

sectionInfo :: Array B Ix2 Object -> Text
sectionInfo arr = "SECTION Info:\n\
              \name=\"User-made level\"\n\
              \tile_spritesheet=Get_Spritesheet(\"tiles_spritesheet\")\n\
              \gravity_strength=0\n\
              \SECTION Tiles:\n"
            <> showArr arr
            <>"SECTION Key:\n\
              \█=wall\n\
              \_=floor\n\
              \SECTION Structures:\n"
            <> showArr arr
            <>"\nSECTION Structure_Key:\n\
              \R=Robot_Spawn_Point"
                --TODO: turn this into something that can use templatehaskell

showArr :: Array B Ix2 Object -> Text
showArr = T.unlines . map (replace "," " " . pack . init . tail . show) . M.toLists
{-# INLINE showArr #-}
writeArr :: Array B Ix2 Object -> Text -> IO ()
writeArr arr txt = TIO.writeFile (T.unpack txt) $ showArr arr
{-# INLINE writeArr #-}