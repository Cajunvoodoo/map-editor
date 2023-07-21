{-# LANGUAGE DataKinds, ScopedTypeVariables, TemplateHaskell, FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module CommandLib.Commands (
    Object (..)
  , ConsoleEnv (..)
  , ObjState (..)

) where

import qualified Data.Map as Map
import Commander.Params   (Flag(..), Value(..))
import Commander.Commands (Command(..), commands, command, help, run, evalCommand, ToParam)

import Data.Massiv.Array (Array, MArray, B, Ix2 ((:.)), Sz, B, P, Comp(..))
import qualified Data.Massiv.Array as M
import System.Console.ANSI.Codes as ANSI
import Optics
import Graphics.Vty (Vty)
import Control.Monad.ST.Strict (runST)
import Text.Read (Lexeme(Symbol), step, Read (readPrec), lexP, parens, prec)
import Control.Monad.RWS
import Data.Text (Text)
import qualified Data.Text as T

--
-- build up a Command (IO ()) object. The IO () is based
-- on the output of the functions used in the commands.
--
someCommands :: Command (IO ())
someCommands = commands $ do

    command "repeat" $ do

        help "Repeat a string n times"

        run $
            \(Value str :: Value "value to repeat" String)
             (Flag n :: Flag '["n"] "times to repeat" Int) ->
             sequence_ $ replicate n (putStrLn str)

    command "calculate" $ do

        help "perform calculations"

        command "add" $ do

            help "add two numbers"

            run $
                \(Value n1 :: Value "number 1" Int)
                 (Value n2 :: Value "number 2" Int)
                 (Flag verbose :: Flag '["v", "verbose"] "verbose mode" Bool) ->
                 if verbose
                    then putStrLn $ (show n1) ++ " + " ++ (show n2) ++ " = " ++ (show $ n1 + n2)
                    else putStrLn (show $ n1 + n2)

        command "multiply" $ do

            help "multiply two numbers"

            run $
                \(Value n1 :: Value "number 1" Int)
                 (Value n2 :: Value "number 2" Int) ->
                 putStrLn $ (show n1) ++ " x " ++ (show n2) ++ " = " ++ (show $ n1 * n2)

    command "login" $ do

        help "pretend authentication"

        run $
            \(Value username :: Value "Username" String)
             (Flag mPassword :: Flag '["p", "password"] "Password" (Maybe String)) -> do
             pass <- case mPassword of
                Just password -> return password
                Nothing -> getLine
             putStrLn $ "logging in with username=" ++ username ++ " password=" ++ pass


data Object = Floor
            | Wall
            | Robot
    deriving (Bounded, Eq, Ord, Enum, Read)


instance Show Object where
    show Floor = "-"
    show Wall  = "█"
    show Robot = setSGRCode [SetColor Foreground Vivid Red] <> "R" <> setSGRCode [Reset]

data ConsoleEnv = ConsoleEnv {
    _path     :: FilePath   -- | path for the output of the file
  , _initsize :: Int -- | size of the board
  , _vty      :: Vty
}
makeLenses ''ConsoleEnv

data ObjState = ObjState {
    _arr        :: Array B Ix2 Object
  , _pos        :: (Int, Int)
  , _sz         :: Int
  , _forceCtrls :: Bool
}
makeLenses ''ObjState

type Editor logEntry state = RWST ConsoleEnv [logEntry] state IO

arrCommands :: Command (Array B Ix2 Object)--(Array B Ix2 Object)
arrCommands = commands $ do
-- 
    command "set" $ do
        help "set (x1 :. y1) (x2 :. y2) Object. Sets a area to some object"

        run $
            \(Value (x1 :. y1) :: Value "Position 1" Ix2)
             (Value (x2 :. y2) :: Value "Position 2" Ix2)
             (Value obj        :: Value "Object"     Object)
             (Value st         :: Editor Text ObjState ()) ->
                 (M.empty :: Array B Ix2 Object)

-- instance ToParam (Array B Ix2 Object) where 
    -- toParam 
instance Read Ix2 where
    readPrec = parens $ prec up_prec (do
                              u <- step readPrec
                              Symbol ":." <- lexP
                              v <- step readPrec
                              return (u :. v))
        where
            up_prec = 5

--
-- run an IO () command, printing the error or command output:
--
runCommand :: [String] -> [(String,String)] -> Command (IO ()) -> IO ()
runCommand vals flags cmds = case evalCommand vals (Map.fromList flags) cmds of
    Left err -> putStrLn ("Error: " ++ show err)
    Right res -> res