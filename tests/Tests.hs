module Main where

import ConsoleApp
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.RWS

import qualified Data.Text as T
import Data.Text ( Text )

import Control.Monad.Morph ( hoist )
import qualified Graphics.Vty.Input as V
import Data.Massiv.Array (Ix2((:.)))
import Graphics.Vty (mkVty)
import GHC.IO.Unsafe (unsafePerformIO)
import Optics.State.Operators ((.=))
import System.Exit (exitFailure)

main :: IO ()
main = do
  res <- checkParallel $$(discover)
  unless res exitFailure


default_env :: ConsoleEnv
default_env = ConsoleEnv
  {
    _path = "newBoard.txt"
  , _initsize = 10
  , _vty = unsafePerformIO $ mkVty mempty
  }
{-# NOINLINE default_env #-}


editorProp :: PropertyT (Editor Text ObjState) () -> Property
editorProp = property . hoist (\rwst -> fst <$> evalRWST rwst default_env initialState)

prop_test :: Property
prop_test =
  property . hoist (\rwst -> fst <$> evalRWST rwst default_env initialState) $ do
    lift $ handleEvent (V.EvKey V.KRight [])

-- | Given some X:.Y grid, filling in the region x₁:.y₁ x₂:.y₂ is an
-- idempotent operation; repeating the fill should change nothing, nor should
-- filling in a subregion with the same object.
-- Note: this property applies on all coordinate pairs because
-- @prop_equiv_range@ validates that property (it does not matter which is
-- greater).
prop_idempotent_fill :: Property
prop_idempotent_fill = editorProp $ do
  -- determine the size of the board
  boardSize <- forAll $ Gen.integral (Range.constant 1 20) --20 is a sane number
  -- set the board to the desired size
  lift $ setBoardSize boardSize

  obj <- forAll $ Gen.enum Floor Robot --generate any Object type
  -- generate 4 unique position pairs
  let posGenerator min = forAll $ Gen.integral (Range.constant min boardSize)

  x1 <- posGenerator 1
  y1 <- posGenerator 1
  x2 <- posGenerator x1
  y2 <- posGenerator x2

  let command = handleCommand (Fill (x1:.y1) (x2:.y2) obj)
  lift command
  ObjState {..} <- get
  let !firstBoard = _arr
      statusStr =
        "idempotent command: "
        <> "\nx1: " <> show x1
        <> "\nx2: " <> show x2
        <> "\ny1: " <> show y1
        <> "\ny2: " <> show y2
        <> "\nobj: " <> show obj
        <> "\nboard: " <> show _arr
  annotate statusStr
  forAllWith (const statusStr) (Gen.list (Range.constant 1 20) $
                                Gen.constant command) >>= lift . sequence_
  ObjState {..} <- get
  let !secondBoard = _arr
  firstBoard === secondBoard

-- | ∀x₁ x₂ y₁ y₂ : ℕ where either coordinate pair is larger than the other,
-- filling in a region ought to be the same no matter the argument order
prop_equiv_range :: Property
prop_equiv_range = editorProp $ do
  -- determine the size of the board
  boardSize <- forAll $ Gen.integral (Range.constant 1 20) --20 is a sane number
  -- set the board to the desired size
  lift $ setBoardSize boardSize

  obj <- forAll $ Gen.enum Floor Robot --generate any Object type
  -- generate 4 unique position pairs
  let posGenerator min = forAll $ Gen.integral (Range.constant min boardSize)

  x1 <- posGenerator 1
  y1 <- posGenerator 1
  x2 <- posGenerator x1
  y2 <- posGenerator y1

  ObjState {..} <- get
  let clearBoard = _arr
      command1   = handleCommand (Fill (x1:.y1) (x2:.y2) obj)
      command2   = handleCommand (Fill (x2:.y2) (x1:.y1) obj)
  -- run the first command
  lift $ setBoardSize boardSize >> command1
  ObjState {..} <- get
  let firstBoard = _arr

  -- reset the board
  lift $ arr .= clearBoard

  -- run the second command
  lift $ setBoardSize boardSize >> command2
  ObjState {..} <- get
  let secondBoard = _arr
  firstBoard === secondBoard

-- | Moving to a position with keypresses should exhibit the same behavior
-- as running the move command. That is, jumping shouldn't be able to do
-- anything moving can't and vice versa. For example, if you jump to a position
-- larger than the bounds of the array, you should stop at a wall (depending
-- on which axis was too large). The same goes for moving via the cursor, where
-- moving out of bounds is obviously prohibited.
prop_equiv_jump_move :: Property
prop_equiv_jump_move = editorProp $ do
  -- determine the size of the board
  boardSize <- forAll $ Gen.integral (Range.constant 1 20) --20 is a sane number
  -- set the board to the desired size
  lift $ setBoardSize boardSize

  -- generate the desired position
  let posGenerator = forAll $ Gen.integral (Range.constant 0 (boardSize-1))
  x1 <- posGenerator
  y1 <- posGenerator
  -- determine where the cursor starts and set it accordingly
  startX <- posGenerator
  startY <- posGenerator
  lift $ pos .= (startX, startY)

  let jumpCommand = handleCommand (Jump (x1:.y1))
      dx = x1 - startX
      dy = y1 - startY
      moveXKeys =
        if dx < 0
        then replicate (abs dx) (V.EvKey V.KLeft  [])
        else replicate (abs dx) (V.EvKey V.KRight [])
      moveYKeys =
        if dy < 0
        then replicate (abs dy) (V.EvKey V.KUp [])
        else replicate (abs dy) (V.EvKey V.KDown [])
      moveKeys = moveXKeys <> moveYKeys
      moveCommands = mapM_ handleEvent moveKeys
  annotate $ "dx: " <> show dx
  annotate $ "dy: " <> show dy
  annotate $ "moveKeys: " <> show moveKeys

  -- perform the jump
  lift jumpCommand
  ObjState {..} <- get
  let jump@(jumpX, jumpY) = _pos
  -- reset
  lift $ pos .= (startX, startY)
  lift moveCommands
  ObjState {..} <- get
  let move@(moveX, moveY) = _pos
  jump === move

-- TODO: write no_invalid_block_states
-- | Changing a block N times is equal to changing the block N mod c times,
-- where c is the number of different objects. This test ensures block states
-- are predictable upon change, repeat properly, and do not put the board in
-- an invalid state
-- prop_no_invalid_block_states :: Property
-- prop_no_invalid_block_states
