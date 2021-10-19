module Main where

import qualified Data.IntMap as M
import qualified Data.Sequence as S
import Control.Monad (replicateM)

type Index = Int
data Termination = Accept | Reject | Break deriving (Eq, Ord, Show, Read)
data Destination = Terminate Termination | Goto Index deriving (Eq, Ord, Show, Read)

data Color = Red | Blue | Green | Yellow deriving (Enum, Eq, Ord, Show, Read)
data ArrowColor = AnyColor | ColoredArrow Color deriving (Eq, Ord, Show, Read)
type Tape = S.Seq Color

data ScanResult = ScanResult {consume :: Bool, next :: Destination} deriving (Eq, Ord, Show, Read)
data Arrow = Arrow ArrowColor ScanResult deriving (Eq, Ord, Show, Read)
data Decider = Decider [Arrow] deriving (Eq, Ord, Show, Read)
data Transition = Stamp Color Destination | Scan Decider deriving (Eq, Ord, Show, Read)

type Factory = M.IntMap Transition

type Scanner = Destination -> Destination -> Destination -> Decider
type ScannerArrow = (Color, Destination)

twoWayScanner :: ScannerArrow -> ScannerArrow -> Destination -> Decider
twoWayScanner (leftColor, leftDest) (rightColor, rightDest) defaultDest =
  Decider [ Arrow (ColoredArrow leftColor) $ ScanResult { consume = True, next = leftDest }
          , Arrow (ColoredArrow rightColor) $ ScanResult { consume = True, next = rightDest }
          , Arrow AnyColor $ ScanResult { consume = False, next = defaultDest }
          ]
threeWayScanner :: Destination -> Destination -> ScannerArrow -> Decider
threeWayScanner redDest blueDest (otherColor, otherDest) =
  Decider [ Arrow (ColoredArrow Red) $ ScanResult {consume = True, next = redDest}
          , Arrow (ColoredArrow Blue) $ ScanResult {consume = True, next = blueDest}
          , Arrow (ColoredArrow otherColor) $ ScanResult {consume = True, next = otherDest}
          ]

scanner1, scanner2, scanner3, scanner4 :: Scanner
scanner1 red blue other = twoWayScanner (Red, red) (Blue, blue) other
scanner2 green yellow other = twoWayScanner (Green, green) (Yellow, yellow) other
scanner3 red blue green = threeWayScanner red blue (Green, green)
scanner4 red blue yellow = threeWayScanner red blue (Yellow, yellow)

factoriesUsing :: [Termination] -> [Color] -> [Scanner] -> Index -> [Factory]
factoriesUsing terminations colors scanners maxState = do
  fmap (M.fromList . zip [0..maxState]) . replicateM (maxState + 1)
  $ chooseScanner terminations scanners maxState ++ chooseStamper terminations colors maxState

outcome :: [Termination] -> Index -> [Destination]
outcome exits maxState = map Terminate exits ++ map Goto [0..maxState]

chooseScanner :: [Termination] -> [Scanner] -> Index -> [Transition]
chooseScanner exits scanners maxState = do
  [left, right, straight] <- replicateM 3 $ outcome exits maxState
  scanner <- scanners
  pure (Scan (scanner left right straight))

chooseStamper :: [Termination] -> [Color] -> Index -> [Transition]
chooseStamper exits colors maxState = Stamp <$> colors <*> outcome exits maxState

main :: IO ()
main = interact $ (show . length . factoriesUsing [Accept] [Red, Blue] [scanner1] . read)
