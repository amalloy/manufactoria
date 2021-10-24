{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.IntMap as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Foldable (toList)
import Data.Monoid (Alt(..))

type Index = Int
data Termination = Accept | Reject | Break deriving (Eq, Ord, Show, Read)
data Destination = Terminate Termination | Goto Index deriving (Eq, Ord, Show, Read)
data ProblemStatement = ProblemStatement { permittedExits :: [Termination]
                                         , inputColors, stampColors :: [Color]
                                         , allScannersHaveWhiteArrow :: Bool
                                         }

data Color = Red | Blue | Green | Yellow deriving (Enum, Eq, Ord, Show, Read)
data ArrowColor = AnyColor | ColoredArrow Color deriving (Eq, Ord, Show, Read)
type Tape = Seq.Seq Color

data Directed a = Directed Destination a
data Three a = Three { left, straight, right :: a }
data Node stamper scanner = OneWay (Directed stamper) | ThreeWay (Three (Directed stamper))
data Indexed a = Indexed Index a

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
scanner1 red blue = twoWayScanner (Red, red) (Blue, blue)
scanner2 green yellow = twoWayScanner (Green, green) (Yellow, yellow)
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

first :: Foldable f => f a -> Maybe a
first = getAlt . foldMap (Alt . Just)

data LayoutState = LayoutState { pending, placed, open :: S.Set Index }
type Search a = ReaderT ProblemStatement [] a

layouts :: Int -> Search [Indexed (Node () ())]
layouts n = go $ LayoutState (S.singleton 0) mempty mempty
  where go state = case first (pending state) of
          Nothing -> pure []
          Just nodeIndex -> do
            let state' = state { pending = S.delete nodeIndex (pending state)
                               , placed = S.insert nodeIndex (placed state)
                               }
            (state'', object) <- layoutScanner state' <|> layoutStamper state' nodeIndex
            (Indexed nodeIndex object :) <$> go state''

layoutStamper :: LayoutState -> Index -> Search (LayoutState, Node () ())
layoutStamper state self = fmap (fmap plainStamper) (reuse <|> placeNew)
  where plainStamper dst = OneWay (Directed dst ())
        reuse :: Search (LayoutState, Destination)
        reuse = (state,) <$> do
          exits <- asks permittedExits
          lift $ [Goto dst | dst <- toList (pending state) ++ toList (placed state)
                           , dst /= self] -- self-pointing scanners are never productive
            <|> [Terminate exit | exit <- exits]
        placeNew = lift $ do
          let opens = open state
          dst <- toList opens
          pure (state { pending = S.insert dst (pending state)
                      , open = S.delete dst opens
                      }
                , Goto dst)


layoutScanner :: LayoutState -> Search (LayoutState, Node () ())
layoutScanner = _

main :: IO ()
main = interact $ (show . length . factoriesUsing [Accept] [Red, Blue] [scanner1] . read)
