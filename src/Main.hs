{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.IntMap as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Control.Monad (replicateM, guard)
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

data Directed a = Directed Destination a deriving (Eq, Ord, Show, Read)
data Three a = Three { left, straight, right :: a } deriving (Eq, Ord, Show, Read)
data Node stamper scanner = OneWay (Directed stamper) | ThreeWay (Three (Directed stamper))
                          deriving (Eq, Ord, Show, Read)
data Indexed a = Indexed Index a deriving (Eq, Ord, Show, Read)

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

first :: Foldable f => f a -> Maybe a
first = getAlt . foldMap (Alt . Just)

data LayoutState = LayoutState { pending, placed, open :: S.Set Index }
type Search a = ReaderT ProblemStatement [] a

layouts :: Int -> Search [Indexed (Node () ())]
layouts n = go $ LayoutState { pending = S.singleton 0
                             , placed = mempty
                             , open = S.fromList [1..n]
                             }
  where go state = case first (pending state) of
          Nothing -> pure []
          Just nodeIndex -> do
            let state' = state { pending = S.delete nodeIndex (pending state)
                               , placed = S.insert nodeIndex (placed state)
                               }
            (state'', object) <- layoutScanner state' nodeIndex <|> layoutStamper state' nodeIndex
            (Indexed nodeIndex object :) <$> go state''

allowedExits :: Search Destination
allowedExits = do
  exits <- asks permittedExits
  lift $ Terminate <$> exits

reachable :: LayoutState -> [Index]
reachable state = toList (pending state) ++ toList (placed state)

explore :: LayoutState -> Search (LayoutState, Destination)
explore state = lift $ do
  let opens = open state
  dst <- toList opens
  pure (state { pending = S.insert dst (pending state)
              , open = S.delete dst opens
              }
        , Goto dst)


layoutStamper :: LayoutState -> Index -> Search (LayoutState, Node () ())
layoutStamper state self = fmap (fmap plainStamper) (reuse <|> explore state)
  where plainStamper dst = OneWay (Directed dst ())
        reuse :: Search (LayoutState, Destination)
        reuse = (state,) <$>
                (allowedExits <|>
                 lift [Goto dst | dst <- reachable state
                                , dst /= self]) -- self-pointing scanners are never productive)

layoutScanner :: LayoutState -> Index -> Search (LayoutState, Node () ())
layoutScanner state self = do
  allowSelfArrows <- asks $ not . allScannersHaveWhiteArrow
  (state', s) <- choose state
  guard $ allowSelfArrows || (s /= Goto self)
  (state'', l) <- choose state'
  (state''', r) <- choose state''
  pure (state''', ThreeWay (Three (Directed l ()) (Directed s ()) (Directed r ())))
  where choose state = ((state,) <$> reuse) <|> explore state
          where reuse :: Search Destination
                reuse = allowedExits <|> lift [Goto dst | dst <- reachable state]


main :: IO ()
main = interact $ (show . length . flip runReaderT (ProblemStatement [Accept] [] [] True) . layouts . read)
