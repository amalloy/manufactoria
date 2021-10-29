{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.IntMap as M
import qualified Data.Sequence as Seq
import qualified Data.IntSet as S
import Control.Applicative ((<|>))
import Control.Monad (replicateM, guard)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Lens hiding (Index, Indexed)
import Data.Foldable (toList)
import Data.Monoid (Alt(..))

type Index = Int
data Termination = Accept | Reject | Break deriving (Eq, Ord, Show, Read)
data Destination = Terminate Termination | Goto Index deriving (Eq, Ord, Show, Read)
data ProblemStatement = ProblemStatement { permittedExits :: [Termination]
                                         , colors :: [Color]
                                         , allScannersHaveWhiteArrow :: Bool
                                         }

data Color = Red | Blue | Green | Yellow deriving (Enum, Eq, Ord, Show, Read)
data ArrowColor = White | ColoredArrow Color deriving (Eq, Ord, Show, Read)
type Tape = Seq.Seq Color

data Directed a = Directed Destination a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Three a = Three { _left, _straight, _right :: a } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
makeLenses ''Three
data Node stamper scanner = OneWay (Directed stamper) | ThreeWay (Three (Directed stamper))
                          deriving (Eq, Ord, Show, Read)
data Indexed a = Indexed Index a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
makeLenses ''Indexed

data ScanResult = ScanResult {_consume :: Bool, _next :: Destination} deriving (Eq, Ord, Show, Read)
makeLenses ''ScanResult
data Arrow = Arrow ArrowColor ScanResult deriving (Eq, Ord, Show, Read)
newtype Decider = Decider {_arrows :: [Arrow]} deriving (Eq, Ord, Show, Read)
makeLenses ''Decider
data Transition = Stamp Color Destination | Scan Decider deriving (Eq, Ord, Show, Read)

type Factory = M.IntMap Transition

type Scanner = Destination -> Destination -> Destination -> Decider
type ScannerArrow = (Color, Destination)

twoWayScanner :: ScannerArrow -> ScannerArrow -> Destination -> Decider
twoWayScanner (leftColor, leftDest) (rightColor, rightDest) defaultDest =
  Decider [ Arrow (ColoredArrow leftColor) $ ScanResult { _consume = True, _next = leftDest }
          , Arrow (ColoredArrow rightColor) $ ScanResult { _consume = True, _next = rightDest }
          , Arrow White $ ScanResult { _consume = False, _next = defaultDest }
          ]
threeWayScanner :: Destination -> Destination -> ScannerArrow -> Decider
threeWayScanner redDest blueDest (otherColor, otherDest) =
  Decider [ Arrow (ColoredArrow Red) $ ScanResult {_consume = True, _next = redDest}
          , Arrow (ColoredArrow Blue) $ ScanResult {_consume = True, _next = blueDest}
          , Arrow (ColoredArrow otherColor) $ ScanResult {_consume = True, _next = otherDest}
          ]

scanner1, scanner2, scanner3, scanner4 :: Scanner
scanner1 red blue = twoWayScanner (Red, red) (Blue, blue)
scanner2 green yellow = twoWayScanner (Green, green) (Yellow, yellow)
scanner3 red blue green = threeWayScanner red blue (Green, green)
scanner4 red blue yellow = threeWayScanner red blue (Yellow, yellow)

data LayoutState = LayoutState { _pending, _placed, _open :: S.IntSet }
makeLenses ''LayoutState
type Search a = ReaderT ProblemStatement [] a

moveNode :: Index -> Setter' s S.IntSet -> Setter' s S.IntSet -> s -> s
moveNode idx from to =   over from (S.delete idx)
                       . over to (S.insert idx)

layouts :: Int -> Search [Indexed (Node () ())]
layouts n = go $ LayoutState { _pending = S.singleton 0
                             , _placed = mempty
                             , _open = S.fromList [1..n]
                             }
  where go state = case S.minView $ view pending state of
          Nothing -> pure []
          Just (nodeIndex, pending') -> do
            let state' = state & set pending pending' & over placed (S.insert nodeIndex)
            (state'', object) <- layoutScanner state' nodeIndex <|> layoutStamper state' nodeIndex
            (Indexed nodeIndex object :) <$> go state''

allowedExits :: Search Destination
allowedExits = do
  exits <- asks permittedExits
  lift $ Terminate <$> exits

reachable :: LayoutState -> [Index]
reachable = S.toList . view (pending <> placed)

explore :: LayoutState -> Search (LayoutState, Destination)
explore state = lift $ do
  dst <- S.toList . view open $ state
  pure (state & moveNode dst open pending, Goto dst)


layoutStamper :: LayoutState -> Index -> Search (LayoutState, Node () ())
layoutStamper state self = fmap (fmap plainStamper) (reuse <|> explore state)
  where plainStamper dst = OneWay (Directed dst ())
        reuse :: Search (LayoutState, Destination)
        reuse = (state,) <$>
                (allowedExits <|>
                 lift [Goto dst | dst <- reachable state
                                , dst /= self]) -- self-pointing scanners are never productive

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

colorings :: [Indexed (Node () ())] -> Search [Indexed (Node Color ArrowColor)]
colorings = mapM colorObject
  where colorObject (Indexed ix node) = do
          case node of
            OneWay stamper -> do
              colors <- asks colors
              color <- lift colors
              pure $ Indexed ix (OneWay (color <$ stamper))
            ThreeWay arrows -> undefined
              

main :: IO ()
main = interact $ (show . length . flip runReaderT (ProblemStatement [Accept] [Red, Blue] True) . layouts . read)
