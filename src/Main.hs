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
import Control.Monad (replicateM, guard, (<=<))
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Lens hiding (Index, Indexed)
import Data.Foldable (toList)

type Index = Int
data Termination = Accept | Reject | Break deriving (Eq, Ord, Show, Read)
data Destination = Terminate Termination | Goto Index deriving (Eq, Ord, Show, Read)

data Color = Red | Blue | Green | Yellow deriving (Enum, Eq, Ord, Show, Read)
data ArrowColor = White | ColoredArrow Color deriving (Eq, Ord, Show, Read)
type Tape = Seq.Seq Color

data Directed a = Directed { _dir :: Destination, _result :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Three a = Three { _left, _straight, _right :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
instance Applicative Three where
  pure x = Three x x x
  (Three f g h) <*> (Three x y z) = Three (f x) (g y) (h z)

newtype ScannerTemplate = ScannerTemplate { _template :: Three ArrowColor } deriving (Eq, Ord, Show, Read)
data Node stamper scanner = OneWay (Directed stamper)
                          | ThreeWay (Three (Directed scanner))
                          deriving (Eq, Ord, Show, Read)
data Indexed a = Indexed Index a deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data ProblemStatement = ProblemStatement { permittedExits :: [Termination]
                                         , colors :: [Color]
                                         , scanners :: [ScannerTemplate]
                                         , allScannersHaveWhiteArrow :: Bool
                                         }
data LayoutState = LayoutState { _pending, _placed, _open :: S.IntSet }
type Search a = ReaderT ProblemStatement [] a

makeLenses ''Directed
makeLenses ''Three
makeLenses ''ScannerTemplate
makeLenses ''Indexed
makeLenses ''LayoutState

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

hasWhiteArrow :: ScannerTemplate -> Bool
hasWhiteArrow = (== White) . view (template . straight)

colorings :: [Indexed (Node () ())] -> Search [Indexed (Node Color ArrowColor)]
colorings = mapM colorObject
  where colorObject (Indexed ix node) = do
          case node of
            OneWay stamper -> do
              colors <- asks colors
              color <- lift colors
              pure $ Indexed ix (OneWay (color <$ stamper))
            ThreeWay layout -> do
              templates <- asks scanners
              let legalScanners = case layout^.straight.dir of
                    Goto ix' | ix == ix' -> filter (not . hasWhiteArrow) templates
                    _ -> templates
              ScannerTemplate template <- lift legalScanners
              pure . Indexed ix . ThreeWay $ colorScanner <$> template <*> layout
                where colorScanner :: ArrowColor -> Directed a -> Directed ArrowColor
                      colorScanner = (<$)

twoWayScanner left right = ScannerTemplate (Three (ColoredArrow left) White (ColoredArrow right))
threeWayScanner left straight right = ScannerTemplate (Three (ColoredArrow left) (ColoredArrow straight) (ColoredArrow right))
scanner1, scanner2, scanner3, scanner4 :: ScannerTemplate
scanner1 = twoWayScanner Red Blue
scanner2 = twoWayScanner Green Yellow
scanner3 = threeWayScanner Red Green Blue
scanner4 = threeWayScanner Red Yellow Blue

main :: IO ()
main = interact $ (show .length . flip runReaderT (ProblemStatement [Accept, Reject] [Red, Blue] [scanner1] True) . (colorings <=< layouts) . read)
