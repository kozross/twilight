{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Twilight.Tarot
  ( Arcanum (..),
    Suit (..),
    Value (..),
    Card (..),
    spread,
  )
where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Kind (Type)
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random.Stateful (StatefulGen, uniformRM)

data Arcanum
  = Fool
  | Magician
  | HighPriestess
  | Empress
  | Emperor
  | Hierophant
  | Lovers
  | Chariot
  | Strength
  | Hermit
  | WheelOfFortune
  | Justice
  | HangedMan
  | Death
  | Temperance
  | Devil
  | Tower
  | Star
  | Moon
  | Sun
  | Judgement
  | World
  deriving stock (Eq, Show, Bounded, Enum)

data Suit = Swords | Pentacles | Cups | Wands
  deriving stock (Eq, Show, Bounded, Enum)

data Value
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Knight
  | Queen
  | King
  deriving stock (Eq, Show, Bounded, Enum)

data Card = Major !Arcanum | Minor !Value !Suit
  deriving stock (Eq, Show)

spread ::
  forall (m :: Type -> Type) (g :: Type).
  (StatefulGen g m) =>
  g ->
  Int ->
  m (Vector Card)
spread g n
  | n <= 0 = pure V.empty
  | n >= V.length allCards = pure allCards
  | otherwise =
    V.unsafeBackpermute allCards <$> V.unfoldrM go HS.empty
  where
    go :: HashSet Int -> m (Maybe (Int, HashSet Int))
    go acc =
      if HS.size acc == n
        then pure Nothing
        else Just <$> untilUnique acc
    untilUnique :: HashSet Int -> m (Int, HashSet Int)
    untilUnique acc = do
      i <- uniformRM (0, V.length allCards - 1) g
      if HS.member i acc
        then untilUnique acc -- try again
        else pure (i, HS.insert i acc)

-- Helpers

allCards :: Vector Card
allCards =
  [Major x | x <- go] <> [Minor v s | v <- go, s <- go]
  where
    go :: (Bounded a, Enum a) => Vector a
    go = V.enumFromTo minBound maxBound
