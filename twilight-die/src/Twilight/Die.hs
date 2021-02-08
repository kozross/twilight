{-# LANGUAGE DerivingVia #-}

module Twilight.Die
  ( Die,
    Success (..),
    success,
  )
where

import System.Random.Stateful (Uniform (uniformM), uniformRM)

newtype Die = Die Word
  deriving (Eq, Show) via Word

instance Uniform Die where
  {-# INLINEABLE uniformM #-}
  uniformM g = Die <$> uniformRM (1, 6) g

data Success = Failed | Succeeded
  deriving stock (Eq, Show)

success :: Die -> Success
success (Die d)
  | d <= 4 = Failed
  | otherwise = Succeeded
