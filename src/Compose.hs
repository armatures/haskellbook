{-# LANGUAGE InstanceSigs #-}
module Compose where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) ->
           Compose f g a ->
           Compose f g b
  (Compose fgab) <*> (Compose fga) =
    Compose $ ((fmap (<*>)) fgab) <*> fga

-- one level of wrapping less than Compose:
newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f =>
  Functor (One f) where
  fmap g (One fa) = One $ fmap g fa

instance (Applicative f) => Applicative (One f) where
  pure :: a -> One f a
  pure = One . pure

  (<*>) :: One f (a -> b) -> One f a -> One f b
  (One gab) <*> (One ga) = One $ gab <*> ga
