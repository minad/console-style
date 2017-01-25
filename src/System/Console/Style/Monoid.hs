{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module System.Console.Style.Monoid (
  Styled(..)
  , Flat(..)
  , flatten
) where

import System.Console.Style.Color
import Data.Semigroup (Semigroup)
import Data.String (IsString(..))
import GHC.Exts (IsList(..))
import Control.Monad (ap)

data Flat a
  = FValue a
  | FSet   !Attribute
  | FUnset !Attribute
  | FFg    !Color
  | FBg    !Color
  | FSave
  | FRestore
  | FReset
  deriving (Functor, Foldable, Traversable)

data Styled a
  = Value a
  | Set   !Attribute (Styled a)
  | Unset !Attribute (Styled a)
  | Fg    !Color     (Styled a)
  | Bg    !Color     (Styled a)
  | List             [Styled a]
  deriving (Functor, Foldable, Traversable)

instance Applicative Styled where
  pure = Value
  (<*>) = ap

instance Monad Styled where
  Value   x >>= f = f x
  List    x >>= f = List    (map (>>= f) x)
  Set   a x >>= f = Set   a (x >>= f)
  Unset a x >>= f = Unset a (x >>= f)
  Fg    a x >>= f = Fg    a (x >>= f)
  Bg    a x >>= f = Bg    a (x >>= f)

instance Semigroup (Styled a)
instance Monoid (Styled a) where
  mempty                     = List []
  List [] `mappend` b       = b
  a       `mappend` List [] = a
  List a  `mappend` List b  = List $ a `mappend` b
  List a  `mappend` b       = List $ a `mappend` [b]
  a       `mappend` List b  = List $ a : b
  a       `mappend` b       = List [a, b]

instance IsString a => IsString (Styled a) where
  fromString = Value . fromString

instance IsList (Styled a) where
  type Item (Styled a) = Styled a
  fromList = List
  toList (List s) = s
  toList s         = [s]

flatten :: Styled a -> [Flat a]
flatten s = go s []
  where go (Value a)      = (FValue a :)
        go (Set   a b)    = ((FSave : FSet   a : go b [FRestore]) ++)
        go (Unset a b)    = ((FSave : FUnset a : go b [FRestore]) ++)
        go (Fg    a b)    = ((FSave : FFg    a : go b [FRestore]) ++)
        go (Bg    a b)    = ((FSave : FBg    a : go b [FRestore]) ++)
        go (List  [])     = id
        go (List  (x:xs)) = go x . go (List xs)
