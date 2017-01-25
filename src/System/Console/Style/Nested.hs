{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module System.Console.Style.Nested (
  Attribute(..)
  , Color(..)
  , Term(..)
  , Styled(..)
  , hGetTerm
  , getTerm
  , printStyled
  , hPrintStyled
  , printStyledS
  , hPrintStyledS
) where

import System.IO (Handle, stdout, hPutStr)
import System.Console.Style.Term
import System.Console.Style.Color
import System.Console.Style.Trustworthy
import Data.String (IsString(..))
import Data.Semigroup (Semigroup)
import GHC.Generics (Generic, Generic1)
import qualified System.Console.Style.Flat as Flat
import Control.Monad (ap)

data Styled a
  = Value a
  | Set   !Attribute (Styled a)
  | Unset !Attribute (Styled a)
  | Fg    !Color     (Styled a)
  | Bg    !Color     (Styled a)
  | List             [Styled a]
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1)

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

flatten :: Styled a -> [Flat.Styled a]
flatten s = go s []
  where go (Value a)      = (Flat.Value a :)
        go (Set   a b)    = (Flat.Push:) . (Flat.Set   a:) . go b . (Flat.Pop:)
        go (Unset a b)    = (Flat.Push:) . (Flat.Unset a:) . go b . (Flat.Pop:)
        go (Fg    a b)    = (Flat.Push:) . (Flat.Fg    a:) . go b . (Flat.Pop:)
        go (Bg    a b)    = (Flat.Push:) . (Flat.Bg    a:) . go b . (Flat.Pop:)
        go (List  [])     = id
        go (List  (x:xs)) = go x . go (List xs)

printStyled :: Term -> Styled (IO ()) -> IO ()
printStyled = hPrintStyled stdout

hPrintStyled :: Handle -> Term -> Styled (IO ()) -> IO ()
hPrintStyled handle term = Flat.hPrintStyled (const id) handle term . flatten

printStyledS :: Term -> Styled String -> IO ()
printStyledS = hPrintStyledS stdout

hPrintStyledS :: Handle -> Term -> Styled String -> IO ()
hPrintStyledS handle term = Flat.hPrintStyled hPutStr handle term . flatten
