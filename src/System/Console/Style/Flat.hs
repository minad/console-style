{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module System.Console.Style.Flat (
  Attribute(..)
  , Color(..)
  , Term(..)
  , Styled(..)
  , hGetTerm
  , getTerm
  , hPrintStyled
  , printStyled
  , showStyled
  , showStyledA
  , showStyledS
) where

import System.IO (Handle, stdout, hPutStr)
import System.Console.Style.Term
import System.Console.Style.Style
import System.Console.Style.Color
import System.Console.Style.SGR
import Data.Functor.Identity
import Data.Bifunctor (first, second)
import GHC.Generics (Generic, Generic1)

data Styled a
  = Value a
  | Set   !Attribute
  | Unset !Attribute
  | Fg    !Color
  | Bg    !Color
  | Push
  | Pop
  | Reset
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)

hPrintStyled :: (Handle -> a -> IO ()) -> Handle -> Term -> [Styled a] -> IO ()
hPrintStyled out handle = showStyledA (out handle) (hPutStr handle)

printStyled :: (a -> IO ()) -> Term -> [Styled a] -> IO ()
printStyled out = hPrintStyled (const out) stdout

showStyledS :: Term -> [Styled String] -> String
showStyledS = showStyled id id

showStyled :: Monoid o => (a -> o) -> (SGRCode -> o) -> Term -> [Styled a] -> o
showStyled str code term flat = runIdentity $ showStyledA (pure . str) (pure . code) term flat

showStyledA :: (Applicative m, Monoid o) => (a -> m o) -> (SGRCode -> m o) -> Term -> [Styled a] -> m o
showStyledA str code term = go (defaultStyle, (defaultStyle, []))
  where go s (Set   a:b) = go ((second.first) (setAttr a True) s) b
        go s (Unset a:b) = go ((second.first) (setAttr a False) s) b
        go s (Fg    a:b) = go ((second.first) (setFg a) s) b
        go s (Bg    a:b) = go ((second.first) (setBg a) s) b
        go s (Push   :b) = go (second pushStyle s) b
        go s (Pop    :b) = go (second popStyle s) b
        go s (Reset  :b) = go (second resetStyle s) b
        go s (Value a:b) = let (old, stack@(new, _)) = s in
          mappend <$> (mappend <$> code (sgrCode term old new) <*> str a) <*> go (new, stack) b
        go s [] = let (old, (new, _)) = s in code (sgrCode term old new)
