module System.Console.Style.Main (
  printStyled
  , hPrintStyled
  , printStyledS
  , hPrintStyledS
  , hPrintFlat
  , printFlat
  , showFlat
  , showFlatA
  , showFlatS
) where

import System.IO (Handle, stdout, hPutStr)
import System.Console.Style.Term
import System.Console.Style.Style
import System.Console.Style.SGR
import System.Console.Style.Monoid
import Data.Functor.Identity
import Data.Bifunctor (first, second)

printStyled :: Term -> Styled (IO ()) -> IO ()
printStyled = hPrintStyled stdout

hPrintStyled :: Handle -> Term -> Styled (IO ()) -> IO ()
hPrintStyled handle term = hPrintFlat (const id) handle term . flatten

printStyledS :: Term -> Styled String -> IO ()
printStyledS = hPrintStyledS stdout

hPrintStyledS :: Handle -> Term -> Styled String -> IO ()
hPrintStyledS handle term = hPrintFlat hPutStr handle term . flatten

hPrintFlat :: (Handle -> a -> IO ()) -> Handle -> Term -> [Flat a] -> IO ()
hPrintFlat out handle = showFlatA (out handle) (hPutStr handle)

printFlat :: (a -> IO ()) -> Term -> [Flat a] -> IO ()
printFlat out = hPrintFlat (const out) stdout

showFlatS :: Term -> [Flat String] -> String
showFlatS = showFlat id id

showFlat :: Monoid o => (a -> o) -> (SGRCode -> o) -> Term -> [Flat a] -> o
showFlat str code term flat = runIdentity $ showFlatA (pure . str) (pure . code) term flat

showFlatA :: (Applicative m, Monoid o) => (a -> m o) -> (SGRCode -> m o) -> Term -> [Flat a] -> m o
showFlatA str code term = go (defaultStyle, (defaultStyle, []))
  where go s (FSet   a:b) = go ((second.first) (setAttr a True) s) b
        go s (FUnset a:b) = go ((second.first) (setAttr a False) s) b
        go s (FFg    a:b) = go ((second.first) (setFg a) s) b
        go s (FBg    a:b) = go ((second.first) (setBg a) s) b
        go s (FSave   :b) = go (second saveStyle s) b
        go s (FRestore:b) = go (second restoreStyle s) b
        go s (FReset  :b) = go (second resetStyle s) b
        go s (FValue a:b) = let (old, stack@(new, _)) = s in
          mappend <$> (mappend <$> code (sgrCode term old new) <*> str a) <*> go (new, stack) b
        go s [] = let (old, (new, _)) = s in code (sgrCode term old new)
