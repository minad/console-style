-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Style
-- Copyright   :  Daniel Mendler (c) 2016,
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provides styled text output using ANSI
-- escape sequences. The main feature is that the library
-- keeps track of a stack of the active styles using a state monad.
-- This makes it easy to use this library for a pretty printer with
-- nested annotations, e.g., wl-pprint-console.
--
-- Warning: Windows support is currently not implemented, but
-- is planned (by using ansi-terminal or the ffi).
--
-- Example:
--
-- > basicExample :: IO ()
-- > basicExample = runWithStyle [Fg Blue] $ do
-- >   withStyle [Bold] $ liftIO $ putStr "Bold Blue"
-- >
-- >   setStyle [Save, Set Italic, Bg Red]
-- >   liftIO $ putStr "Italic Red"
-- >   setStyle [Restore]
-- >
-- >   setStyle [Set Under]
-- >   liftIO $ putStr "Underlined Blue"
-- >   setStyle [Reset]
-- >
-- >   liftIO $ putStrLn "Normal output"
--
-- For many more examples, see the
-- <https://github.com/minad/console-style/blob/master/example.hs example.hs> file.
-----------------------------------------------------------

module System.Console.Style (
  Attribute(..)
  , Color(..)
  , Term(..)
  , Styled(..)
  , Flat(..)
  , hGetTerm
  , getTerm
  , printStyled
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
import System.Console.Style.Color
import System.Console.Style.Term
import System.Console.Style.Style
import System.Console.Style.SGR
import System.Console.Style.Monoid
import Data.Functor.Identity

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
showFlatA str code term = go (defaultStyle, defaultStyle, [])
  where go s (FSet   a:b) = go (setAttr a True s) b
        go s (FUnset a:b) = go (setAttr a False s) b
        go s (FFg    a:b) = go (setFg a s) b
        go s (FBg    a:b) = go (setBg a s) b
        go s (FSave   :b) = go (saveStyle s) b
        go s (FRestore:b) = go (restoreStyle s) b
        go s (FReset  :b) = go (resetStyle s) b
        go s (FValue a:b) = let (old, new, stack) = s in
          mappend <$> (mappend <$> code (sgrCode term old new) <*> str a) <*> go (new, new, stack) b
        go s [] = let (old, new, _) = s in code (sgrCode term old new)

setAttr :: Attribute -> Bool -> StyleState -> StyleState
setAttr Bold   b (x, y, ys) = (x, y { styleBold   = b }, ys)
setAttr Italic b (x, y, ys) = (x, y { styleItalic = b }, ys)
setAttr Under  b (x, y, ys) = (x, y { styleUnder  = b }, ys)
setAttr Invert b (x, y, ys) = (x, y { styleInvert = b }, ys)
setAttr Blink  b (x, y, ys) = (x, y { styleBlink  = b }, ys)

setBg, setFg :: Color -> StyleState -> StyleState
setBg c (x, y, ys) = (x, y { styleBg = c }, ys)
setFg c (x, y, ys) = (x, y { styleFg = c }, ys)

resetStyle, saveStyle, restoreStyle :: StyleState -> StyleState
resetStyle   (x, _, ys)     = (x, defaultStyle, ys)
saveStyle    (x, y, ys)     = (x, y, y:ys)
restoreStyle (_, y, [])     = (y, defaultStyle, [])
restoreStyle (_, y, z : zs) = (y, z, zs)
