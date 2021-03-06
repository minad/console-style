module Main where

import System.Console.Style
import Control.Monad.Trans
import Data.Foldable
import Text.Printf
import Control.Monad.Trans.State.Strict
import System.IO (stdout)

ansiColors :: [Color]
ansiColors = [ DefaultColor
             , Black
             , Red
             , Green
             , Yellow
             , Blue
             , Magenta
             , Cyan
             , White
             , DullBlack
             , DullRed
             , DullGreen
             , DullYellow
             , DullBlue
             , DullMagenta
             , DullCyan
             , DullWhite
             ]

ansiColorsExample :: IO ()
ansiColorsExample = runWithStyle [] $
  for_ ansiColors $ \c -> do
    withStyle [BgColor c] $ liftIO $ printf "%-15s" $ show c
    withStyle [FgColor c] $ liftIO $ printf " %-15s" $ show c
    withStyle [BgColor c, Invert] $ liftIO $ printf " %-15s" $ show c
    withStyle [FgColor c, Invert] $ liftIO $ printf " %-15s" $ show c
    liftIO $ putChar '\n'

colors256Example :: IO ()
colors256Example = runWithStyle [] $
  for_ [0..255] $ \c -> do
    withStyle [BgColor $ Color256 c] $ liftIO $ printf "%02x" c
    withStyle [FgColor $ Color256 c] $ liftIO $ printf " %02x" c
    withStyle [BgColor $ Color256 c, Invert] $ liftIO $ printf " %02x" c
    withStyle [FgColor $ Color256 c, Invert] $ liftIO $ printf " %02x" c
    liftIO $ putChar '\n'

rgbExample :: IO ()
rgbExample = runWithStyle [] $
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        withStyle [BgColor c] $ liftIO $ printf "%-20s" $ show c
        withStyle [FgColor c] $ liftIO $ printf " %-20s" $ show c
        withStyle [BgColor c, Invert] $ liftIO $ printf " %-20s" $ show c
        withStyle [FgColor c, Invert] $ liftIO $ printf " %-20s" $ show c
        liftIO $ putChar '\n'

specialExample :: IO ()
specialExample = runWithStyle [] $
  for_ [(Bold,NotBold),(Italic,NotItalic),(Under,NotUnder),(Invert,NotInvert),(Blink,NotBlink)] $ \(a, b) -> do
    setStyle [a]
    liftIO $ printf "%-20s" $ show a
    setStyle [b]
    liftIO $ printf " %-20s" $ show b
    liftIO $ putChar '\n'

stackExample :: IO ()
stackExample = do
  runWithStyle [] $ loop 0
  liftIO $ putChar '\n'
  where
    loop 8 = pure ()
    loop n = do
      setStyle [Save, BgColor $ Color256 n]
      liftIO $ putStr $ replicate (fromIntegral n) ' '
      loop (n + 1)
      liftIO $ putStr $ replicate (fromIntegral n) ' '
      setStyle [Restore]

basicExample :: IO ()
basicExample = runWithStyle [FgColor Blue] $ do
  withStyle [Bold] $ liftIO $ putStr "Bold Blue"

  setStyle [Save, Italic, BgColor Red]
  liftIO $ putStr "Italic Red"
  setStyle [Restore]

  setStyle [Under]
  liftIO $ putStr "Under Blue"
  setStyle [Reset]

  liftIO $ putStrLn "Normal output"

setStyleCodeExample :: IO ()
setStyleCodeExample = runStyleT Term8 $ do
  start <- setStyleCode [FgColor Green, Bold]
  end <- setStyleCode [Reset]
  liftIO $ putStrLn (start ++ "Green" ++ end)

applyStyleExample :: IO ()
applyStyleExample = runWithStyle [] $ do
  changeStyle [FgColor Blue] -- No escape sequence generated
  changeStyle [FgColor Red]
  applyStyle                 -- Escape sequences generated
  liftIO $ putStrLn "Red"

reduceExample :: IO ()
reduceExample = do
  for_ [0..255] $ \c -> do
    flip evalStateT (hDefaultStyle stdout Term256) $ withStyle [BgColor $ Color256 c] $ liftIO $ printf "%02x" c
    flip evalStateT (hDefaultStyle stdout Term8)   $ withStyle [BgColor $ Color256 c] $ liftIO $ printf "%02x" c
    liftIO $ putChar '\n'
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        flip evalStateT (hDefaultStyle stdout TermRGB) $ withStyle [BgColor c] $ liftIO $ printf "%20s" $ show c
        flip evalStateT (hDefaultStyle stdout Term256) $ withStyle [BgColor c] $ liftIO $ printf "%20s" $ show c
        flip evalStateT (hDefaultStyle stdout Term8)   $ withStyle [BgColor c] $ liftIO $ printf "%20s" $ show c
        liftIO $ putChar '\n'

main :: IO ()
main = do
  ansiColorsExample
  colors256Example
  rgbExample
  specialExample
  stackExample
  basicExample
  setStyleCodeExample
  applyStyleExample
  reduceExample
