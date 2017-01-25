{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.Style
import Data.Foldable
import Text.Printf
import Data.Monoid ((<>))

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
ansiColorsExample = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "ANSI Example\n"
  for_ ansiColors $ \c -> do
    printStyled term $ Bg c $ Value $ printf "%-15s" $ show c
    printStyled term $ Fg c $ Value $ printf "%-15s" $ show c
    printStyled term $ Bg c $ Set Invert $ Value $ printf " %-15s" $ show c
    printStyled term $ Fg c $ Set Invert $ Value $ printf " %-15s" $ show c
    printStyledS term $ Bg c $ Value $ printf "%-15s" $ show c
    printStyledS term $ Fg c $ Value $ printf "%-15s" $ show c
    printStyledS term $ Bg c $ Set Invert $ Value $ printf " %-15s" $ show c
    printStyledS term $ Fg c $ Set Invert $ Value $ printf " %-15s" $ show c
    putChar '\n'

colors256Example :: IO ()
colors256Example = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "Color256 Example\n"
  for_ [0..255] $ \c -> do
    printStyled term $ Bg (Color256 c) $ Value $ printf "%02x" c
    printStyled term $ Fg (Color256 c) $ Value $ printf " %02x" c
    printStyled term $ Bg (Color256 c) $ Set Invert $ Value $ printf " %02x" c
    printStyled term $ Fg (Color256 c) $ Set Invert $ Value $ printf " %02x" c
    putChar '\n'

rgbExample :: IO ()
rgbExample = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "RGB Example\n"
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        printStyled term $ Bg c $ Value $ printf "%-20s" $ show c
        printStyled term $ Fg c $ Value $ printf " %-20s" $ show c
        printStyled term $ Bg c $ Set Invert $ Value $ printf " %-20s" $ show c
        printStyled term $ Fg c $ Set Invert $ Value $ printf " %-20s" $ show c
        putChar '\n'

specialExample :: IO ()
specialExample = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "Special Example\n"
  for_ [Bold,Italic,Under,Invert,Blink] $ \a -> do
    printStyled term $
      Set a (Value (printf "%-20s" $ show a) <>
             Unset a (Value $ printf " %-20s" $ "Not" ++ show a) <>
             Value (printf "%-20s" $ show a))
    putChar '\n'

stackExample :: IO ()
stackExample = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "Stack Example\n"
  printStyledS term $ loop 0
  putChar '\n'
  where
    loop 8 = mempty
    loop n =
      Bg (Color256 n) $
        Value (replicate (fromIntegral n) ' ') <>
        loop (n + 1) <>
        Value (replicate (fromIntegral n) ' ')

basicExample :: IO ()
basicExample = do
  term <- getTerm
  printStyledS term $ Set Under $ Set Bold "Basic Example\n"
  printStyledS term $ Set Bold "Bold"
  printStyledS term $ Set Italic $ Bg Red "Italic Red"
  printStyledS term $ Set Under "Under"
  putChar '\n'

reduceExample :: IO ()
reduceExample = do
  printStyledS Term8 $ Set Under $ Set Bold "Reduction Example\n"
  for_ [0..255] $ \c -> do
    printStyled Term256 $ Bg (Color256 c) $ Value $ printf "%02x" c
    printStyled Term8   $ Bg (Color256 c) $ Value $ printf "%02x" c
    putChar '\n'
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        printStyled TermRGB $ Bg c $ Value $ printf "%20s" $ show c
        printStyled Term256 $ Bg c $ Value $ printf "%20s" $ show c
        printStyled Term8   $ Bg c $ Value $ printf "%20s" $ show c
        putChar '\n'

main :: IO ()
main = do
  putStrLn "\n"
  ansiColorsExample
  colors256Example
  rgbExample
  specialExample
  stackExample
  basicExample
  reduceExample
  putStrLn "\n"
