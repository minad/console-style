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
-- > basicExample = do
-- >  term <- getTerm
-- >  printStyledS term $ Set Under $ Set Bold "Basic Example\n"
-- >  printStyledS term $ Set Bold "Bold"
-- >  printStyledS term $ Set Italic $ Bg Red "Italic Red"
-- >  printStyledS term $ Set Under "Under"
-- >  putChar '\n'
--
-- For many more examples, see the
-- <https://github.com/minad/console-style/blob/master/example.hs example.hs> file.
-----------------------------------------------------------

module System.Console.Style (
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

import System.Console.Style.Nested
