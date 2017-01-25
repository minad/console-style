module System.Console.Style.Style (
  StyleState
  , Style(..)
  , defaultStyle
) where

import System.Console.Style.Color

data Style = Style
  { styleBold   :: !Bool
  , styleItalic :: !Bool
  , styleUnder  :: !Bool
  , styleInvert :: !Bool
  , styleBlink  :: !Bool
  , styleFg     :: !Color
  , styleBg     :: !Color
  } deriving (Eq, Ord, Show)

type StyleState = (Style, Style, [Style])

defaultStyle :: Style
defaultStyle = Style
  { styleBold   = False
  , styleItalic = False
  , styleInvert = False
  , styleUnder  = False
  , styleBlink  = False
  , styleFg     = DefaultColor
  , styleBg     = DefaultColor
  }
