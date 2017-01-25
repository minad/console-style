module System.Console.Style.Style (
  StyleState
  , defaultStyleState
  , Style(..)
  , defaultStyle
) where

import System.Console.Style.Color
import Data.List.NonEmpty (NonEmpty)

data Style = Style
  { styleBold   :: !Bool
  , styleItalic :: !Bool
  , styleUnder  :: !Bool
  , styleInvert :: !Bool
  , styleBlink  :: !Bool
  , styleFg     :: !Color
  , styleBg     :: !Color
  } deriving (Eq, Ord, Show)

type StyleState = (Style, NonEmpty Style)

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

defaultStyleState :: StyleState
defaultStyleState = (defaultStyle, pure defaultStyle)
