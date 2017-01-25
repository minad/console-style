module System.Console.Style.Style (
  Style(..)
  , defaultStyle
  , setAttr, setBg, setFg
  , resetStyle, saveStyle, restoreStyle
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

type StyleStack = (Style, [Style])

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

setAttr :: Attribute -> Bool -> Style -> Style
setAttr Bold   b s = s { styleBold   = b }
setAttr Italic b s = s { styleItalic = b }
setAttr Under  b s = s { styleUnder  = b }
setAttr Invert b s = s { styleInvert = b }
setAttr Blink  b s = s { styleBlink  = b }

setBg, setFg :: Color -> Style -> Style
setBg c s = s { styleBg = c }
setFg c s = s { styleFg = c }

resetStyle, saveStyle, restoreStyle :: StyleStack -> StyleStack
resetStyle   (_, ys)     = (defaultStyle, ys)
saveStyle    (y, ys)     = (y, y:ys)
restoreStyle (_, [])     = (defaultStyle, [])
restoreStyle (_, z : zs) = (z, zs)
