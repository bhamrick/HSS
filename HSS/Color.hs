module HSS.Color where

import HSS.Datatypes

import Control.Monad
import Data.ByteString.Char8
import Data.Maybe
import Data.String

import Text.Printf

data Color = RGB Integer Integer Integer
           | RGBA Integer Integer Integer Float
           | HSL Integer Float Float
           | HSLA Integer Float Float Float
           deriving (Eq, Show, Ord)

clamp l h x = if x < l then l else if x > h then h else x

percent :: Float -> String
percent x = printf "%f%%" (100.0 * x)

instance PropertyValue Color where
    displayValue (RGB r g b) = pack $
        printf "#%02x%02x%02x" (clamp 0 255 r) (clamp 0 255 g) (clamp 0 255 b)
    displayValue (RGBA r g b a) = pack $
        printf "rgba(%d,%d,%d,%f)" (clamp 0 255 r) (clamp 0 255 g) (clamp 0 255 b) a
    displayValue (HSL h s l) = pack $
        printf "hsl(%d,%s,%s)" (clamp 0 360 h) (percent s) (percent l)
    displayValue (HSLA h s l a) = pack $
        printf "hsla(%d,%s,%s,%f)" (clamp 0 360 h) (percent s) (percent l) a

readHex :: Monad m => String -> m Integer
readHex s = do
    digits <- mapM readHexChar s
    return $ Prelude.foldl (\x y -> 16 * x + y) 0 digits

readHexChar :: Monad m => Char -> m Integer
readHexChar '0' = return 0
readHexChar '1' = return 1
readHexChar '2' = return 2
readHexChar '3' = return 3
readHexChar '4' = return 4
readHexChar '5' = return 5
readHexChar '6' = return 6
readHexChar '7' = return 7
readHexChar '8' = return 8
readHexChar '9' = return 9
readHexChar 'a' = return 10
readHexChar 'A' = return 10
readHexChar 'b' = return 11
readHexChar 'B' = return 11
readHexChar 'c' = return 12
readHexChar 'C' = return 12
readHexChar 'd' = return 13
readHexChar 'D' = return 13
readHexChar 'e' = return 14
readHexChar 'E' = return 14
readHexChar 'f' = return 15
readHexChar 'F' = return 15
readHexChar _ = fail "Invalid hex character"

parseColor :: Monad m => String -> m Color
parseColor s = parseColor_ $ mapMaybe readHexChar s
parseColor_ digits = case digits of
    (r:g:b:[]) -> return $ RGB (0x11*r) (0x11*g) (0x11*b)
    (r1:r2:g1:g2:b1:b2:[]) -> return $ RGB (16*r1+r2) (16*g1+g2) (16*b1+b2)
    _ -> fail "Unknown color format"

instance IsString Color where
    fromString = fromJust . parseColor

-- Predefined colors
aliceBlue               = RGB 0xF0 0xF8 0xFF
antiqueWhite            = RGB 0xFA 0xEB 0xD7
aqua                    = RGB 0x00 0xFF 0xFF
aquamarine              = RGB 0x7F 0xFF 0xD4
azure                   = RGB 0xF0 0xFF 0xFF
beige                   = RGB 0xF5 0xF5 0xDC
bisque                  = RGB 0xFF 0xE4 0xC4
black                   = RGB 0x00 0x00 0x00
blanchedAlmond          = RGB 0xFF 0xEB 0xCD
blue                    = RGB 0x00 0x00 0xFF
blueViolet              = RGB 0x8A 0x2B 0xE2
brown                   = RGB 0xA5 0x2A 0x2A
burlyWood               = RGB 0xDE 0xB8 0x87
cadetBlue               = RGB 0x5F 0x9E 0xA0
chartreuse              = RGB 0x7F 0xFF 0x00
chocolate               = RGB 0xD2 0x69 0x1E
coral                   = RGB 0xFF 0x7F 0x50
cornflowerBlue          = RGB 0x64 0x95 0xED
cornsilk                = RGB 0xFF 0xF8 0xDC
crimson                 = RGB 0xDC 0x14 0x3C
cyan                    = RGB 0x00 0xFF 0xFF
darkBlue                = RGB 0x00 0x00 0x8B
darkCyan                = RGB 0x00 0x8B 0x8B
darkGoldenRod           = RGB 0xB8 0x86 0x0B
darkGray                = RGB 0xA9 0xA9 0xA9
darkGreen               = RGB 0x00 0x64 0x00
darkKhaki               = RGB 0xBD 0xB7 0x6B
darkMagenta             = RGB 0x8B 0x00 0x8B
darkOliveGreen          = RGB 0x55 0x6B 0x2F
darkOrange              = RGB 0xFF 0x8C 0x00
darkOrchid              = RGB 0x99 0x32 0xCC
darkRed                 = RGB 0x8B 0x00 0x00
darkSalmon              = RGB 0xE9 0x96 0x7A
darkSeaGreen            = RGB 0x8F 0xBC 0x8F
darkSlateBlue           = RGB 0x48 0x3D 0x8B
darkSlateGray           = RGB 0x2F 0x4F 0x4F
darkTurquoise           = RGB 0x00 0xCE 0xD1
darkVoilet              = RGB 0x94 0x00 0xD3
deepPink                = RGB 0xFF 0x14 0x93
deepSkyBlue             = RGB 0x00 0xBF 0xFF
dimGray                 = RGB 0x69 0x69 0x69
dodgerBlue              = RGB 0x1E 0x90 0xFF
fireBrick               = RGB 0xB2 0x22 0x22
floralWhite             = RGB 0xFF 0xFA 0xF0
forestGreen             = RGB 0x22 0x8B 0x22
fuchsia                 = RGB 0xFF 0x00 0xFF
gainsboro               = RGB 0xDC 0xDC 0xDC
ghostWhite              = RGB 0xF8 0xF8 0xFF
gold                    = RGB 0xFF 0xD7 0x00
goldenRod               = RGB 0xDA 0xA5 0x20
gray                    = RGB 0x80 0x80 0x80
green                   = RGB 0x00 0x80 0x00
greenYellow             = RGB 0xAD 0xFF 0x2F
honeyDew                = RGB 0xF0 0xFF 0xF0
hotPink                 = RGB 0xFF 0x69 0xB4
indianRed               = RGB 0xCD 0x5C 0x5C
indigo                  = RGB 0x4B 0x00 0x82
ivory                   = RGB 0xFF 0xFF 0xF0
khaki                   = RGB 0xF0 0xE6 0x8C
lavender                = RGB 0xE6 0xE6 0xFA
lavenderBlush           = RGB 0xFF 0xF0 0xF5
lawnGreen               = RGB 0x7C 0xFC 0x00
lemonChiffon            = RGB 0xFF 0xFA 0xCD
lightBlue               = RGB 0xAD 0xD8 0xE6
lightCoral              = RGB 0xF0 0x80 0x80
lightCyan               = RGB 0xE0 0xFF 0xFF
lightGoldenRodYellow    = RGB 0xFA 0xFA 0xD2
lightGray               = RGB 0xD3 0xD3 0xD3
lightGreen              = RGB 0x90 0xEE 0x90
lightPink               = RGB 0xFF 0xB6 0xC1
lightSalmon             = RGB 0xFF 0xA0 0x7A
lightSeaGreen           = RGB 0x20 0xB2 0xAA
lightSkyBlue            = RGB 0x87 0xCE 0xFA
lightSlateGray          = RGB 0x77 0x88 0x99
lightSteelBlue          = RGB 0xB0 0xC4 0xDE
lightYellow             = RGB 0xFF 0xFF 0xE0
lime                    = RGB 0x00 0xFF 0x00
limeGreen               = RGB 0x32 0xCD 0x32
linen                   = RGB 0xFA 0xF0 0xE6
magenta                 = RGB 0xFF 0x00 0xFF
maroon                  = RGB 0x80 0x00 0x00
mediumAquaMarine        = RGB 0x66 0xCD 0xAA
mediumBlue              = RGB 0x00 0x00 0xCD
mediumOrchid            = RGB 0xBA 0x55 0xD3
mediumPurple            = RGB 0x93 0x70 0xDB
mediumSeaGreen          = RGB 0x3C 0xB3 0x71
mediumSlateBlue         = RGB 0x7B 0x68 0xEE
mediumSpringGreen       = RGB 0x00 0xFA 0x9A
mediumTurquoise         = RGB 0x48 0xD1 0xCC
mediumVioletRed         = RGB 0xC7 0x15 0x85
midnightBlue            = RGB 0x19 0x19 0x70
mintCream               = RGB 0xF5 0xFF 0xFA
mistyRose               = RGB 0xFF 0xE4 0xE1
moccasin                = RGB 0xFF 0xE4 0xB5
navajoWhite             = RGB 0xFF 0xDE 0xAD
navy                    = RGB 0x00 0x00 0x80
oldLace                 = RGB 0xFD 0xF5 0xE6
olive                   = RGB 0x80 0x80 0x00
oliveDrab               = RGB 0x6B 0x8E 0x23
orange                  = RGB 0xFF 0xA5 0x00
orangeRed               = RGB 0xFF 0x45 0x00
orchid                  = RGB 0xDA 0x70 0xD6
paleGoldenRod           = RGB 0xEE 0xE8 0xAA
paleGreen               = RGB 0x98 0xFB 0x98
paleTurquoise           = RGB 0xAF 0xEE 0xEE
paleVioletRed           = RGB 0xDB 0x70 0x93
papayaWhip              = RGB 0xFF 0xEF 0xD5
peachPuff               = RGB 0xFF 0xDA 0xB9
peru                    = RGB 0xCD 0x85 0x3F
pink                    = RGB 0xFF 0xC0 0xCB
plum                    = RGB 0xDD 0xA0 0xDD
powderBlue              = RGB 0xB0 0xE0 0xE6
purple                  = RGB 0x80 0x00 0x80
red                     = RGB 0xFF 0x00 0x00
rosyBrown               = RGB 0xBC 0x8F 0x8F
royalBlue               = RGB 0x41 0x69 0xE1
saddleBrown             = RGB 0x8B 0x45 0x13
salmon                  = RGB 0xFA 0x80 0x72
sandyBrown              = RGB 0xF4 0xA4 0x60
seaGreen                = RGB 0x2E 0x8B 0x57
seaShell                = RGB 0xFF 0xF5 0xEE
sienna                  = RGB 0xA0 0x52 0x2D
silver                  = RGB 0xC0 0xC0 0xC0
skyBlue                 = RGB 0x87 0xCE 0xEB
slateBlue               = RGB 0x6A 0x5A 0xCD
slateGray               = RGB 0x70 0x80 0x90
snow                    = RGB 0xFF 0xFA 0xFA
springGreen             = RGB 0x00 0xFF 0x7F
steelBlue               = RGB 0x46 0x82 0xB4
tan                     = RGB 0xD2 0xB4 0x8C
teal                    = RGB 0x00 0x80 0x80
thistle                 = RGB 0xD8 0xBF 0xD8
tomato                  = RGB 0xFF 0x63 0x47
turquoise               = RGB 0x40 0xE0 0xD0
violet                  = RGB 0xEE 0x82 0xEE
wheat                   = RGB 0xF5 0xDE 0xB3
white                   = RGB 0xFF 0xFF 0xFF
whiteSmoke              = RGB 0xF5 0xF5 0xF5
yellow                  = RGB 0xFF 0xFF 0x00
yellowGreen             = RGB 0x9A 0xCD 0x32
