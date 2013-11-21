{-# LANGUAGE OverloadedStrings #-}
import HSS.Datatypes
import HSS.Selectors
import qualified HSS.Selectors as S
import HSS.Properties
import qualified HSS.Properties as P

import HSS.Renderer.Simple

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

template :: Css
template = do
    S.tag "a" $ do
        P.color "#5500f3"
        P.prop "text-align" "center"
    S.tag "p" & S.cls "hi" $ do
        P.color "#000"

main :: IO ()
main = BS.putStrLn $ renderCss template
