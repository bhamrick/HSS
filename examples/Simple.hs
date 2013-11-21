{-# LANGUAGE OverloadedStrings #-}
import HSS.Datatypes
import qualified HSS.Selectors as S
import qualified HSS.Properties as P

import HSS.Renderer.Simple

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

template :: Css
template = do
    S.tag "a" $ do
        P.color "#5500f3"
        P.prop "text-align" ("center" :: ByteString)

main :: IO ()
main = BS.putStrLn $ renderCss template
