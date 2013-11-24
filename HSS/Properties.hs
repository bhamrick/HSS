{-# LANGUAGE OverloadedStrings #-}
module HSS.Properties where

import Data.ByteString.Char8

import HSS.Datatypes

import HSS.Color
import HSS.Length

-- generic property restricted to ByteString names/values
prop :: ByteString -> ByteString -> CssM a
prop = AddProperty

color :: Color -> CssM a
color = AddProperty ("color" :: ByteString)

-- Various length properties
borderBottomWidth :: Length -> CssM a
borderBottomWidth = AddProperty ("border-bottom" :: ByteString)

borderLeftWidth :: Length -> CssM a
borderLeftWidth = AddProperty ("border-left" :: ByteString)

borderRightWidth :: Length -> CssM a
borderRightWidth = AddProperty ("border-right" :: ByteString)

borderTopWidth :: Length -> CssM a
borderTopWidth = AddProperty ("border-top" :: ByteString)
