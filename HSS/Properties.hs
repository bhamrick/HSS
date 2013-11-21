{-# LANGUAGE OverloadedStrings #-}
module HSS.Properties where

import Data.ByteString.Char8

import HSS.Datatypes

import HSS.Color

-- generic property restricted to ByteString names/values
prop :: ByteString -> ByteString -> CssM a
prop = AddProperty

color :: Color -> CssM a
color = AddProperty ("color" :: ByteString)
