{-# LANGUAGE OverloadedStrings #-}
module HSS.Properties where

import HSS.Datatypes
import Data.ByteString.Char8

-- generic property restricted to ByteString names
prop :: PropertyValue b => ByteString -> b -> CssM a
prop = AddProperty

color :: ByteString -> CssM a
color = prop "color"
