{-# LANGUAGE OverloadedStrings #-}
module HSS.Renderer.Simple where

import Prelude
import qualified Prelude as P

import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

import Data.Maybe
import Data.Monoid

import HSS.Datatypes

renderCss :: CssM a -> ByteString
renderCss = go []

go :: [Selector] -> CssM a -> ByteString
go _ Empty = ""
go sels (AddRule s c) = renderBlock (s:sels) c `mappend`
                        go (s:sels) c
go sels (Append c1 c2) = go sels c1 `mappend`
                         go sels c2
go sels (AddProperty n v) = ""

renderBlock :: [Selector] -> CssM a -> ByteString
renderBlock sels c = case renderBlockProperties c of
    "" -> ""
    props -> renderSelectorList sels `mappend`
             "{" `mappend`
             props `mappend`
             "}"

-- Follow Appends but not AddRules to find all
-- properties in a block and render the properties
-- that should live in that block
renderBlockProperties :: CssM a -> ByteString
renderBlockProperties Empty = ""
renderBlockProperties (AddRule _ _) = ""
renderBlockProperties (Append c1 c2) = renderBlockProperties c1 `mappend`
                                       renderBlockProperties c2
renderBlockProperties (AddProperty n v) = renderProperty n v

renderProperty n v = displayName n `mappend`
                     ":" `mappend`
                     displayValue v `mappend`
                     ";"

renderSelector :: Selector -> ByteString
renderSelector = renderTag `mappend`
                 renderClasses `mappend`
                 renderIds `mappend`
                 renderPseudoElement

renderTag = fromMaybe "" . s_tag
renderClasses = mconcat . (P.map (cons '.')) . s_classes
renderIds = mconcat . (P.map (cons '#')) . s_ids
renderPseudoElement s = case s_pseudo_element s of
    Nothing -> ""
    Just x -> cons ':' x

renderSelectorList :: [Selector] -> ByteString
renderSelectorList [] = "*"
renderSelectorList (s:[]) = renderSelector s
renderSelectorList (s:ss) = renderSelector s `mappend`
                            " " `mappend`
                            renderSelectorList ss
