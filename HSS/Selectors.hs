{-# LANGUAGE FlexibleInstances #-}
module HSS.Selectors where

import Control.Applicative
import Data.ByteString.Char8
import Data.Monoid
import Data.String

import HSS.Datatypes

-- Allow overloading for selector `literals'
class IsSelector a where
    fromSelector :: Selector -> a

-- Trivial instance
instance IsSelector Selector where
    fromSelector = id

-- Allow `selectors' to take an argument and become Css chunks
instance IsSelector (CssM b -> CssM a) where
    fromSelector s = AddRule s

-- Combining operator
(&) :: IsSelector a => Selector -> Selector -> a
s1 & s2 = fromSelector $ s1 `mappend` s2

-- Basic selectors
tag :: IsSelector a => ByteString -> a
tag s = fromSelector $ Selector { s_tag = Just s
                                , s_classes = []
                                , s_ids = []
                                , s_pseudo_element = Nothing
                                }

cls s = fromSelector $ Selector { s_tag = Nothing
                                , s_classes = [s]
                                , s_ids = []
                                , s_pseudo_element = Nothing
                                }

id_ s = fromSelector $ Selector { s_tag = Nothing
                                , s_classes = []
                                , s_ids = [s]
                                , s_pseudo_element = Nothing
                                }

pseudo_element s = fromSelector $ Selector { s_tag = Nothing
                                           , s_classes = []
                                           , s_ids = []
                                           , s_pseudo_element = Just s
                                           }
