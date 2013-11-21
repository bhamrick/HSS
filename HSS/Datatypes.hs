{-# LANGUAGE ExistentialQuantification #-}

module HSS.Datatypes where

import Data.ByteString.Char8
import Data.String

import Control.Applicative
import Control.Monad
import Data.Monoid

class PropertyName a where
    displayName :: a -> ByteString

instance PropertyName ByteString where
    displayName = id

class PropertyValue a where
    displayValue :: a -> ByteString

instance PropertyValue ByteString where
    displayValue = id

-- TODO: expand to include all valid css selectors
data Selector = Root |
                Selector { s_tag :: Maybe ByteString
                         , s_classes :: [ByteString]
                         , s_ids :: [ByteString]
                         , s_pseudo_element :: Maybe ByteString
                         }
    deriving (Eq, Show, Ord)

combine :: Selector -> Selector -> Selector
combine Root _ = Root
combine _ Root = Root
combine s1 s2 = Selector
    { s_tag = (s_tag s1) <|> (s_tag s2)
    , s_classes = (s_classes s1) ++ (s_classes s2)
    , s_ids = (s_ids s1) ++ (s_ids s2)
    , s_pseudo_element = (s_pseudo_element s1) <|> (s_pseudo_element s2)
    }

instance Monoid Selector where
    mempty = Selector { s_tag = Nothing
                      , s_classes = []
                      , s_ids = []
                      , s_pseudo_element = Nothing
                      }
    mappend = combine

data CssM a = Empty
            | forall b. AddRule Selector (CssM b)
            | forall b c. Append (CssM b) (CssM c)
            | forall b c. (PropertyName b, PropertyValue c) => AddProperty b c

type Css = CssM ()

instance Monoid a => Monoid (CssM a) where
    mempty = Empty
    mappend = Append

instance Monad CssM where
    return _ = Empty
    (>>) = Append
    c1 >>= f = c1 >> f (error "_|_")
