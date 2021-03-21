module Util ((<.>)) where

import Data.Functor ((<$>))


(<.>)
    :: Functor f
    => (b -> c)
    -> (a -> f b)
    -> a
    -> f c
(<.>) f g a = f <$> g a
