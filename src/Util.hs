module Util ((<.>), throwErrnoFromResult) where

import Data.Functor ((<$>))
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import System.IO.Error (ioError)


(<.>)
    :: Functor f
    => (b -> c)
    -> (a -> f b)
    -> a
    -> f c
(<.>) f g a = f <$> g a

throwErrnoFromResult
    :: (Integral a)
    => String       -- location
    -> IO a         -- IO with result
    -> IO a
throwErrnoFromResult loc = flip (>>=) throwErrnoFromResult' where
    throwErrnoFromResult' :: (Integral a) => a -> IO a
    throwErrnoFromResult' ret | ret < 0   = ioError (errnoToIOError loc (Errno $ fromIntegral (-ret)) Nothing Nothing)
    throwErrnoFromResult' ret | otherwise = return ret
