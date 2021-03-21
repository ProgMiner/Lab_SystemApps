{-# LANGUAGE ForeignFunctionInterface #-}

module BTRFS (btrfsOpenFS, btrfsOpenFileFS, btrfsOpenPtrFS) where

import Control.Monad ((<=<))
import System.IO (FilePath)
import System.IO.MMap (mmapFileForeignPtr, Mode (ReadOnly))
import Foreign (ForeignPtr, Ptr, FunPtr, withForeignPtr, newForeignPtr, plusPtr, nullPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CSize (CSize))

import Util ((<.>))

-- struct btrfs
data Btrfs

type OnlyBTRFS = ForeignPtr Btrfs

-- we save ForeignPtr to BTRFS in order to tell GHC that it must not be unmapped
type BTRFS =
    ( ForeignPtr () -- pointer to data
    , OnlyBTRFS     -- pointer to btrfs struct
    )

-- struct btrfs * btrfs_openfs(const void *, size_t)
foreign import ccall "btrfs_openfs" btrfsCOpenFS :: Ptr () -> CSize -> IO (Ptr Btrfs)

-- void (* btrfs_delete)(struct btrfs *)
foreign import ccall "&btrfs_delete" btrfsCDeletePtr :: FunPtr (Ptr Btrfs -> IO ())

btrfsOpenFS
    :: (Integral i)
    => ForeignPtr ()    -- pointer to a btrfs volume
    -> i                -- size of a volume
    -> IO BTRFS
btrfsOpenFS ptr = ((,) ptr) <.> f ptr where
    f :: (Integral i) => ForeignPtr () -> i -> IO OnlyBTRFS
    f = flip $ flip withForeignPtr . flip btrfsOpenPtrFS . fromIntegral

btrfsOpenFileFS
    :: FilePath -- path to a file with a btrfs volume
    -> IO BTRFS
btrfsOpenFileFS path = do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadOnly Nothing
    ((,) ptr) <$> withForeignPtr ptr (flip btrfsOpenPtrFS (fromIntegral size) . flip plusPtr offset)

btrfsOpenPtrFS
    :: Ptr ()   -- pointer to a btrfs volume
    -> CSize    -- size of the volume
    -> IO OnlyBTRFS
btrfsOpenPtrFS = curry
    $ newForeignPtr btrfsCDeletePtr
    <=< throwErrnoIfNull "btrfs_openfs"
    . uncurry btrfsCOpenFS
