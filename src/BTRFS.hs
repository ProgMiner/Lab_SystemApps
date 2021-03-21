{-# LANGUAGE ForeignFunctionInterface #-}

module BTRFS (btrfsOpenFS, btrfsOpenFileFS, btrfsOpenPtrFS) where

import Control.Monad ((<=<))
import System.IO (FilePath)
import System.IO.MMap (mmapFileForeignPtr, Mode (ReadOnly))
import Foreign (ForeignPtr, Ptr, FunPtr, withForeignPtr, newForeignPtr, plusPtr, nullPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CSize (CSize))

-- struct btrfs
data Btrfs

type BTRFS = ForeignPtr Btrfs

-- struct btrfs * btrfs_openfs(const void *, size_t)
foreign import ccall "btrfs_openfs" btrfsCOpenFS :: Ptr () -> CSize -> IO (Ptr Btrfs)

-- void (* btrfs_delete)(struct btrfs *)
foreign import ccall "&btrfs_delete" btrfsCDeletePtr :: FunPtr (Ptr Btrfs -> IO ())

btrfsOpenFS
    :: (Integral i)
    => ForeignPtr ()    -- pointer to a btrfs volume
    -> i                -- size of a volume
    -> IO BTRFS
btrfsOpenFS = flip $ flip withForeignPtr . flip btrfsOpenPtrFS . fromIntegral

-- change mmapFileForeignPtr to just Ptr due to too early unmap
btrfsOpenFileFS
    :: FilePath -- path to a file with a btrfs volume
    -> IO BTRFS
btrfsOpenFileFS path = do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadOnly Nothing
    withForeignPtr ptr (\p -> btrfsOpenPtrFS (plusPtr p offset) (fromIntegral size))

btrfsOpenPtrFS
    :: Ptr ()   -- pointer to a btrfs volume
    -> CSize    -- size of the volume
    -> IO BTRFS
btrfsOpenPtrFS = curry
    $ newForeignPtr btrfsCDeletePtr
    <=< throwErrnoIfNull "btrfs_openfs"
    . uncurry btrfsCOpenFS
