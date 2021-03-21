{-# LANGUAGE ForeignFunctionInterface #-}

module BTRFS (btrfsOpenFS, btrfsOpenFileFS) where

import System.IO (FilePath)
import System.IO.MMap (mmapFileForeignPtr, Mode (ReadOnly))
import Foreign (ForeignPtr, Ptr, withForeignPtr, plusPtr, nullPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CSize (CSize))

-- struct btrfs
data Btrfs

type BTRFS = ForeignPtr Btrfs

-- struct btrfs * btrfs_openfs(const void *, size_t)
foreign import ccall "btrfs_openfs" btrfsCOpenFS :: Ptr () -> CSize -> IO (Ptr Btrfs)

-- void btrfs_delete(struct btrfs *)
foreign import ccall "btrfs_delete" btrfsCDelete :: Ptr Btrfs -> IO ()

btrfsOpenFS :: (Integral i) => ForeignPtr () -> i -> IO BTRFS
btrfsOpenFS ptr size = withForeignPtr ptr $ flip btrfsDoOpenFS (fromIntegral size)

btrfsOpenFileFS :: FilePath -> IO BTRFS
btrfsOpenFileFS path = do
    (ptr, offset, size) <- mmapFileForeignPtr path ReadOnly Nothing
    withForeignPtr ptr (\p -> btrfsDoOpenFS (plusPtr p offset) (fromIntegral size))

btrfsDoOpenFS :: Ptr () -> CSize -> IO BTRFS
btrfsDoOpenFS ptr length = do
    btrfs <- throwErrnoIfNull "btrfs_openfs" $ btrfsCOpenFS ptr length
    undefined
