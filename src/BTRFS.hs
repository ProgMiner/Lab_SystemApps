{-# LANGUAGE ForeignFunctionInterface #-}

module BTRFS (
    btrfsOpenFS,
    btrfsOpenFileFS,
    btrfsOpenPtrFS,
    btrfsStat,
    btrfsReadDir,
    btrfsRead,
    btrfsReadString,
    btrfsGetContents,
    btrfsGetContentsString,
    btrfsReadLink
) where

import Data.Word (Word8)
import Control.Monad ((<=<))
import System.IO (FilePath)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.MMap (mmapFileForeignPtr, Mode (ReadOnly))
import System.Posix.Internals (CStat, sizeof_stat)
import System.Posix.Types (COff (COff))
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString, withCString, peekCString, peekCStringLen)
import Foreign.C.Types (CSize (CSize), CInt (CInt), CChar (CChar))
import Foreign (ForeignPtr, Ptr, FunPtr, withForeignPtr, newForeignPtr,
    mallocForeignPtr, mallocForeignPtrArray, mallocForeignPtrBytes,
    plusPtr, nullPtr, peek, peekArray)

import Util ((<.>), throwErrnoFromResult)

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

-- int btrfs_stat(const struct btrfs *, const char *, struct stat *)
foreign import ccall "btrfs_stat" btrfsCStat :: Ptr Btrfs -> CString -> Ptr CStat -> IO CInt

-- int btrfs_readdir(
--         const struct btrfs * btrfs,
--         const char * filename,
--         size_t * length,
--         char *** contents
-- )
foreign import ccall "btrfs_readdir" btrfsCReadDir
    :: Ptr Btrfs
    -> CString
    -> Ptr CSize
    -> Ptr (Ptr CString)
    -> IO CInt

-- int btrfs_read(
--         const struct btrfs * btrfs,
--         const char * filename,
--         char * data,
--         size_t length,
--         off_t offset
-- )
foreign import ccall "btrfs_read" btrfsCRead
    :: Ptr Btrfs
    -> CString
    -> Ptr CChar
    -> CSize
    -> COff
    -> IO CInt

-- int btrfs_readlink(
--         const struct btrfs * btrfs,
--         const char * filename,
--         char * data,
--         size_t length
-- )
foreign import ccall "btrfs_readlink" btrfsCReadLink
    :: Ptr Btrfs
    -> CString
    -> Ptr CChar
    -> CSize
    -> IO CInt

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

btrfsStat
    :: BTRFS            -- BTRFS volume
    -> FilePath         -- path to file within volume
    -> IO (ForeignPtr CStat)
btrfsStat (_, btrfs) path = do
    stat <- mallocForeignPtrBytes sizeof_stat :: IO (ForeignPtr CStat)

    withForeignPtr btrfs (\pBtrfs ->
        withForeignPtr stat (\pStat ->
            withCString path (\cPath ->
                throwErrnoFromResult "btrfs_stat"
                    $ btrfsCStat pBtrfs cPath pStat)))

    return stat

btrfsReadDir
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> IO [String]
btrfsReadDir (_, btrfs) path = do
    contents <- mallocForeignPtr :: IO (ForeignPtr (Ptr CString))
    count <- mallocForeignPtr :: IO (ForeignPtr CSize)

    withForeignPtr count (\pCount ->
        withForeignPtr contents (\pContents -> do
            withForeignPtr btrfs (\pBtrfs ->
                withCString path (\cPath ->
                    throwErrnoFromResult "btrfs_readdir"
                        $ btrfsCReadDir pBtrfs cPath pCount pContents))

            contentsArrayLength <- fromIntegral <$> peek pCount :: IO Int
            contentsArray <- peek pContents :: IO (Ptr CString)

            cContents <- peekArray contentsArrayLength contentsArray :: IO [CString]
            mapM peekCString cContents))

btrfsRead'
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> Int          -- count of bytes to read
    -> Int          -- number of first byte in file to read
    -> IO (ForeignPtr CChar, Int)
btrfsRead' (_, btrfs) path length offset = do
    buf <- mallocForeignPtrArray length :: IO (ForeignPtr CChar)

    bytesRead <- fromIntegral <$>
        withForeignPtr buf (\pBuf ->
            withForeignPtr btrfs (\pBtrfs ->
                withCString path (\cPath ->
                    throwErrnoFromResult "btrfs_read"
                        $ btrfsCRead pBtrfs cPath pBuf
                            (fromIntegral length) (fromIntegral offset))))

    return (buf, bytesRead)

btrfsRead
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> Int          -- count of bytes to read
    -> Int          -- number of first byte in file to read
    -> IO [Word8]
btrfsRead btrfs path length offset = do
    (buf, bytesRead) <- btrfsRead' btrfs path length offset
    map fromIntegral <$> withForeignPtr buf (peekArray bytesRead)

btrfsReadString
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> Int          -- count of bytes to read
    -> Int          -- number of first byte in file to read
    -> IO String
btrfsReadString btrfs path length offset = do
    (buf, bytesRead) <- btrfsRead' btrfs path length offset
    withForeignPtr buf $ peekCStringLen . flip (,) bytesRead

btrfsGetContents
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> IO [Word8]
btrfsGetContents btrfs path = btrfsGetContents' 256 0 where
    btrfsGetContents' :: Int -> Int -> IO [Word8]
    btrfsGetContents' len off = do
        (buf, bytesRead) <- btrfsRead' btrfs path len off :: IO (ForeignPtr CChar, Int)
        bytes <- map fromIntegral <$> withForeignPtr buf (peekArray bytesRead) :: IO [Word8]

        if bytesRead == len
            then unsafeInterleaveIO $ (bytes ++) <$> btrfsGetContents' len (off + len)
            else return bytes

-- not lazy!
btrfsGetContentsString
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> IO String
btrfsGetContentsString btrfs path = btrfsGetContentsString' 256 where
    btrfsGetContentsString' :: Int -> IO String
    btrfsGetContentsString' len = do
        (buf, bytesRead) <- btrfsRead' btrfs path len 0 :: IO (ForeignPtr CChar, Int)

        if bytesRead == len
            then unsafeInterleaveIO $ btrfsGetContentsString' (2 * len)
            else withForeignPtr buf (peekCStringLen . flip (,) bytesRead) :: IO String

btrfsReadLink
    :: BTRFS        -- BTRFS volume
    -> FilePath     -- path to file within volume
    -> IO String
btrfsReadLink (_, btrfs) path = btrfsReadLink' 256 where
    btrfsReadLink' :: Int -> IO String
    btrfsReadLink' len = do
        buf <- mallocForeignPtrArray len :: IO (ForeignPtr CChar)

        withForeignPtr buf (\pBuf ->
            withForeignPtr btrfs (\pBtrfs ->
                withCString path (\cPath ->
                    throwErrnoFromResult "btrfs_readlink"
                        $ btrfsCReadLink pBtrfs cPath pBuf $ fromIntegral len)))

        content <- withForeignPtr buf peekCString :: IO String

        if length content == len
            then unsafeInterleaveIO $ btrfsReadLink' (2 * len)
            else return content
