{-#LANGUAGE CPP#-}
{-#LANGUAGE ForeignFunctionInterface#-}
{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE RecordWildCards#-}

module System.SysInfo
  ( 

   -- * Usage
   -- $usage

    sysInfo
  , SysInfo(..)
  , Loads(..)
  ) where

#ifndef NO_SYSINFO
#include <sys/sysinfo.h>
#endif

import Foreign.C
import Foreign.Ptr
import Foreign.C.Error
import Foreign.Storable
import Foreign.Marshal.Alloc

-- $usage
--
-- @
-- λ> import System.SysInfo
-- λ> val <- sysInfo
-- λ> either (\_ -> "sysinfo failed") show val
-- "SysInfo {uptime = 121149, loads = Loads {sloads = [91200,80736,82592]}, totalram = 12286611456, freeram = 967655424, sharedram = 63033344, bufferram = 838983680, totalswap = 8261726208, freeswap = 8259276800, procs = 418, totalhigh = 0, freehigh = 0, memUnit = 1}"
-- @

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- | Data type representing system informating
data SysInfo = SysInfo
  { uptime :: CLong -- ^ Seconds since boot
  , loads :: Loads -- ^ 1, 5, and 15 minute load averages 
  , totalram :: CULong -- ^ Total usable main memory size 
  , freeram :: CULong -- ^ Available memory size 
  , sharedram :: CULong -- ^ Amount of shared memory 
  , bufferram :: CULong -- ^ Memory used by buffers
  , totalswap :: CULong -- ^ Total swap space size
  , freeswap :: CULong -- ^ swap space still available
  , procs :: CUShort -- ^ Number of current processes
  , totalhigh :: CULong -- ^ Total high memory size
  , freehigh :: CULong -- ^ Available high memory size
  , memUnit :: CUInt -- ^ Memory unit size in bytes
  } deriving (Show, Eq, Ord)

newtype Loads = Loads
  { sloads :: [CULong]
  } deriving (Show, Eq, Ord)

instance Storable Loads where
  sizeOf _ = (sizeOf (undefined :: CULong)) * 3
  alignment _ = alignment (undefined :: CULong)
  peek ptr = do
    (vals :: [CULong]) <- mapM (peekElemOff ptr') index
    return $ Loads vals
    where
      ptr' = castPtr ptr
      index = [0, 1, 2]
  poke ptr (Loads val) = mapM_ (\(v, i) -> pokeElemOff ptr' i v) (zip val index)
    where
      (ptr' :: Ptr CULong) = castPtr ptr
      index = [0, 1, 2]

#ifndef NO_SYSINFO
instance Storable SysInfo where
  sizeOf _ = (#size struct sysinfo)
  alignment _ = (#alignment struct sysinfo)
  peek ptr = do
    uptime <- (#peek struct sysinfo, uptime) ptr
    loads <- (#peek struct sysinfo, loads) ptr
    totalram <- (#peek struct sysinfo, totalram) ptr
    freeram <- (#peek struct sysinfo, freeram) ptr
    sharedram <- (#peek struct sysinfo, sharedram) ptr
    bufferram <- (#peek struct sysinfo, bufferram) ptr
    totalswap <- (#peek struct sysinfo, totalswap) ptr
    freeswap <- (#peek struct sysinfo, freeswap) ptr
    procs <- (#peek struct sysinfo, procs) ptr
    totalhigh <- (#peek struct sysinfo, totalhigh) ptr
    freehigh <- (#peek struct sysinfo, freehigh) ptr
    memUnit <- (#peek struct sysinfo, mem_unit) ptr
    return $
      SysInfo
      { ..
      }
  poke ptr (SysInfo {..}) = do
    (#poke struct sysinfo, uptime) ptr uptime
    (#poke struct sysinfo, loads) ptr loads
    (#poke struct sysinfo, totalram) ptr totalram
    (#poke struct sysinfo, freeram) ptr freeram
    (#poke struct sysinfo, sharedram) ptr sharedram
    (#poke struct sysinfo, bufferram) ptr bufferram
    (#poke struct sysinfo, totalswap) ptr totalswap
    (#poke struct sysinfo, freeswap) ptr freeswap
    (#poke struct sysinfo, procs) ptr procs
    (#poke struct sysinfo, totalhigh) ptr totalhigh
    (#poke struct sysinfo, freehigh) ptr freehigh
    (#poke struct sysinfo, mem_unit) ptr memUnit

foreign import ccall safe "sysinfo" c_sysinfo ::
               Ptr SysInfo -> IO CInt

-- | Function for getting system information. Internally it uses the
-- Linux system call sysinfo to get the system statistics.
sysInfo :: IO (Either Errno SysInfo)
sysInfo = do
  (sptr :: Ptr SysInfo) <- malloc
  res <- c_sysinfo sptr
  if (res == 0)
    then do
      val <- peek sptr
      free sptr
      return $ Right val
    else do
      free sptr
      err <- getErrno
      return $ Left err
#else
-- | Functor for getting system information. On non-Lonux sytems always fails
-- with @E_NODATA@.
sysInfo :: IO (Either Errno SysInfo)
sysInfo = return (Left eNODATA)
#endif
