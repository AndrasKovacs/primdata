{-# language UnboxedTuples, TypeOperators, MagicHash, CPP, RankNTypes, TypeApplications,
             ScopedTypeVariables, AllowAmbiguousTypes #-}

module Data.Flat (
  Flat(..), size
  ) where

{-|
A class for types which can be naturally represented as uniform-sized pointer-free
values.
-}

import GHC.Exts
import GHC.Int
import GHC.Word
import Data.MachDeps

class Flat a where

  -- | Size of values of type @a@.
  size# :: Proxy# a -> Int#

  -- | Read a value from the array. The offset is in elements of type
  -- @a@ rather than in bytes.
  indexByteArray# :: ByteArray# -> Int# -> a

  -- | Read a value from the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to the mutable array. The offset is in elements of type
  -- @a@ rather than in bytes.
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  -- | Read a value from a memory position given by an address and an offset.
  -- The memory block the address refers to must be immutable. The offset is in
  -- elements of type @a@ rather than in bytes.
  indexOffAddr# :: Addr# -> Int# -> a

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  readOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of type @a@ rather than in bytes.
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

  -- | Read a value from an array. The offset is in bytes.
  indexWord8ArrayAs# :: ByteArray# -> Int# -> a

  -- | Read a value from a mutable array. The offset is in bytes.
  readWord8ArrayAs# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a bytearray. The offset is in bytes.
  writeWord8ArrayAs# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

size :: forall a. Flat a => Int
size = I# (size# @a proxy#)
{-# inline size #-}

#define derivePrim(ty, ctr, sz, idx_arr, rd_arr, wr_arr, idx_addr, rd_addr, wr_addr, idx_as, read_as, write_as) \
instance Flat (ty) where {                                 \
  size# _ = (case sz of I# sz -> sz)                       \
; indexByteArray# arr i = ctr (idx_arr arr i)              \
; readByteArray#  arr i s = case rd_arr arr i s of         \
                        { (# s1, x #) -> (# s1, ctr x #) } \
; writeByteArray# arr i (ctr x) s = wr_arr arr i x s       \
; indexOffAddr# addr i = ctr (idx_addr addr i)             \
; readOffAddr#  addr i s = case rd_addr addr i s of        \
                        { (# s1, x #) -> (# s1, ctr x #) } \
; writeOffAddr# addr i (ctr x) s = wr_addr addr i x s      \
; indexWord8ArrayAs# arr i  = ctr (idx_as arr i)           \
; readWord8ArrayAs# arr i s = case read_as arr i s of      \
                        { (# s1, x #) -> (# s, ctr x #) }  \
; writeWord8ArrayAs# arr i (ctr x) s = write_as arr i x s  \
; {-# inline size# #-}                                     \
; {-# inline indexByteArray# #-}                           \
; {-# inline readByteArray# #-}                            \
; {-# inline writeByteArray# #-}                           \
; {-# inline indexOffAddr# #-}                             \
; {-# inline readOffAddr# #-}                              \
; {-# inline writeOffAddr# #-}                             \
; {-# inline indexWord8ArrayAs# #-}                        \
; {-# inline readWord8ArrayAs# #-}                         \
; {-# inline writeWord8ArrayAs# #-}                        \
}


derivePrim(Int, I#, sIZEOF_INT,
           indexIntArray#, readIntArray#, writeIntArray#,
           indexIntOffAddr#, readIntOffAddr#, writeIntOffAddr#,
           indexWord8ArrayAsInt#, readWord8ArrayAsInt#, writeWord8ArrayAsInt#)
derivePrim(Word, W#, sIZEOF_WORD,
           indexWordArray#, readWordArray#, writeWordArray#,
           indexWordOffAddr#, readWordOffAddr#, writeWordOffAddr#,
           indexWord8ArrayAsWord#, readWord8ArrayAsWord#, writeWord8ArrayAsWord#)
derivePrim(Double, D#, sIZEOF_DOUBLE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#,
           indexDoubleOffAddr#, readDoubleOffAddr#, writeDoubleOffAddr#,
           indexWord8ArrayAsDouble#, readWord8ArrayAsDouble#, writeWord8ArrayAsDouble#)
derivePrim(Char, C#, sIZEOF_CHAR,
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#,
           indexWideCharOffAddr#, readWideCharOffAddr#, writeWideCharOffAddr#,
           indexWord8ArrayAsWideChar#, readWord8ArrayAsWideChar#, writeWord8ArrayAsWideChar#)
derivePrim(Word8, W8#, sIZEOF_WORD8,
           indexWord8Array#, readWord8Array#, writeWord8Array#,
           indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#,
           indexWord8Array#, readWord8Array#, writeWord8Array#)
derivePrim(Word16, W16#, sIZEOF_WORD16,
           indexWord16Array#, readWord16Array#, writeWord16Array#,
           indexWord16OffAddr#, readWord16OffAddr#, writeWord16OffAddr#,
           indexWord8ArrayAsWord16#, readWord8ArrayAsWord16#, writeWord8ArrayAsWord16#)
derivePrim(Word32, W32#, sIZEOF_WORD32,
           indexWord32Array#, readWord32Array#, writeWord32Array#,
           indexWord32OffAddr#, readWord32OffAddr#, writeWord32OffAddr#,
           indexWord8ArrayAsWord32#, readWord8ArrayAsWord32#, writeWord8ArrayAsWord32#)
derivePrim(Word64, W64#, sIZEOF_WORD64,
           indexWord64Array#, readWord64Array#, writeWord64Array#,
           indexWord64OffAddr#, readWord64OffAddr#, writeWord64OffAddr#,
           indexWord8ArrayAsWord64#, readWord8ArrayAsWord64#, writeWord8ArrayAsWord64#)
derivePrim(Int8, I8#, sIZEOF_INT8,
           indexInt8Array#, readInt8Array#, writeInt8Array#,
           indexInt8OffAddr#, readInt8OffAddr#, writeInt8OffAddr#,
           indexInt8Array#, readInt8Array#, writeInt8Array#)
derivePrim(Int16, I16#, sIZEOF_INT16,
           indexInt16Array#, readInt16Array#, writeInt16Array#,
           indexInt16OffAddr#, readInt16OffAddr#, writeInt16OffAddr#,
           indexWord8ArrayAsInt16#, readWord8ArrayAsInt16#, writeWord8ArrayAsInt16#)
derivePrim(Int32, I32#, sIZEOF_INT32,
           indexInt32Array#, readInt32Array#, writeInt32Array#,
           indexInt32OffAddr#, readInt32OffAddr#, writeInt32OffAddr#,
           indexWord8ArrayAsInt32#, readWord8ArrayAsInt32#, writeWord8ArrayAsInt32#)
derivePrim(Int64, I64#, sIZEOF_INT64,
           indexInt64Array#, readInt64Array#, writeInt64Array#,
           indexInt64OffAddr#, readInt64OffAddr#, writeInt64OffAddr#,
           indexWord8ArrayAsInt64#, readWord8ArrayAsInt64#, writeWord8ArrayAsInt64#)
derivePrim((Ptr a), Ptr, sIZEOF_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#,
           indexWord8ArrayAsAddr#, readWord8ArrayAsAddr#, writeWord8ArrayAsAddr#)
