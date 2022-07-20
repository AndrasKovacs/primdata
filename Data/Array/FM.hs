{-# language
  UnboxedTuples, TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

{-|
Flat mutable arrays.
-}

module Data.Array.FM where

import GHC.Exts
import Data.Kind

import Data.Flat
import IO

import qualified Data.Array.FI as FI
import Data.Unlifted

type role Array representational
data Array (a :: Type) = Array (MutableByteArray# RealWorld)

elemType :: Array a -> Proxy# a
elemType _ = proxy#
{-# inline elemType #-}

instance Unlifted (Array a) where
  type Rep (Array a) = MutableByteArray# RealWorld
  to# (Array arr) = arr
  from#           = Array
  {-# inline to# #-}
  {-# inline from# #-}
  defaultElem = empty
  {-# inline defaultElem #-}

new :: forall a. Flat a => Int -> IO (Array a)
new (I# n) = IO \s -> case newByteArray# (toByteOffset# @a proxy# n) s of
    (# s, marr #) -> (# s, Array marr #)
{-# inline new #-}

empty :: Array a
empty = Array (runRW# \s -> case newByteArray# 0# s of
  (# s, arr #) -> arr)
{-# noinline empty #-}

read :: forall a. Flat a => Array a -> Int -> IO a
read (Array arr) (I# i) = IO (readByteArray# arr i)
{-# inline read #-}

write :: forall a. Flat a => Array a -> Int -> a -> IO ()
write (Array arr) (I# i) a = IO \s ->
  case writeByteArray# arr i a s of
    s -> (# s, () #)
{-# inline write #-}

modify :: forall a.  Flat a => Array a -> Int -> (a -> a) -> IO ()
modify (Array arr) (I# i) f = IO \s -> case readByteArray# arr i s of
  (# s, a #) -> let !v = f a in case writeByteArray# arr i v s of
    s -> (# s, () #)
{-# inline modify #-}

map' :: forall a. Flat a => (a -> a) -> Array a -> IO ()
map' f (Array arr) = IO \s ->
  let go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readWord8ArrayAs# arr i s of
          (# s, a #) -> case a of
            !a -> case f a of
              !a -> case writeWord8ArrayAs# arr i a s of
                 s -> go arr (i +# size# @a proxy#) n s
  in go arr 0# (sizeofMutableByteArray# arr) s
{-# inline map' #-}

for :: forall a. Flat a => Array a -> (a -> IO ()) -> IO ()
for (Array arr) f = IO \s ->
  let go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readWord8ArrayAs# arr i s of
          (# s, a #) -> case a of
            !a -> case f a of
              IO f -> case f s of
                (# s, _ #) -> go arr (i +# size# @a proxy#) n s
  in go arr 0# (sizeofMutableByteArray# arr) s
{-# inline for #-}

set :: forall a. Flat a => Array a -> a -> IO ()
set (Array arr) a = IO \s ->
  let go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case writeWord8ArrayAs# arr i a s of
          s -> go arr (i +# size# @a proxy#) n s
  in go arr 0# (sizeofMutableByteArray# arr) s
{-# inline set #-}

size :: forall a. Flat a => Array a -> Int
size (Array arr) = I# (fromByteOffset# @a proxy# (sizeofMutableByteArray# arr))
{-# inline size #-}

thaw :: forall a. FI.Array a -> IO (Array a)
thaw (FI.Array arr) =
  let n = sizeofByteArray# arr
  in IO \s -> case newByteArray# n s of
       (# s, marr #) -> case copyByteArray# arr 0# marr 0# n s of
         s -> (# s, Array marr #)
{-# inline thaw #-}

unsafeFreeze :: Array a -> IO (FI.Array a)
unsafeFreeze (Array marr) = IO \s -> case unsafeFreezeByteArray# marr s of
  (# s, arr #) -> (# s, FI.Array arr #)
{-# inline unsafeFreeze #-}
