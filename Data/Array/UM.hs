{-# language
  UnboxedTuples, TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

module Data.Array.UM where

import GHC.Exts

import IO
import qualified Data.Array.UI as UI
import Data.Unlifted

type role Array representational
data Array a = Array (MutableArrayArray# RealWorld)

instance Unlifted (Array a) where
  type Rep (Array a) = (MutableArrayArray# RealWorld)
  to# (Array arr) = arr
  from#           = Array
  {-# inline to# #-}
  {-# inline from# #-}
  defaultElem = empty
  {-# inline defaultElem #-}

new :: forall a. Unlifted a => Int -> a -> IO (Array a)
new (I# i) a = IO (\s -> case newUnlifted# i (to# a) s of
    (# s, arr #) -> (# s, Array arr #))
{-# inline new #-}

empty :: Array a
empty = Array (runRW# \s -> case newArrayArray# 0# s of
  (# s, arr #) -> arr)
{-# noinline empty #-}

read :: forall a. Unlifted a => Array a -> Int -> IO a
read (Array arr) (I# i) = IO \s -> case readUnlifted# arr i s of
  (# s, a #) -> case from# a of
    !a -> (# s, a #)
{-# inline read #-}

write :: forall a. Unlifted a => Array a -> Int -> a -> IO ()
write (Array arr) (I# i) a = IO \s ->
  case writeUnlifted# arr i (to# a) s of
    s -> (# s, () #)
{-# inline write #-}

modify :: forall a. Unlifted a => Array a -> Int -> (a -> a) -> IO ()
modify (Array arr) (I# i) f = IO \s -> case readUnlifted# arr i s of
  (# s, a #) -> case from# a of
    !a -> case f a of
      !a -> case writeUnlifted# arr i (to# a) s of
        s -> (# s, () #)
{-# inline modify #-}

map' :: forall a. Unlifted a => (a -> a) -> Array a -> IO ()
map' f (Array arr) = IO \s ->
  let go :: forall s. MutableArrayArray# s -> Int# -> Int# -> State# s -> (# State# s, () #)
      go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readUnlifted# arr i s of
          (# s, a #) -> case from# a of
            !a -> case f a of
              !a -> case writeUnlifted# arr i (to# a) s of
                 s -> go arr (i +# 1#) n s
  in go arr 0# (sizeofMutableArrayArray# arr) s
{-# inline map' #-}

for :: forall a. Unlifted a => Array a -> (a -> IO ()) -> IO ()
for (Array arr) f = IO \s ->
  let go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readUnlifted# arr i s of
          (# s, a #) -> case f (from# a) of
            IO f -> case f s of
              (# s, _ #) -> go arr (i +# 1#) n s
  in go arr 0# (sizeofMutableArrayArray# arr) s
{-# inline for #-}

set :: forall a. Unlifted a => Array a -> a -> IO ()
set arr a = map' (\_ -> a) arr
{-# inline set #-}

size :: Array a -> Int
size (Array arr) = I# (sizeofMutableArrayArray# arr)
{-# inline size #-}

thawSlice :: UI.Array a -> Int -> Int -> IO (Array a)
thawSlice (UI.Array arr) (I# start) (I# len) = IO \s ->
  case newArrayArray# len s of
    (# s , marr #) -> case copyArrayArray# arr start marr 0# len s of
      s -> (# s, Array marr #)
{-# inline thawSlice #-}

thaw :: forall a. UI.Array a -> IO (Array a)
thaw arr = thawSlice arr 0 (UI.size arr)
{-# inline thaw #-}

copySlice :: forall a. Array a -> Int -> Array a -> Int -> Int -> IO ()
copySlice (Array src) (I# i) (Array dst) (I# j) (I# len) = IO \s ->
  case copyMutableArrayArray# src i dst j len s of
    s -> (# s, () #)
{-# inline copySlice #-}

sizedThaw :: forall a. Int -> UI.Array a -> IO (Array a)
sizedThaw size arr = thawSlice arr 0 size
{-# inline sizedThaw #-}

unsafeFreeze :: Array a -> IO (UI.Array a)
unsafeFreeze (Array marr) = IO \s -> case unsafeFreezeArrayArray# marr s of
  (# s, arr #) -> (# s, UI.Array arr #)
{-# inline unsafeFreeze #-}

freezeSlice :: Array a -> Int -> Int -> IO (UI.Array a)
freezeSlice (Array src) (I# start) (I# len) = IO \s ->
  case newArrayArray# len s of
    (# s, marr #) -> case copyMutableArrayArray# src start marr 0# len s of
      s -> case unsafeFreezeArrayArray# marr s of
        (# s, arr #) -> (# s , UI.Array arr #)
{-# inline freezeSlice #-}

freeze :: Array a -> IO (UI.Array a)
freeze arr = freezeSlice arr 0 (size arr)
{-# inline freeze #-}
