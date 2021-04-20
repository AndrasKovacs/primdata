
module Data.Array.SM where

import GHC.Types
import GHC.Prim
import GHC.Magic

import Data.Unlifted
import Data.Array.UndefElem
import qualified Data.Array.SI as SI

type role Array representational
data Array a = Array (SmallMutableArray# RealWorld a)

instance Unlifted (Array a) where
  type Rep (Array a) = SmallMutableArray# RealWorld a
  to# (Array arr) = arr
  from#           = Array
  {-# inline to# #-}
  {-# inline from# #-}
  defaultElem = empty
  {-# inline defaultElem #-}

new :: forall a.  Int -> a -> IO (Array a)
new (I# i) a = IO (\s -> case newSmallArray# i a s of
    (# s, arr #) -> (# s, Array arr #))

empty :: Array a
empty = runRW# \s -> case newSmallArray# 0# undefElem s of
  (# s, arr #) -> Array arr
{-# noinline empty #-}

read :: forall a.  Array a -> Int -> IO a
read (Array arr) (I# i) = IO (readSmallArray# arr i)
{-# inline read #-}

write :: forall a.  Array a -> Int -> a -> IO ()
write (Array arr) (I# i) a = IO \s ->
  case writeSmallArray# arr i a s of
    s -> (# s, () #)
{-# inline write #-}

modify :: forall a.  Array a -> Int -> (a -> a) -> IO ()
modify (Array arr) (I# i) f = IO \s -> case readSmallArray# arr i s of
  (# s, a #) -> case writeSmallArray# arr i (f a) s of
    s -> (# s, () #)
{-# inline modify #-}

modify' :: forall a.  Array a -> Int -> (a -> a) -> IO ()
modify' (Array arr) (I# i) f = IO \s -> case readSmallArray# arr i s of
  (# s, a #) -> let !v = f a in case writeSmallArray# arr i v s of
    s -> (# s, () #)
{-# inline modify' #-}

map' :: forall a b. (a -> b) -> Array a -> IO ()
map' f (Array arr) = IO \s ->
  let go :: forall s. SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, () #)
      go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readSmallArray# arr i s of
          (# s, a #) -> let !v = unsafeCoerce# (f a) in case writeSmallArray# arr i v s of
            s -> go arr (i +# 1#) n s
  in go arr 0# (sizeofSmallMutableArray# arr) s
{-# inline map' #-}

for :: forall a. Array a -> (a -> IO ()) -> IO ()
for (Array arr) f = IO \s ->
  let go arr i n s = case i ==# n of
        1# -> (# s, () #)
        _  -> case readSmallArray# arr i s of
          (# s, a #) -> case f a of
            IO f -> case f s of
              (# s, _ #) -> go arr (i +# 1#) n s
  in go arr 0# (sizeofSmallMutableArray# arr) s
{-# inline for #-}

set :: forall a. Array a -> a -> IO ()
set arr a = map' (\_ -> a) arr
{-# inline set #-}

size :: Array a -> Int
size (Array arr) = I# (sizeofSmallMutableArray# arr)
{-# inline size #-}

thawSlice :: SI.Array a -> Int -> Int -> IO (Array a)
thawSlice (SI.Array arr) (I# start) (I# len) = IO \s ->
  case thawSmallArray# arr start len s of
    (# s, marr #) -> (# s, Array marr #)
{-# inline thawSlice #-}

thaw :: forall a. SI.Array a -> IO (Array a)
thaw arr = thawSlice arr 0 (SI.size arr)
{-# inline thaw #-}

copySlice :: forall a. Array a -> Int -> Array a -> Int -> Int -> IO ()
copySlice (Array src) (I# i) (Array dst) (I# j) (I# len) = IO \s ->
  case copySmallMutableArray# src i dst j len s of
    s -> (# s, () #)
{-# inline copySlice #-}

sizedThaw :: forall a. Int -> SI.Array a -> IO (Array a)
sizedThaw size arr = thawSlice arr 0 size
{-# inline sizedThaw #-}

unsafeFreeze :: Array a -> IO (SI.Array a)
unsafeFreeze (Array marr) = IO \s -> case unsafeFreezeSmallArray# marr s of
  (# s, arr #) -> (# s, SI.Array arr #)
{-# inline unsafeFreeze #-}

freezeSlice :: Array a -> Int -> Int -> IO (SI.Array a)
freezeSlice (Array marr) (I# start) (I# len) = IO \s ->
  case freezeSmallArray# marr start len s of
    (# s, arr #) -> (# s, (SI.Array arr) #)
{-# inline freezeSlice #-}

freeze :: Array a -> IO (SI.Array a)
freeze arr = freezeSlice arr 0 (size arr)
{-# inline freeze #-}

sizedFreeze :: Int -> Array a -> IO (SI.Array a)
sizedFreeze size arr = freezeSlice arr 0 size
{-# inline sizedFreeze #-}
