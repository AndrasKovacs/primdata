
{-# language
  UnboxedTuples, TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

module Data.Ref.UUU where

import GHC.Exts
import Data.Unlifted
import qualified Data.Array.UM as UM

type role Ref representational representational representational
newtype Ref a b c = Ref (UM.Array a)

instance (Unlifted a, Unlifted b) => Unlifted (Ref a b c) where
  type Rep (Ref a b c) = MutableArrayArray# RealWorld
  to# (Ref (UM.Array r)) = r
  {-# inline to# #-}
  from# r = Ref (UM.Array r)
  {-# inline from# #-}
  defaultElem = Ref defaultElem
  {-# inline defaultElem #-}

new :: forall a b c. (Unlifted a, Unlifted b, Unlifted c) => a -> b -> c -> IO (Ref a b c)
new a b c = do
  arr <- UM.new @a 3 a
  UM.write @b (unsafeCoerce# arr) 1 b
  UM.write @c (unsafeCoerce# arr) 2 c
  pure (Ref (unsafeCoerce# arr))
{-# inline new #-}

readFst :: forall a b c. (Unlifted a) => Ref a b c -> IO a
readFst (Ref arr) = UM.read arr 0
{-# inline readFst #-}

readSnd :: forall a b c. (Unlifted b) => Ref a b c -> IO b
readSnd (Ref arr) = UM.read @b (unsafeCoerce# arr) 1
{-# inline readSnd #-}

readThd :: forall a b c. (Unlifted c) => Ref a b c -> IO c
readThd (Ref arr) = UM.read @c (unsafeCoerce# arr) 2
{-# inline readThd #-}

writeFst :: forall a b c. (Unlifted a) => Ref a b c -> a -> IO ()
writeFst (Ref arr) a = UM.write arr 0 a
{-# inline writeFst #-}

writeSnd :: forall a b c. (Unlifted b) => Ref a b c -> b -> IO ()
writeSnd (Ref arr) b = UM.write @b (unsafeCoerce# arr) 1 b
{-# inline writeSnd #-}

writeThd :: forall a b c. (Unlifted c) => Ref a b c -> c -> IO ()
writeThd (Ref arr) c = UM.write @c (unsafeCoerce# arr) 2 c
{-# inline writeThd #-}

modifyFst :: forall a b c. Unlifted a => Ref a b c -> (a -> a) -> IO ()
modifyFst (Ref arr) f = UM.modify arr 0 f
{-# inline modifyFst #-}

modifySnd :: forall a b c. Unlifted b => Ref a b c -> (b -> b) -> IO ()
modifySnd (Ref arr) f = UM.modify @b (unsafeCoerce# arr) 1 f
{-# inline modifySnd #-}

modifyThd :: forall a b c. Unlifted c => Ref a b c -> (c -> c) -> IO ()
modifyThd (Ref arr) f = UM.modify @c (unsafeCoerce# arr) 2 f
{-# inline modifyThd #-}
