{-# language
  UnboxedTuples, TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

module Data.Ref.FF where

import GHC.Exts
import IO
import Data.Unlifted
import Data.Flat (Flat)
import qualified Data.Flat as F

type role Ref representational representational
data Ref a b = Ref (MutableByteArray# RealWorld)

instance (Flat a, Flat b) => Unlifted (Ref a b) where
  type Rep (Ref a b) = MutableByteArray# RealWorld
  to# (Ref r) = r
  {-# inline to# #-}
  from# r = Ref r
  {-# inline from# #-}
  defaultElem = defaultRef
  {-# inline defaultElem #-}

instance RunIO (Ref a b) where
  runIO (IO f) = Ref (runRW# \s -> case f s of (# _, Ref a #) -> a )
  {-# inline runIO #-}

defaultRef :: forall a b. (Flat a, Flat b) => Ref a b
defaultRef =
  Ref (runRW# (\s -> case newByteArray# (F.size# @a proxy# +# F.size# @b proxy#) s of
    (# s, arr #) -> arr))

new :: forall a b. (Flat a, Flat b) => a -> b -> IO (Ref a b)
new a b = IO \s -> case newByteArray# (F.size# @a proxy# +# F.size# @b proxy#) s of
  (# s, arr #) -> case F.writeByteArray# @a arr 0# a s of
    s -> case F.writeWord8ArrayAs# @b arr (F.size# @a proxy#) b s of
      s -> (# s, Ref arr #)
{-# inline new #-}

writeFst :: forall a b. Flat a => Ref a b -> a -> IO ()
writeFst (Ref r) a = IO (\s -> case F.writeByteArray# @a r 0# a s of
  s -> (# s , () #))
{-# inline writeFst #-}

readFst :: forall a b. Flat a => Ref a b -> IO a
readFst (Ref r) = IO (F.readByteArray# @a r 0#)
{-# inline readFst #-}

modifyFst :: forall a b. Flat a => Ref a b -> (a -> a) -> IO ()
modifyFst (Ref r) f = IO (\s -> case F.readByteArray# @a r 0# s of
  (# s, a #) -> case F.writeByteArray# @a r 0# (f a) s of
    s -> (# s, () #))
{-# inline modifyFst #-}

writeSnd :: forall a b. (Flat a, Flat b) => Ref a b -> b -> IO ()
writeSnd (Ref r) b = IO (\s -> case F.writeWord8ArrayAs# @b r (F.size# @a proxy#) b s of
  s -> (# s , () #))
{-# inline writeSnd #-}

readSnd :: forall a b. (Flat a, Flat b) => Ref a b -> IO b
readSnd (Ref r) = IO (F.readWord8ArrayAs# @b r (F.size# @a proxy#))
{-# inline readSnd #-}

modifySnd :: forall a b. (Flat a, Flat b) => Ref a b -> (b -> b) -> IO ()
modifySnd (Ref r) f = IO (\s -> case F.readWord8ArrayAs# @b r (F.size# @a proxy#) s of
  (# s, b #) -> case F.writeWord8ArrayAs# @b r (F.size# @a proxy#) (f b) s of
    s -> (# s, () #))
{-# inline modifySnd #-}
