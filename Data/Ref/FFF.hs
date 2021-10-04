
{-# language
  UnboxedTuples, TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

module Data.Ref.FFF where

import GHC.Exts
import IO
import Data.Unlifted
import Data.Flat (Flat)
import qualified Data.Flat as F

type role Ref representational representational representational
data Ref a b c = Ref (MutableByteArray# RealWorld)

instance (Flat a, Flat b, Flat c) => Unlifted (Ref a b c) where
  type Rep (Ref a b c) = MutableByteArray# RealWorld
  to# (Ref r) = r
  {-# inline to# #-}
  from# r = Ref r
  {-# inline from# #-}
  defaultElem = defaultRef
  {-# inline defaultElem #-}

instance RunIO (Ref a b c) where
  runIO (IO f) = Ref (runRW# \s -> case f s of (# _, Ref a #) -> a )
  {-# inline runIO #-}

defaultRef :: forall a b c. (Flat a, Flat b, Flat c) => Ref a b c
defaultRef =
  Ref (runRW# (\s ->
    case newByteArray# (F.size# @a proxy# +# F.size# @b proxy# +# F.size# @c proxy#) s of
      (# s, arr #) -> arr))

new :: forall a b c. (Flat a, Flat b, Flat c) => a -> b -> c -> IO (Ref a b c)
new a b c = IO \s ->
  case newByteArray# (F.size# @a proxy# +# F.size# @b proxy# +# F.size# @c proxy#) s of
    (# s, arr #) -> case F.writeByteArray# @a arr 0# a s of
      s -> case F.writeWord8ArrayAs# @b arr (F.size# @a proxy#) b s of
        s -> case F.writeWord8ArrayAs# @c arr (F.size# @a proxy# +# F.size# @b proxy#) c s of
          s -> (# s, Ref arr #)
{-# inline new #-}

writeFst :: forall a b c. Flat a => Ref a b c -> a -> IO ()
writeFst (Ref r) a = IO (\s -> case F.writeByteArray# @a r 0# a s of
  s -> (# s , () #))
{-# inline writeFst #-}

readFst :: forall a b c. Flat a => Ref a b c -> IO a
readFst (Ref r) = IO (F.readByteArray# @a r 0#)
{-# inline readFst #-}

modifyFst :: forall a b c. Flat a => Ref a b c -> (a -> a) -> IO ()
modifyFst (Ref r) f = IO (\s -> case F.readByteArray# @a r 0# s of
  (# s, a #) -> case F.writeByteArray# @a r 0# (f a) s of
    s -> (# s, () #))
{-# inline modifyFst #-}

writeSnd :: forall a b c. (Flat a, Flat b) => Ref a b c -> b -> IO ()
writeSnd (Ref r) b = IO (\s -> case F.writeWord8ArrayAs# @b r (F.size# @a proxy#) b s of
  s -> (# s , () #))
{-# inline writeSnd #-}

readSnd :: forall a b c. (Flat a, Flat b) => Ref a b c -> IO b
readSnd (Ref r) = IO (F.readWord8ArrayAs# @b r (F.size# @a proxy#))
{-# inline readSnd #-}

modifySnd :: forall a b c. (Flat a, Flat b) => Ref a b c -> (b -> b) -> IO ()
modifySnd (Ref r) f = IO (\s -> case F.readWord8ArrayAs# @b r (F.size# @a proxy#) s of
  (# s, b #) -> case F.writeWord8ArrayAs# @b r (F.size# @a proxy#) (f b) s of
    s -> (# s, () #))
{-# inline modifySnd #-}

writeThd :: forall a b c. (Flat a, Flat b, Flat c) => Ref a b c -> c -> IO ()
writeThd (Ref r) c = IO (\s ->
  case F.writeWord8ArrayAs# @c r (F.size# @a proxy# +# F.size# @b proxy#) c s of
    s -> (# s , () #))
{-# inline writeThd #-}

readThd :: forall a b c. (Flat a, Flat b, Flat c) => Ref a b c -> IO c
readThd (Ref r) = IO (F.readWord8ArrayAs# @c r (F.size# @a proxy# +# F.size# @b proxy#))
{-# inline readThd #-}

modifyThd :: forall a b c. (Flat a, Flat b, Flat c) => Ref a b c -> (c -> c) -> IO ()
modifyThd (Ref r) f = IO (\s ->
  case F.readWord8ArrayAs# @c r (F.size# @a proxy# +# F.size# @b proxy#) s of
    (# s, c #) ->
      case F.writeWord8ArrayAs# @c r (F.size# @a proxy# +# F.size# @b proxy#) (f c) s of
        s -> (# s, () #))
{-# inline modifyThd #-}
