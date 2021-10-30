{-# language
  TypeOperators, MagicHash, RankNTypes,
  TypeApplications, ScopedTypeVariables, BangPatterns, BlockArguments,
  RoleAnnotations, TypeFamilies, AllowAmbiguousTypes #-}

module Data.Ref.U where

import GHC.Exts
import Data.Unlifted

import qualified Data.Array.UM as UM

type role Ref representational
newtype Ref a = Ref (UM.Array a)

instance (Unlifted a) => Unlifted (Ref a) where
  type Rep (Ref a) = MutableArrayArray# RealWorld
  to# (Ref (UM.Array r)) = r
  {-# inline to# #-}
  from# r = Ref (UM.Array r)
  {-# inline from# #-}
  defaultElem = Ref defaultElem
  {-# inline defaultElem #-}

new :: forall a. (Unlifted a) => a -> IO (Ref a)
new a = Ref <$> UM.new @a 1 a
{-# inline new #-}

read :: forall a. (Unlifted a) => Ref a -> IO a
read (Ref arr) = UM.read arr 0
{-# inline read #-}

write :: forall a. (Unlifted a) => Ref a -> a -> IO ()
write (Ref arr) a = UM.write arr 0 a
{-# inline write #-}

modify :: forall a. Unlifted a => Ref a -> (a -> a) -> IO ()
modify (Ref arr) f = UM.modify arr 0 f
{-# inline modify #-}
