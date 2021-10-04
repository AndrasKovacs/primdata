{-# language UnboxedTuples, TypeOperators, MagicHash, BlockArguments #-}

module IO (IO(..), RunIO(..), unIO) where

import GHC.Types
import GHC.Exts
import GHC.Word
import GHC.Int

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO f) = f
{-# inline unIO #-}

class RunIO a where
  runIO :: IO a -> a
  runIO (IO f) = runRW# \s -> case f s of (# _ , a #) -> a
  {-# inline runIO #-}

instance RunIO Int where
  runIO (IO f) = I# (runRW# \s -> case f s of (# _, I# n #) -> n)
  {-# inline runIO #-}

instance RunIO Word where
  runIO (IO f) = W# (runRW# \s -> case f s of (# _, W# x #) -> x)
  {-# inline runIO #-}

instance RunIO Char where
  runIO (IO f) = C# (runRW# \s -> case f s of (# _, C# x #) -> x)
  {-# inline runIO #-}

instance RunIO Word8 where
  runIO (IO f) = W8# (runRW# \s -> case f s of (# _, W8# x #) -> x)
  {-# inline runIO #-}

instance RunIO Word16 where
  runIO (IO f) = W16# (runRW# \s -> case f s of (# _, W16# x #) -> x)
  {-# inline runIO #-}

instance RunIO Word32 where
  runIO (IO f) = W32# (runRW# \s -> case f s of (# _, W32# x #) -> x)
  {-# inline runIO #-}

instance RunIO Word64 where
  runIO (IO f) = W64# (runRW# \s -> case f s of (# _, W64# x #) -> x)
  {-# inline runIO #-}

instance RunIO Int8 where
  runIO (IO f) = I8# (runRW# \s -> case f s of (# _, I8# x #) -> x)
  {-# inline runIO #-}

instance RunIO Int16 where
  runIO (IO f) = I16# (runRW# \s -> case f s of (# _, I16# x #) -> x)
  {-# inline runIO #-}

instance RunIO Int32 where
  runIO (IO f) = I32# (runRW# \s -> case f s of (# _, I32# x #) -> x)
  {-# inline runIO #-}

instance RunIO Int64 where
  runIO (IO f) = I64# (runRW# \s -> case f s of (# _, I64# x #) -> x)
  {-# inline runIO #-}

instance RunIO Float where
  runIO (IO f) = F# (runRW# \s -> case f s of (# _, F# x #) -> x)
  {-# inline runIO #-}

instance RunIO Double where
  runIO (IO f) = D# (runRW# \s -> case f s of (# _, D# x #) -> x)
  {-# inline runIO #-}

instance RunIO Bool
instance RunIO [a]
instance RunIO (Maybe a)
instance RunIO (Either a b)
instance RunIO Ordering
