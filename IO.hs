{-# language UnboxedTuples, TypeOperators, MagicHash, BlockArguments #-}

module IO (IO(..), unIO, runIO) where

import GHC.Types
import GHC.Exts

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO f) = f
{-# inline unIO #-}

runIO :: IO a -> a
runIO (IO f) = runRW# \s -> case f s of (# _ , a #) -> a
{-# inline runIO #-}
