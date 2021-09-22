{-# language UnboxedTuples, TypeOperators, MagicHash #-}

module IO (IO(..), runIO, unIO) where

import GHC.Types
import GHC.Exts

runIO :: IO a -> a
runIO (IO f) = runRW# (\s -> case f s of (# _, a #) -> a)
{-# inline runIO #-}

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO f) = f
{-# inline unIO #-}
