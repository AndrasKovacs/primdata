{-# language RankNTypes, KindSignatures, PolyKinds #-}

module Data.Internal.Errors where

import GHC.Stack
import GHC.Types

undefElem :: forall r (a :: TYPE r). HasCallStack => a
undefElem = error "undefined element"
{-# noinline undefElem #-}

unpinnedContents :: forall r (a :: TYPE r). HasCallStack => a
unpinnedContents = error "Data.Array.FM: can't take contents of unpinned array"
{-# noinline unpinnedContents #-}
