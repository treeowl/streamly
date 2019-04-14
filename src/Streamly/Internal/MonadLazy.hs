{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Streamly.Internal.MonadLazy
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.MonadLazy
    (
      MonadLazy (..)
    )
where

import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative (liftA2)
import GHC.IO (IO(..), unIO, failIO)
import GHC.Prim
import Data.Functor.Identity (Identity)

-------------------------------------------------------------------------------
-- Lazy bind for strict monads
-------------------------------------------------------------------------------

-- Strict Monads (read IO) can make the foldr run to completion due to the
-- strictness. We need a lazy bind to keep the composition lazy enough. When
-- implementing a left fold we need to make lazy values strict. That is easy to
-- do using explicit evaluation or strictness notations. In contrast, here in
-- foldr we need to make strict functions lazy (e.g. the IO bind) and there
-- seem to be no good way of doing that.
--
-- Note that this is only need to keep the Applicative composition of folds in
-- IO lazy and not to keep the IO itself lazy.
-- XXX Need to investigate if this can introduce any kind of badness/insanity?
--
class Monad m => MonadLazy m where
    lazyBind :: forall a b. m a -> (a -> m b) -> m b

instance MonadLazy IO where
    {-# INLINE lazyBind #-}
    lazyBind (IO m) k = IO ( \ s ->
            let r = case m s of (# _, res #) -> res
            in unIO (k r) s)

-- For lazy monads bind is lazy bind
instance MonadLazy Identity where
    {-# INLINE lazyBind #-}
    lazyBind = (>>=)