{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Toss
  ( Throws,
    throw,
    try,
    catch,
    handle,

    -- * reexport
    (&),
  )
where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Function ((&))

class Monad m => Throws e m where
  throw :: e -> m a

instance Monad m => Throws e (ExceptT e m) where
  throw = ExceptT . pure . Left

instance {-# OVERLAPPABLE #-} (Throws e m, MonadTrans t, Monad (t m)) => Throws e (t m) where
  throw = lift . throw

try :: ExceptT e m a -> m (Either e a)
try = runExceptT

catch :: Monad m => ExceptT e m a -> (e -> m a) -> m a
catch action handler = runExceptT action >>= either handler pure

handle :: Monad m => (e -> m a) -> ExceptT e m a -> m a
handle = flip catch
