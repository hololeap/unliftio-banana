{-#
    LANGUAGE DeriveTraversable
#-}


module UnliftIO.Banana.Handler 
    ( EventHandler
    , AddHandler(..)
    , newAddHandler
    , mapIO
    , filterIO ) where

import qualified Control.Event.Handler as H
import Control.Monad
import Control.Monad.IO.Unlift

import UnliftIO.Banana.Internal

newAddHandler :: MonadUnliftIO m => m (AddHandler m a, EventHandler m a)
newAddHandler = liftIO $ do
    (ah, eh) <- H.newAddHandler
    pure (toAH ah, toEH eh)

mapIO :: MonadUnliftIO m => (a -> m b) -> AddHandler m a -> AddHandler m b
mapIO f ah = unliftAH $ do
    conv <- askRunInIO
    ah' <- fromAH ah
    pure $ toAH $ H.mapIO (conv . f) ah'

filterIO :: MonadUnliftIO m 
    => (a -> m Bool) -> AddHandler m a -> AddHandler m a
filterIO f ah = unliftAH $ do
    conv <- askRunInIO
    ah' <- fromAH ah
    pure $ toAH $ H.filterIO (conv . f) ah'

