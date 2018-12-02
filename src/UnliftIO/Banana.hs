{-#
    LANGUAGE DeriveTraversable
#-}


module UnliftIO.Banana where

import qualified Control.Event.Handler as H
import Control.Monad
import UnliftIO hiding (toIO)

type EventHandler m a = a -> m ()
newtype AddHandler m a
    = AddHandler
    { register :: EventHandler m a -> m (m ()) }
    deriving (Functor)

toEventHandler :: MonadIO m => H.Handler a -> EventHandler m a
toEventHandler h = liftIO . h

fromEventHandler :: MonadUnliftIO m => EventHandler m a -> m (H.Handler a)
fromEventHandler eh = (\toIO -> toIO . eh) <$> askRunInIO

toAddHandler :: MonadUnliftIO m => H.AddHandler a -> AddHandler m a
toAddHandler (H.AddHandler ah) = AddHandler 
    $ fromEventHandler >=> (liftIO . fmap liftIO . ah)

fromAddHandler :: MonadUnliftIO m => AddHandler m a -> m (H.AddHandler a)
fromAddHandler (AddHandler ah) = do
    toIO  <- askRunInIO
    toIO' <- askRunInIO
    pure $ H.AddHandler $ fmap toIO . toIO' . ah . toEventHandler


