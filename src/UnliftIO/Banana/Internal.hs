{-# LANGUAGE DeriveFunctor #-}

module UnliftIO.Banana.Internal where

import qualified Control.Event.Handler as H
import Control.Monad
import Control.Monad.IO.Unlift

type EventHandler m a = a -> m ()
newtype AddHandler m a
    = AddHandler
    { register :: EventHandler m a -> m (m ()) }
    deriving (Functor)

unliftAH :: Monad m => m (AddHandler m a) -> AddHandler m a
unliftAH mah = AddHandler $ \eh -> do
    ah <- mah
    register ah eh

unliftEH :: Monad m => m (a -> m b) -> a -> m b
unliftEH mf a = do
    f <- mf
    f a

toEH :: MonadIO m => H.Handler a -> EventHandler m a
toEH h = liftIO . h

fromEH :: MonadUnliftIO m => EventHandler m a -> m (H.Handler a)
fromEH eh = (\conv -> conv . eh) <$> askRunInIO

toAH :: MonadUnliftIO m => H.AddHandler a -> AddHandler m a
toAH (H.AddHandler ah) = AddHandler 
    $ fromEH >=> (liftIO . fmap liftIO . ah)

fromAH :: MonadUnliftIO m => AddHandler m a -> m (H.AddHandler a)
fromAH (AddHandler ah) = do
    conv  <- askRunInIO
    conv' <- askRunInIO
    pure $ H.AddHandler $ fmap conv . conv' . ah . toEH


