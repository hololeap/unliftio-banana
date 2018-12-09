
module UnliftIO.Banana.Frameworks 
    ( MomentIO
    , EventNetwork
    , Future
    , interpretAsHandler
    , compile
    , fromAddHandler
    , fromChanges
    , F.fromPoll
    , fromPollU
    , F.reactimate
    , reactimateU
    , F.reactimate'
    , reactimateU'
    , F.changes
    , F.imposeChanges
    , F.execute
    , F.liftIOLater
    , liftIOLaterU
    , interpretFrameworks
    , F.newEvent
    , F.mapEventIO
    , mapEventIOU
    , F.newBehavior
    , actuate
    , pause                ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks (MomentIO, EventNetwork, Future)
import qualified Reactive.Banana.Frameworks as F

import UnliftIO.Banana.Handler
import UnliftIO.Banana.Internal

interpretAsHandler :: MonadUnliftIO m 
    => (Event a -> Moment (Event b)) -> AddHandler m a -> AddHandler m b
interpretAsHandler f ah = unliftAH $ do
    ah' <- fromAH ah
    pure $ toAH $ F.interpretAsHandler f ah'

compile :: MonadIO m => MomentIO () -> m EventNetwork
compile = liftIO . F.compile

fromAddHandler :: AddHandler MomentIO a -> MomentIO (Event a)
fromAddHandler = fromAH >=> F.fromAddHandler

fromChanges :: a -> AddHandler MomentIO a -> MomentIO (Behavior a)
fromChanges a = fromAH >=> F.fromChanges a

fromPollU :: MonadUnliftIO m => m a -> m (MomentIO (Behavior a))
fromPollU ma = (\c -> F.fromPoll $ c ma) <$> askRunInIO

reactimateU :: MonadUnliftIO m => Event (m ()) -> m (MomentIO ())
reactimateU e = (\c -> F.reactimate $ c <$> e) <$> askRunInIO

reactimateU' :: MonadUnliftIO m
    => Event (Future (m ())) -> m (MomentIO ())
reactimateU' e = (\c -> F.reactimate' $ fmap c <$> e) <$> askRunInIO

liftIOLaterU :: MonadUnliftIO m => m () -> m (MomentIO ())
liftIOLaterU a = (\c -> F.liftIOLater $ c a) <$> askRunInIO

interpretFrameworks :: MonadIO m 
    => (Event a -> MomentIO (Event b)) -> [Maybe a] -> m [Maybe b]
interpretFrameworks f = liftIO . F.interpretFrameworks f

mapEventIOU :: MonadUnliftIO m
    => (a -> m b) -> Event a -> m (MomentIO (Event b))
mapEventIOU f e = (\c -> F.mapEventIO (c . f) e) <$> askRunInIO

actuate :: MonadIO m => EventNetwork -> m ()
actuate = liftIO . F.actuate

pause :: MonadIO m => EventNetwork -> m ()
pause = liftIO . F.pause
