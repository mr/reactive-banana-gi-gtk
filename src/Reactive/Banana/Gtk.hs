{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Reactive.Banana.Gtk where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.ForeignPtr (ForeignPtr(..))

import Data.GI.Base
import Data.GI.Base.Signals
    ( SignalInfo(..)
    )
import GI.Gtk
    ( GObject
    , IsBuilder
    , builderGetObject
    , get
    )
import Data.GI.Base.ManagedPtr (unsafeCastTo)

castB
    :: (IsBuilder a, GObject o, MonadIO m)
    => a
    -> Text
    -> (ForeignPtr o -> o)
    -> m o
castB builder ident gtype =
    liftIO (builderGetObject builder ident
        >>= unsafeCastTo gtype . fromJust)

signalAddHandler
    ::
        ( SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> ((a -> IO ()) -> HaskellCallbackType info)
    -> IO (AddHandler a)
signalAddHandler object signal f = do
    (addHandler, fire) <- newAddHandler
    on object signal (f fire)
    return addHandler

signalEventN
    ::
        ( SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> ((a -> IO ()) -> HaskellCallbackType info)
    -> MomentIO (Event a)
signalEventN object signal f = do
    addHandler <- liftIO $ signalAddHandler object signal f
    fromAddHandler addHandler

signalEvent0
    ::
        ( HaskellCallbackType info ~ IO ()
        , SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> MomentIO (Event ())
signalEvent0 object signal =  signalEventN object signal ($ ())

signalEvent1
    ::
        ( HaskellCallbackType info ~ (a -> IO ())
        , SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> MomentIO (Event a)
signalEvent1 object signal = signalEventN object signal id

propEvent object attr = do
    e <- signalEventN object (PropertyNotify attr) id
    (const $ get object attr) `mapEventIO` e
