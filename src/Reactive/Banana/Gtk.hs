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

castB :: (IsBuilder a, GObject o) => a -> Text -> (ForeignPtr o -> o) -> IO o
castB builder ident gtype =
    builderGetObject builder ident
        >>= unsafeCastTo gtype . fromJust

signalAddHandler
    ::
        ( HaskellCallbackType info ~ (a -> IO ())
        , SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> IO (AddHandler a)
signalAddHandler object signal = do
    (addHandler, fire) <- newAddHandler
    on object signal fire
    return addHandler

signalEvent
    ::
        ( HaskellCallbackType info ~ (a -> IO ())
        , SignalInfo info
        , GObject object
        )
    => object
    -> SignalProxy object info
    -> MomentIO (Event a)
signalEvent object signal = do
    addHandler <- liftIO $ signalAddHandler object signal
    fromAddHandler addHandler

propEvent object = signalEvent object . PropertyNotify
