{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Reactive.Banana.Gtk where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Typeable
import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.ForeignPtr (ForeignPtr(..))

import Data.GI.Base
import Data.GI.Base.Attributes
    ( AttrLabelProxy(..)
    , AttrInfo(..)
    , AttrLabel(..)
    , AttrGetC(..)
    )

import Data.GI.Base.Overloading
    ( ResolveAttribute(..)
    , HasAttributeList(..)
    )

import Data.GI.Base.Signals
    ( SignalInfo(..)
    , GObjectNotifySignalInfo(..)
    )
import GI.Gtk
    ( GObject
    , IsBuilder
    , builderGetObject
    , get
    )
import Data.GI.Base.ManagedPtr (unsafeCastTo)

data BuilderCastException = UnknownIdException String
    deriving (Show, Typeable)

instance Exception BuilderCastException

castB
    :: (IsBuilder a, GObject o, MonadIO m)
    => a
    -> Text
    -> (ForeignPtr o -> o)
    -> m o
castB builder ident gtype =
    liftIO $ do
        o <- builderGetObject builder ident
        case o of
            Just a -> unsafeCastTo gtype a
            Nothing ->
                throw $ UnknownIdException $ T.unpack ident

signalAddHandler
    ::
        ( SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> ((a -> IO ()) -> HaskellCallbackType info)
    -> IO (AddHandler a)
signalAddHandler self signal f = do
    (addHandler, fire) <- newAddHandler
    on self signal (f fire)
    return addHandler

signalEventN
    ::
        ( SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> ((a -> IO ()) -> HaskellCallbackType info)
    -> MomentIO (Event a)
signalEventN self signal f = do
    addHandler <- liftIO $ signalAddHandler self signal f
    fromAddHandler addHandler

signalEvent0
    ::
        ( HaskellCallbackType info ~ IO ()
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> MomentIO (Event ())
signalEvent0 self signal =  signalEventN self signal ($ ())

signalEvent1
    ::
        ( HaskellCallbackType info ~ (a -> IO ())
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> MomentIO (Event a)
signalEvent1 self signal = signalEventN self signal id

propEvent self attr = do
    e <- signalEventN self (PropertyNotify attr) id
    (const $ get self attr) `mapEventIO` e

propBehavior self attr = do
    e <- propEvent self attr
    initV <- get self attr
    stepper initV e

sink self attr b = do
    x <- valueBLater b
    liftIOLater $ set self [attr := x]
    e <- changes b
    reactimate' $ (fmap $ \x -> set self [attr := x]) <$> e
