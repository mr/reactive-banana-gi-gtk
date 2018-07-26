{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Reactive.Banana.GI.Gtk
    ( BuilderCastException(..)
    , castB
    , signalAddHandler
    , signalEN
    , signalE0
    , signalE1
    , signalE0R
    , signalE1R
    , attrE
    , attrB
    , sink
    , AttrOpBehavior(..)
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Data.Typeable
import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.TypeLits

import Data.GI.Base
import Data.GI.Base.Attributes
    ( AttrLabelProxy(..)
    , AttrInfo(..)
    , AttrLabel(..)
    , AttrGetC(..)
    , AttrOpAllowed(..)
    , AttrOpTag(..)
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

-- | Thown when 'castB' fails get an object
data BuilderCastException = UnknownIdException String
    deriving (Show, Typeable)

instance Exception BuilderCastException

-- | Shortcut for getting 'Data.GI.Base.GObject' from a Builder
--
-- @
-- stack <- castB builder "stack" Stack
-- @
castB
    :: (IsBuilder a, GObject o, MonadIO m)
    => a
    -> Text
    -> (ManagedPtr o -> o)
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
    -> ((a -> IO b) -> HaskellCallbackType info)
    -> b
    -> IO (AddHandler a)
signalAddHandler self signal f b = do
    (addHandler, fire) <- newAddHandler
    on self signal (f $ \x -> fire x >> return b)
    return addHandler

-- | Create an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy'. For making signalE# functions.
-- Provides the fire method from 'Reactive.Banana.Frameworks.newAddHandler'
-- for creating a callback.
signalEN
    ::
        ( SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> ((a -> IO b) -> HaskellCallbackType info)
    -> b
    -> MomentIO (Event a)
signalEN self signal f b = do
    addHandler <- liftIO $ signalAddHandler self signal f b
    fromAddHandler addHandler

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces nothing.
--
-- @
-- destroyE <- signalE0 window #destroy
-- @
signalE0
    ::
        ( HaskellCallbackType info ~ IO ()
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> MomentIO (Event ())
signalE0 self signal =  signalEN self signal ($ ()) ()

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces one argument.
signalE1
    ::
        ( HaskellCallbackType info ~ (a -> IO ())
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> MomentIO (Event a)
signalE1 self signal = signalEN self signal id ()

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces nothing.
-- The given value is returned in the GTK callback
--
-- @
-- keyPressedE <- signalE1R widget #keyPressedEvent False
-- @
signalE0R
    ::
        ( HaskellCallbackType info ~ IO b
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> b
    -> MomentIO (Event ())
signalE0R self signal b = signalEN self signal ($ ()) b

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces one argument.
-- The given value is returned in the GTK callback
signalE1R
    ::
        ( HaskellCallbackType info ~ (a -> IO b)
        , SignalInfo info
        , GObject self
        )
    => self
    -> SignalProxy self info
    -> b
    -> MomentIO (Event a)
signalE1R self signal b = signalEN self signal id b

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Attributes.AttrLabelProxy' that produces one argument.
attrE
    ::
        ( GObject self
        , AttrGetC info self attr result
        , KnownSymbol (AttrLabel info)
        )
    => self
    -> AttrLabelProxy (attr :: Symbol)
    -> MomentIO (Event result)
attrE self attr = do
    e <- signalE1 self (PropertyNotify attr)
    (const $ get self attr) `mapEventIO` e

-- | stepper on 'attrE'
attrB
    ::
        ( GObject self
        , AttrGetC info self attr result
        , KnownSymbol (AttrLabel info)
        )
    => self
    -> AttrLabelProxy (attr :: Symbol)
    -> MomentIO (Behavior result)
attrB self attr = do
    e <- attrE self attr
    initV <- get self attr
    stepper initV e

-- | Alternative to 'Data.GI.Base.Attributes.AttrOp' for use with 'sink'.
-- Accepts a 'Reactive.Banana.Behavior' and a GTK Attribute
data AttrOpBehavior self tag where
    (:==)
        ::
            ( HasAttributeList self
            , info ~ ResolveAttribute attr self
            , AttrInfo info
            , AttrBaseTypeConstraint info self
            , AttrOpAllowed tag info self
            , AttrSetTypeConstraint info b
            )
        => AttrLabelProxy (attr :: Symbol)
        -> Behavior b
        -> AttrOpBehavior self tag

    (:==>)
        ::
            ( HasAttributeList self
            , info ~ ResolveAttribute attr self
            , AttrInfo info
            , AttrBaseTypeConstraint info self
            , AttrOpAllowed tag info self
            , AttrSetTypeConstraint info b
            )
        => AttrLabelProxy (attr :: Symbol)
        -> Behavior (IO b)
        -> AttrOpBehavior self tag

    (:~~)
        ::
            ( HasAttributeList self
            , info ~ ResolveAttribute attr self
            , AttrInfo info
            , AttrBaseTypeConstraint info self
            , tag ~ AttrSet
            , AttrOpAllowed AttrSet info self
            , AttrOpAllowed AttrGet info self
            , AttrSetTypeConstraint info b
            , a ~ AttrGetType info
            )
        => AttrLabelProxy (attr :: Symbol)
        -> Behavior (a -> b)
        -> AttrOpBehavior self tag

    (:~~>)
        ::
            ( HasAttributeList self
            , info ~ ResolveAttribute attr self
            , AttrInfo info
            , AttrBaseTypeConstraint info self
            , tag ~ AttrSet
            , AttrOpAllowed AttrSet info self
            , AttrOpAllowed AttrGet info self
            , AttrSetTypeConstraint info b
            , a ~ AttrGetType info
            )
        => AttrLabelProxy (attr :: Symbol)
        -> Behavior (a -> IO b)
        -> AttrOpBehavior self tag

infixr 0 :==
infixr 0 :==>
infixr 0 :~~
infixr 0 :~~>

sink1 :: GObject self => self -> AttrOpBehavior self AttrSet -> MomentIO ()
sink1 self (attr :== b) = do
    x <- valueBLater b
    liftIOLater $ set self [attr := x]
    e <- changes b
    reactimate' $ (fmap $ \x -> set self [attr := x]) <$> e
sink1 self (attr :==> b) = do
    x <- valueBLater b
    liftIOLater $ set self [attr :=> x]
    e <- changes b
    reactimate' $ (fmap $ \x -> set self [attr :=> x]) <$> e
sink1 self (attr :~~ b) = do
    x <- valueBLater b
    liftIOLater $ set self [attr :~ x]
    e <- changes b
    reactimate' $ (fmap $ \x -> set self [attr :~ x]) <$> e
sink1 self (attr :~~> b) = do
    x <- valueBLater b
    liftIOLater $ set self [attr :~> x]
    e <- changes b
    reactimate' $ (fmap $ \x -> set self [attr :~> x]) <$> e

-- "Animate" an attribute with a 'Reactive.Banana.Behavior'.
--
-- @
-- clickedE <- signalE0 button #clicked
-- clickedCount <- accumB (0 :: Int) ((+ 1) <$ clickedE)
-- sink myLabel [#label :== (T.pack . show) <$> clickedCount]
-- @
sink :: GObject self => self -> [AttrOpBehavior self AttrSet] -> MomentIO ()
sink self attrBs = mapM_ (sink1 self) attrBs
