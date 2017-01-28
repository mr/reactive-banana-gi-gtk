{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Gtk
import Reactive.Banana.Frameworks

import Control.Exception (catch)
import qualified Data.Text as T

import Data.Maybe (fromJust)
import qualified GI.Gtk as Gtk
import GI.Gtk
    ( mainQuit
    , builderAddFromFile
    , builderNew
    , Window(..)
    , HeaderBar(..)
    , Stack(..)
    , Button(..)
    , GError(..)
    , get
    , on
    , gerrorMessage
    , SignalProxy(..)
    )

doWhen f x = fmap (const f) x

networkDescription :: MomentIO ()
networkDescription = do
    builder <- builderNew
    builderAddFromFile builder "test.ui"

    window <- castB builder "window" Window
    stack <- castB builder "stack" Stack
    button <- castB builder "back_button" Button

    visibleE <- propEvent stack #visibleChildName
    destroyE <- signalEvent0 window #destroy
    pressedE <- signalEvent0 button #clicked

    reactimate $ print <$> visibleE
    reactimate $ mainQuit `doWhen` destroyE
    reactimate $ print "pressed" `doWhen` pressedE

    #showAll window

runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
