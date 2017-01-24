{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Gtk

import Data.Maybe (fromJust)
import Control.Exception (catch)
import qualified Data.Text as T
import Data.Text (Text)
import Data.GI.Base.GError
    ( gerrorMessage
    , GError(..)
    )
import qualified GI.Gtk as Gtk
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk
    ( mainQuit
    , widgetShowAll
    , onWidgetDestroy
    , builderAddFromFile
    , builderNew
    , builderGetObject
    , Window(..)
    , onWidgetDestroy
    , windowSetTitlebar
    , HeaderBar(..)
    )

castB builder ident gtype =
    builderGetObject builder ident
        >>= unsafeCastTo gtype . fromJust

runGtk = do
    Gtk.init Nothing
    builder <- builderNew
    builderAddFromFile builder "test.ui"

    window <- castB builder "window" Window
    titlebar <- castB builder "headerbar" HeaderBar
    windowSetTitlebar window (Just titlebar)

    onWidgetDestroy window mainQuit
    widgetShowAll window
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
