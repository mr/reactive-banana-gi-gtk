{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Reactive.Banana
import Reactive.Banana.Gtk

import Control.Exception (catch)
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Gtk as Gtk
import GI.Gtk
    ( mainQuit
    , widgetShowAll
    , onWidgetDestroy
    , builderAddFromFile
    , builderNew
    , Window(..)
    , onWidgetDestroy
    , HeaderBar(..)
    , Stack(..)
    )

runGtk = do
    Gtk.init Nothing
    builder <- builderNew
    builderAddFromFile builder "test.ui"

    window <- castB builder "window" Window
    titlebar <- castB builder "headerbar" HeaderBar
    stack <- castB builder "stack" Stack

    on stack (PropertyNotify #visibleChildName) $ \p -> do
        name <- get stack #visibleChildName
        print name

    on window #destroy mainQuit

    #showAll window
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
