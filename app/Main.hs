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
    , Label(..)
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
    destroyE <- signalEvent0 window #destroy
    reactimate $ mainQuit `doWhen` destroyE

    stack <- castB builder "stack" Stack
    visibleB <- propBehavior stack #visibleChildName

    button <- castB builder "back_button" Button
    pressedE <- signalEvent0 button #clicked

    downloadedLabel <- castB builder "download_count" Label
    searchedLabel <- castB builder "search_count" Label

    let onPage b s = maybe False (== s) <$> b
        searched = whenE (visibleB `onPage` "search") pressedE
        downloaded = whenE (visibleB `onPage` "downloads") pressedE

    let countE e = accumB (0 :: Int) ((+ 1) <$ e)
    searchCount <- countE searched
    downloadCount <- countE downloaded

    sink searchedLabel #label $ (T.pack . show) <$> searchCount
    sink downloadedLabel #label $ (T.pack . show) <$> downloadCount

    #showAll window

runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
