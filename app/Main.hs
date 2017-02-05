{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Reactive.Banana
import Reactive.Banana.GI.Gtk
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
    b <- builderNew
    builderAddFromFile b "test.ui"

    window <- castB b "window" Window
    destroyE <- signalE0 window #destroy
    reactimate $ mainQuit `doWhen` destroyE

    stack <- castB b "stack" Stack
    visibleB <- propB stack #visibleChildName

    button <- castB b "back_button" Button
    pressedE <- signalE0 button #clicked

    downloadedLabel <- castB b "download_count" Label
    searchedLabel <- castB b "search_count" Label

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
