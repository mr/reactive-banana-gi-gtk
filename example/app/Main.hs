{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Reactive.Banana
import Reactive.Banana.GI.Gtk
import Reactive.Banana.Frameworks

import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (fromJust)
import qualified GI.Gtk as Gtk
import GI.Gtk
    ( mainQuit
    , builderAddFromFile
    , builderNew
    , get
    , Window(..)
    , Stack(..)
    , Button(..)
    , Label(..)
    , GError(..)
    , gerrorMessage
    )
import GI.Gdk.Structs.EventKey (EventKey(..))

data StackPage = Search | Downloads

instance Show StackPage where
    show Search = "search"
    show Downloads = "downloads"

showT :: Show a => a -> Text
showT = T.pack . show

networkDescription :: MomentIO ()
networkDescription = do
    b <- builderNew
    builderAddFromFile b "test.ui"

    window <- castB b "window" Window
    destroyE <- signalE0 window #destroy
    reactimate $ mainQuit <$ destroyE

    stack <- castB b "stack" Stack
    visibleB <- attrB stack #visibleChildName

    button <- castB b "back_button" Button
    pressedE <- signalE0 button #clicked

    keyPressE <- signalE1R window #keyPressEvent False
    reactimate $ (\e -> get e #string >>= print) <$> keyPressE

    downloadedLabel <- castB b "download_count" Label
    searchedLabel <- castB b "search_count" Label

    let onPage b s = maybe False (== showT s) <$> b
        searched = whenE (visibleB `onPage` Search) pressedE
        downloaded = whenE (visibleB `onPage` Downloads) pressedE
        countE e = accumB (0 :: Int) ((+ 1) <$ e)

    searchCount <- countE searched
    downloadCount <- countE downloaded

    sink searchedLabel [#label :== showT <$> searchCount]
    sink downloadedLabel [#label :== showT <$> downloadCount]

    #showAll window

runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
