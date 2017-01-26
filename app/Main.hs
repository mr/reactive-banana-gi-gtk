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
    , GError(..)
    , get
    , on
    , gerrorMessage
    )

printProp :: Stack -> IO ()
printProp stack = print =<< fromJust <$> get stack #visibleChildName

networkDescription :: Stack -> MomentIO ()
networkDescription stack = do
    pEvent <- propEvent stack #visibleChildName
    reactimate $ (const $ printProp stack) <$> pEvent

runGtk = do
    Gtk.init Nothing
    builder <- builderNew
    builderAddFromFile builder "test.ui"

    window <- castB builder "window" Window
    headerBar <- castB builder "headerbar" HeaderBar
    stack <- castB builder "stack" Stack

    on window #destroy mainQuit
    #showAll window

    network <- compile (networkDescription stack)
    actuate network

    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
