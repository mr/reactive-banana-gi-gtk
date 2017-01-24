module Reactive.Banana.Gtk where

import Reactive.Banana
import qualified Data.Text as T
import Data.Text (Text)
import GI.Gtk
    ( GObject
    , IsBuilder
    )
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Control.Monad.IO.Class

--castB :: (MonadIO m, IsBuilder a, GObject o) => a -> Text -> m o
--castB builder ident gtype =
--    builderGetObject builder ident
--        >>= unsafeCastTo gtype . fromJust
