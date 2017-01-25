module Reactive.Banana.Gtk where

import Reactive.Banana
import qualified Data.Text as T
import Data.Text (Text)
import GI.Gtk
    ( GObject
    , IsBuilder
    , builderGetObject
    )
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GHC.ForeignPtr (ForeignPtr(..))
import Control.Monad.IO.Class
import Data.Maybe (fromJust)

castB :: (IsBuilder a, GObject o) => a -> Text -> (ForeignPtr o -> o) -> IO o
castB builder ident gtype =
    builderGetObject builder ident
        >>= unsafeCastTo gtype . fromJust
