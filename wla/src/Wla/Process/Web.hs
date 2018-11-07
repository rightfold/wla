-- |
-- The web process handles HTTP requests.
module Wla.Process.Web
  ( webProcess
  ) where

import Data.Function ((&))
import Data.IORef (IORef)

import qualified Data.IORef as IORef
import qualified Network.Wai.Handler.Warp as Warp

import Wla.Config (Config (..))
import Wla.WishList (WishList)

import qualified Network.Wai.Cont as Wai.Cont
import qualified Wla.I18n as I18n
import qualified Wla.Web as Web

-- |
-- The process that serves HTTP requests.
webProcess :: Config -> IORef WishList -> IO ()
webProcess config wishListRef =
  -- FIXME: Listen on configHttpHost, not the Warp default.
  Warp.run (configHttpPort config & fromIntegral) $
    Wai.Cont.downgrade $
      Web.application I18n.nlNl (IORef.readIORef wishListRef)
