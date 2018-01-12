{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Session where

----------------------------------------------------------------------

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import qualified Data.Text as T
import Heist.Interpreted
import Safe (readMay)

-- for Session stuff
import Control.Monad.Trans.Class (MonadTrans, lift)
import Snap.Snaplet (SnapletLens, Handler, withTop)
import Snap.Snaplet.Session (SessionManager, getFromSession)
import Snap.Snaplet.Heist (SnapletISplice)

import Heist.Splices.Camellia.Visibility (maybeSplice, ifSplice)

{----------------------------------------------------------------------------------------------------{
                                                                      | Helpers
}----------------------------------------------------------------------------------------------------}

readFromSession :: Read a => T.Text -> Handler b SessionManager (Maybe a)
readFromSession = fmap (readMay <=< fmap T.unpack) . getFromSession

inSession :: T.Text -> Handler b SessionManager Bool
inSession = fmap isJust . getFromSession

liftS :: MonadTrans t => SnapletLens b SessionManager -> Handler b SessionManager a -> t (Handler b v) a
liftS sess h = lift $ withTop sess h

liftFromSession :: MonadTrans t => SnapletLens b SessionManager -> T.Text -> t (Handler b v) (Maybe T.Text)
liftFromSession sess = liftS sess . getFromSession

{----------------------------------------------------------------------------------------------------{
                                                                      | Splices
}----------------------------------------------------------------------------------------------------}

sessionTextSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionTextSplice sess key = maybeSplice textSplice =<< liftFromSession sess key

sessionIfSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionIfSplice sess key = ifSplice =<< liftS sess (inSession key)

{-# DEPRECATED getFromSession' "Deprecated in favor of liftFromSession" #-}
getFromSession' :: MonadTrans t => SnapletLens b SessionManager -> T.Text -> t (Handler b v) (Maybe T.Text)
getFromSession' = liftFromSession

{-# DEPRECATED sessionInfoSplice "Deprecated in favor of sessionTextSplice" #-}
sessionInfoSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionInfoSplice = sessionTextSplice

{-# DEPRECATED sessionHasSplice "Deprecated in favor of sessionIfSplice" #-}
sessionHasSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionHasSplice = sessionIfSplice
