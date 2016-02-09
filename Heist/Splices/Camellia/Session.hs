{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Session where

----------------------------------------------------------------------

import Data.Maybe (isJust)
import qualified Data.Text as T
import Heist.Interpreted

-- for Session stuff
import Control.Monad.Trans.Class (MonadTrans, lift)
import Snap.Core
import Snap.Snaplet (SnapletLens, Handler, withTop)
import Snap.Snaplet.Session (SessionManager, getFromSession)
import Snap.Snaplet.Heist (SnapletISplice)

import Heist.Splices.Camellia.Visibility

----------------------------------------------------------------------

getFromSession' :: MonadTrans t => SnapletLens b SessionManager -> T.Text -> t (Handler b v) (Maybe T.Text)
getFromSession' sess = lift . withTop sess . getFromSession

sessionInfoSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionInfoSplice sess key = maybeSplice textSplice =<< getFromSession' sess key

sessionHasSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionHasSplice sess key = ifSplice' . isJust =<< getFromSession' sess key
