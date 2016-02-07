{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.BlueBird.Session where

----------------------------------------------------------------------

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Heist.Interpreted

-- for Session stuff
import Control.Monad.Trans.Class (MonadTrans, lift)
import Snap.Core
import Snap.Snaplet (SnapletLens, Handler, withTop)
import Snap.Snaplet.Session (SessionManager, getFromSession)
import Snap.Snaplet.Heist (SnapletISplice)

import Heist.Splices.BlueBird.Visibility

----------------------------------------------------------------------

getFromSession' :: MonadTrans t => SnapletLens b SessionManager -> T.Text -> t (Handler b v) (Maybe T.Text)
getFromSession' sess = lift . withTop sess . getFromSession

sessionInfoSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionInfoSplice sess key = do
	val <- getFromSession' sess key
	textSplice $ fromMaybe "" val

sessionHasSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionHasSplice sess key = do
	val <- getFromSession' sess key
	ifSplice' (isJust val)
