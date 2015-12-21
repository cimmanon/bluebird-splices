{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Session where

----------------------------------------------------------------------

import qualified Data.Text as T
import Heist.Interpreted
import Heist.SpliceAPI

-- for Session stuff
import Control.Monad.Trans.Class (lift)
import Snap.Snaplet (SnapletLens, withTop)
import Snap.Snaplet.Session (SessionManager, getFromSession)
import Snap.Snaplet.Heist (SnapletISplice)

import Data.Maybe (fromMaybe, isJust)

import Heist.Splices.Common

----------------------------------------------------------------------

sessionInfoSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
sessionInfoSplice sess key = do
	val <- lift $ withTop sess $ getFromSession key
	textSplice $ fromMaybe "" val

loggedInSplice :: SnapletLens b SessionManager -> T.Text -> SnapletISplice b
loggedInSplice sess key = do
	val <- lift $ withTop sess $ getFromSession key
	ifSplice' (isJust val)
