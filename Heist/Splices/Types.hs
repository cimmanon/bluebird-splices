{-# LANGUAGE OverloadedStrings, TypeOperators, ScopedTypeVariables #-}
-- TODO: move some of this to a general purpose library

module Heist.Splices.Types where

------------------------------------------------------------------------------

--import Control.Monad.Trans (MonadIO)
--import Data.ByteString (ByteString)
--import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
--import Data.Monoid ((<>))
import Heist (getParamNode)
import Heist.Interpreted
import Heist.SpliceAPI
import qualified Text.XmlHtml as X hiding (render)
import Data.List.Split (chunksOf)
import Heist (getParamNode)

-- for dates
import Data.Time.Format
import System.Locale
import Data.Time.Calendar
import Data.Time.Clock

------------------------------------------------------------------------------

stringSplice :: (Monad m) => String -> Splice m
stringSplice = textSplice . T.pack

showSplice :: (Monad m, Show a) => a -> Splice m
showSplice = stringSplice . show

numericSplice :: (Num a, Show a, Monad m) => a -> Splice m
numericSplice = stringSplice . show

listSplice :: Monad m => Text -> [Text] -> Splice m
listSplice tag = mapSplices (\ x -> runChildrenWith $ tag ## textSplice x)

dateFormatSplice :: (Monad m, FormatTime t) => TimeLocale -> String -> t -> Splice m
dateFormatSplice locale defaultFormat t = do
	node <- getParamNode
	let
		format = maybe defaultFormat T.unpack $ X.getAttribute "format" node
	textSplice $ T.pack $ formatTime locale format t
