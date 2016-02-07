{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.BlueBird.Types where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted

-- for examining the Nodes
import Heist (getParamNode)
import qualified Text.XmlHtml as X hiding (render)

-- for dates
import Data.Time.Format
import System.Locale

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
