{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Formatting where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Heist.Interpreted

-- for examining the Nodes
import Heist (getParamNode)
import qualified Text.XmlHtml as X hiding (render)

-- for dates
import Data.Time.Format
import System.Locale

-- for Markdown
import qualified Data.Text.Lazy as T (fromStrict)
import Text.Markdown (markdown, defaultMarkdownSettings, msXssProtect)
import Text.Blaze.Renderer.XmlHtml (renderHtmlNodes)


import Heist.Splices.Camellia.Types

------------------------------------------------------------------------------

-- this splice examines the value of the `format` attribute to allow controlling
-- of the date's appearance from the template
dateFormatSplice :: (Monad m, FormatTime t) => TimeLocale -> String -> t -> Splice m
dateFormatSplice locale defaultFormat t = do
	node <- getParamNode
	let
		format = maybe defaultFormat T.unpack $ X.getAttribute "format" node
	textSplice $ T.pack $ formatTime locale format t

-- this converts a number to a comma delimited number
prettyNumberSplice :: (Monad m, Num n, Show n) => n -> Splice m
prettyNumberSplice i =
	let
		(whole, decimal) = break (== '.') $ show i
		commaWhole = reverse $ intercalate "," $ chunksOf 3 $ reverse whole
		dotHalf = if not (null decimal) then '.' : decimal else []
	in stringSplice $ commaWhole <> dotHalf

-- runs the provided Text through a Markdown parser
markdownSplice :: Monad m => T.Text -> Splice m
markdownSplice =
	return . renderHtmlNodes . markdown defaultMarkdownSettings { msXssProtect = False } . T.fromStrict
