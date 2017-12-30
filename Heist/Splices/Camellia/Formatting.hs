{-# LANGUAGE OverloadedStrings, CPP #-}

module Heist.Splices.Camellia.Formatting where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import Data.List (intercalate, span)
import Data.List.Split (chunksOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Heist.Interpreted

-- for examining the Nodes
import Heist (getParamNode)
import qualified Text.XmlHtml as X hiding (render)

-- for dates
import Data.Time.Format
#if !MIN_VERSION_time(1,5,0)
import System.Locale (TimeLocale)
#endif

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
	in stringSplice $ commaWhole <> decimal

-- runs the provided Text through a Markdown parser
markdownSplice :: Monad m => T.Text -> Splice m
markdownSplice =
	return . fixListNodes . renderHtmlNodes . markdown defaultMarkdownSettings { msXssProtect = False } . T.fromStrict

-- This function is a pretty janky fix, but it's the best I can do at the moment.
-- see https://github.com/snoyberg/markdown/issues/36
fixListNodes :: [X.Node] -> [X.Node]
fixListNodes xs =
	let
		(cleanNodes, hasList) = span (`notElem` [X.TextNode "<ol>", X.TextNode "<ul>"]) xs
	in
		map fixElement cleanNodes ++ reformatList hasList
	where
		reformatList [] = []
		reformatList (X.TextNode tag : ys) =
			let
				(listItems, rest) = span isListItem ys
				tagName = T.dropAround (`elem` "<>") tag
			in
				X.Element tagName [] (map fixElement listItems) : fixListNodes (dropWhile (`elem` [X.TextNode "</ol>", X.TextNode "</ul>"]) rest)
		reformatList ys = ys
		-- ^ this last case shouldn't be possible to get to

		isListItem (X.Element "li" _ _) = True
		isListItem _ = False

		fixElement x@(X.Element{}) = x {X.elementChildren = fixListNodes $ X.childNodes x}
		fixElement x = x
