{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Lists where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted

import Data.List.Split (chunksOf)

import Heist.Splices.Camellia.Types
import Heist.Splices.Camellia.Visibility

------------------------------------------------------------------------------

listToSplice :: Monad m => (a -> MapSyntax Text (Splice m)) -> [a] -> Splice m
listToSplice splice = mapSplices (runChildrenWith . splice)

{-# DEPRECATED hasListSplice "I'm expecting to remove this function soon(tm)" #-}
hasListSplice :: Monad m => Text -> (a -> MapSyntax Text (Splice m)) -> [a] -> Splice m
hasListSplice _ _ [] = hideContents
hasListSplice tag splices xs = runChildrenWith $ tag ## listToSplice splices xs

rowspanSplice :: (Monad m, Eq a) => MapSyntax Text (Splice m) -> (a -> MapSyntax Text (Splice m)) -> [a] -> Splice m
rowspanSplice spanSplices rowSplices xs = listToSplice splices xs
	where
		splices x = do
			"spanned" ## (if x == head xs
				then runChildrenWith $ do
					"rows" ## numericSplice $ length xs
					spanSplices
				else hideContents)
			rowSplices x

chunkedListToSplice :: Monad m => Int -> Text -> (a -> MapSyntax Text (Splice m)) -> [a] -> Splice m
chunkedListToSplice i name splices =
	listToSplice splices' . chunksOf i
		where
			splices' xs = name ## listToSplice splices xs
