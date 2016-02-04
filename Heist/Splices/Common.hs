{-# LANGUAGE OverloadedStrings, TypeOperators, ScopedTypeVariables #-}
-- TODO: move some of this to a general purpose library

module Heist.Splices.Common where

------------------------------------------------------------------------------

--import Control.Monad.Trans (MonadIO)
--import Data.ByteString (ByteString)
--import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
--import Data.Monoid ((<>))
--import Heist (getParamNode)
import Heist.Interpreted
import Heist.SpliceAPI
--import qualified Text.XmlHtml as X hiding (render)
import Data.List.Split (chunksOf)

import Heist.Splices.Types

{----------------------------------------------------------------------------------------------------{
                                                                       | Generic Splices
}----------------------------------------------------------------------------------------------------}

listToSplice :: Monad m => (a -> Splices (Splice m)) -> [a] -> Splice m
listToSplice splice = mapSplices (runChildrenWith . splice)

rowspanSplice :: (Monad m, Eq a) => Splices (Splice m) -> (a -> Splices (Splice m)) -> [a] -> Splice m
rowspanSplice spanSplices rowSplices xs = listToSplice splices xs
	where
		splices x = do
			"spanned" ## (if x == head xs
				then runChildrenWith $ do
					"rows" ## numericSplice $ length xs
					spanSplices
				else hideContents)
			rowSplices x

pageSplice :: (Num a, Show a, Monad m) => a -> Splice m
pageSplice x = runChildrenWith $ "page" ## numericSplice x

chunkedListToSplice :: Monad m => Int -> T.Text -> (a -> Splices (Splice m)) -> [a] -> Splice m
chunkedListToSplice i name splices =
	listToSplice splices' . chunksOf i
		where
			splices' xs = name ## listToSplice splices xs

{----------------------------------------------------------------------------------------------------{
                                                                       | Visibility Splices
}----------------------------------------------------------------------------------------------------}

hideContents :: Monad m => Splice m
hideContents = return []

showContents :: Monad m => Splice m
showContents = runChildren

eitherSplice :: Monad m => Bool -> Splice m -> Splice m -> Splice m
eitherSplice True a _ = a
eitherSplice False _ b = b

toggleSplice :: Monad m => Bool -> Splice m
toggleSplice x = eitherSplice x showContents hideContents

ifSplice :: Monad m => Bool -> Splice m -> Splice m -> Splice m
ifSplice x a b = runChildrenWith $ do
	"yes" ## eitherSplice x a hideContents
	"no" ## eitherSplice x hideContents b

ifSplice' :: Monad m => Bool -> Splice m
ifSplice' x = ifSplice x showContents showContents

hasSplice :: Monad m => Maybe a -> Splice m
hasSplice = toggleSplice . isJust

hasListSplice :: Monad m => Text -> (a -> Splices (Splice m)) -> [a] -> Splice m
hasListSplice _ _ [] = hideContents
hasListSplice tag splices xs = runChildrenWith $ tag ## listToSplice splices xs

eitherTextSplice :: Monad m => Bool -> Text -> Text -> Splice m
eitherTextSplice True a _ = textSplice a
eitherTextSplice False _ b = textSplice b

