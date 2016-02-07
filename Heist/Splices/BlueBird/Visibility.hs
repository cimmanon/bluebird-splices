{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.BlueBird.Visibility where

------------------------------------------------------------------------------

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted
import Heist.SpliceAPI
import Data.List.Split (chunksOf)

import Heist.Splices.BlueBird.Types

{----------------------------------------------------------------------------------------------------{
                                                                      | Base Splices
}----------------------------------------------------------------------------------------------------}

hideContents :: Monad m => Splice m
hideContents = return []

showContents :: Monad m => Splice m
showContents = runChildren

------------------------------------------------------------------------------

eitherSplice :: Monad m => Bool -> Splice m -> Splice m -> Splice m
eitherSplice True a _ = a
eitherSplice False _ b = b

toggleSplice :: Monad m => Bool -> Splice m
toggleSplice x = eitherSplice x showContents hideContents

maybeSplice :: Monad m => (a -> Splice m) -> Maybe a -> Splice m
maybeSplice = maybe hideContents

{----------------------------------------------------------------------------------------------------{
                                                                      | Splices with extra elements
}----------------------------------------------------------------------------------------------------}

ifSplice :: Monad m => Bool -> Splice m -> Splice m -> Splice m
ifSplice x a b = runChildrenWith $ do
	"yes" ## eitherSplice x a hideContents
	"no" ## eitherSplice x hideContents b

ifSplice' :: Monad m => Bool -> Splice m
ifSplice' x = ifSplice x showContents showContents

{-# DEPRECATED hasSplice "I'm expecting to remove this function soon(tm)" #-}
-- do we even use this splice?
hasSplice :: Monad m => Maybe a -> Splice m
hasSplice = maybeSplice (const showContents)

eitherTextSplice :: Monad m => Bool -> Text -> Text -> Splice m
eitherTextSplice True a _ = textSplice a
eitherTextSplice False _ b = textSplice b
