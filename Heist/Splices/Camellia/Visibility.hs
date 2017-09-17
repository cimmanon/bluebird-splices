{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Visibility where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted

import Heist.Splices.Camellia.Types

{----------------------------------------------------------------------------------------------------{
                                                                      | Base Splices
}----------------------------------------------------------------------------------------------------}

hideContents :: Monad m => Splice m
hideContents = return []

showContents :: Monad m => Splice m
showContents = runChildren

------------------------------------------------------------------------------

{-# DEPRECATED eitherSplice "eitherSplice has been renamed to toggleSpliceWith, and will be removed in a later version" #-}
eitherSplice :: Monad m => Bool -> Splice m -> Splice m -> Splice m
eitherSplice = toggleSpliceWith

toggleSpliceWith :: Monad m => Bool -> Splice m -> Splice m -> Splice m
toggleSpliceWith True a _ = a
toggleSpliceWith False _ b = b

toggleSplice :: Monad m => Bool -> Splice m
toggleSplice x = toggleSpliceWith x showContents hideContents

maybeSplice :: Monad m => (a -> Splice m) -> Maybe a -> Splice m
maybeSplice = maybe hideContents

{----------------------------------------------------------------------------------------------------{
                                                                      | Splices with extra elements
}----------------------------------------------------------------------------------------------------}

ifSpliceWith :: Monad m => Bool -> Splice m -> Splice m -> Splice m
ifSpliceWith x a b = runChildrenWith $ do
	"yes" ## toggleSpliceWith x a hideContents
	"no" ## toggleSpliceWith x hideContents b

ifSplice :: Monad m => Bool -> Splice m
ifSplice x = ifSpliceWith x showContents showContents

{-# DEPRECATED ifSplice' "ifSplice' has been renamed to ifSplice, the original ifSplice was renamed to ifSpliceWith" #-}
ifSplice' :: Monad m => Bool -> Splice m
ifSplice' = ifSplice

{-# DEPRECATED hasSplice, eitherTextSplice "I'm expecting to remove this function soon(tm)" #-}
-- do we even use this splice?
hasSplice :: Monad m => Maybe a -> Splice m
hasSplice = maybeSplice (const showContents)

eitherTextSplice :: Monad m => Bool -> Text -> Text -> Splice m
eitherTextSplice True a _ = textSplice a
eitherTextSplice False _ b = textSplice b
