{-# LANGUAGE OverloadedStrings #-}

module Heist.Splices.Camellia.Types where

------------------------------------------------------------------------------

import Data.Map.Syntax
import Data.Text (Text)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Heist.Interpreted

------------------------------------------------------------------------------

stringSplice :: (Monad m) => String -> Splice m
stringSplice = textSplice . T.pack

showSplice :: (Monad m, Show a) => a -> Splice m
showSplice = stringSplice . show

numericSplice :: (Num a, Show a, Monad m) => a -> Splice m
numericSplice = stringSplice . show

listSplice :: Monad m => Text -> [Text] -> Splice m
listSplice tag = mapSplices (\ x -> runChildrenWith $ tag ## textSplice x)
