{-# LANGUAGE OverloadedStrings          #-}

module HS.CLI.ToolArgs where

import           Data.Text(Text)
import           Fmt


-- | list of command line arguments to be passed through to the compiler tool
newtype ToolArgs = ToolArgs { getToolArgs :: [Text] }
  deriving (Show)

instance Buildable ToolArgs where
  build (ToolArgs as) = unwordsF as
