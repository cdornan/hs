{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HS.Types.Manager where

import           Data.String
import           Data.Text(Text)
import           Fmt
import           Text.Enum.Text


-- | installation manager @stack@ or @ghcup@ or ...
newtype Manager = Manager { getManager :: Text }
  deriving stock (Eq,Ord,Show)
  deriving newtype (Buildable,IsString,TextParsable)
