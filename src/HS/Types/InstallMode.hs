{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Types.InstallMode where

import           Data.Default
import           Fmt
import           Text.Enum.Text


-- | how to react to a missing toolchain?
data InstallMode
  = IM_no_install   -- ^ do not try to install, report error
  | IM_install      -- ^ download and install
  | IM_ask_install  -- ^ ask the user if they want to install the misssing toolchain
  deriving stock    (Bounded,Enum,Eq,Ord,Show)
  deriving anyclass (EnumText)
  deriving (Buildable,TextParsable) via UsingEnumText InstallMode

instance Default InstallMode where
  def = minBound
