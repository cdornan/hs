{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module HS.CLI.CLI
  ( CLI(..)
  , InstallMode(..)
  , Compiler(..)
  , CompilerVersion(..)
  , Tool(..)
  , ToolName(..)
  , ToolArgs(..)
  , Manager(..)
  ) where

import           HS.CLI.ToolArgs
import           HS.Types.CompilerTool
import           HS.Types.InstallMode
import           HS.Types.Manager


-- | command line abstrax syntax type
data CLI
  = CLI_version
  | CLI_whereis (Maybe InstallMode) Compiler
  | CLI_run     (Maybe InstallMode) Tool ToolArgs
  | CLI_list                        (Maybe Compiler)
  | CLI_use                         [Manager]
  | CLI_use_install_mode            (Maybe InstallMode)
  | CLI_use_compiler                (Maybe CompilerVersion)
  | CLI_dump_ghc_wrappers           (Maybe InstallMode)
  deriving (Show)
