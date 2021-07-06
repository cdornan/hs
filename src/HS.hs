{-# LANGUAGE OverloadedStrings          #-}

module HS
  ( hs
  , module HS.CLI.CLI
  ) where

import           Data.Version
import           Fmt
import           HS.Cfg
import           HS.CLI
import           HS.CLI.CLI
import           HS.Cmd
import           Paths_hs


-- | run an @hs@ command
hs :: CLI -> IO ()
hs cli =
  case cli of
    CLI_version               -> fmtLn $ ""+|showVersion version|+""
    CLI_whereis    imd cp     -> ld $ \cfg -> cmdWhereis cfg imd cp
    CLI_run        imd cp as  -> ld $ \cfg -> cmdRun     cfg imd cp as
    CLI_list           cp     -> ld $ \cfg -> cmdList    cfg     cp
    CLI_use            is     -> ld $ \cfg -> cmdUse     cfg     is
    CLI_use_install_mode  imd -> ld $ \cfg -> cmdUseIM   cfg     imd
    CLI_use_compiler      cpv -> ld $ \cfg -> cmdUseCp   cfg     cpv
    CLI_dump_ghc_wrappers imd -> ld $ \cfg -> cmdDump    cfg     imd
  where
    ld :: (Cfg -> IO ()) -> IO ()
    ld cmd = cmd =<< loadCfg
