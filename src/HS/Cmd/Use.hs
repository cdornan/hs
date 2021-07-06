{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cmd.Use where

import           HS.Cfg.CfgFile
import           HS.Types


-- | command driver to configure installation manager priorities
cmdUse :: Cfg -> [Manager] -> IO ()
cmdUse Cfg{..} ms = case ms of
  [] -> fmtLn $ build _cfg_managers
  _  -> save CF_managers $ Managers ms

-- | command driver to configure default @InstallMode@
cmdUseIM :: Cfg -> Maybe InstallMode -> IO ()
cmdUseIM Cfg{..} mb = case mb of
  Nothing -> fmtLn $ build _cfg_mode
  Just im -> save CF_mode im

-- | command driver to configure default toolchain
cmdUseCp :: Cfg -> Maybe CompilerVersion -> IO ()
cmdUseCp Cfg{..} mb = case mb of
  Nothing -> fmtLn $ build _cfg_compiler
  Just cp -> save CF_compiler cp
