{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cfg.Load where

import           Data.Map(Map)
import           Fmt
import           HS.Cfg.CfgFile
import           HS.Managers
import           HS.Types
import           System.IO


loadCfg :: IO Cfg
loadCfg = ld =<< recover
  where
    ld :: Cfg -> IO Cfg
    ld cfg0 = fmap (mk . mconcat) $ mapM (disco cfg0) $ getManagers _cfg_managers
      where
        mk :: Map Compiler Installation -> Cfg
        mk mp = cfg0
          { _cfg_installations = mp
          }

        Cfg{..} = cfg0

    disco :: Cfg -> Manager -> IO (Map Compiler Installation)
    disco cfg mgr
      | mgr == stack = stackDiscover cfg
      | mgr == ghcup = ghcupDiscover cfg
      | otherwise    = do
          hPutStrLn stderr $ "hs: manager "+|mgr|+" not recognised: ignoring"
          return mempty

recover :: IO Cfg
recover = Cfg <$> load CF_managers <*> load CF_mode <*> load CF_compiler <*> pure mempty
