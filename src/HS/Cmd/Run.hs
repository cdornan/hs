{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cmd.Run where

import qualified Data.Map             as Map
import           Data.Maybe
import qualified Data.Text            as T
import           HS.Install
import           HS.Types
import           System.Process.Typed


-- | command driver to run the given tool
cmdRun :: Cfg -> Maybe InstallMode -> Tool -> ToolArgs -> IO ()
cmdRun cfg mb_im tl tas = case mb_iln of
    Nothing  -> install cfg im cp >>= go
    Just iln -> go iln
  where
    go :: Installation -> IO ()
    go iln = runProcess_ $ proc (_iln_tool_executable iln tn) as

    (tn,cp) = toolToCompiler' tl
    im      = fromMaybe _cfg_mode mb_im
    mb_iln  = Map.lookup cp _cfg_installations
    as      = map T.unpack $ getToolArgs tas

    Cfg{..} = cfg
