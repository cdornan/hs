{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cmd.Whereis where

import qualified Data.Map       as Map
import           Data.Maybe
import           HS.Install
import           HS.Types


-- | command driver to print out bindist root of desiganted compiler
cmdWhereis :: Cfg -> Maybe InstallMode -> Compiler -> IO ()
cmdWhereis cfg mb_im cp = case mb_iln of
    Nothing  -> install cfg im cp >>= go
    Just iln -> go iln
  where
    go :: Installation -> IO ()
    go iln = putStrLn $ _iln_dir iln

    im     = fromMaybe _cfg_mode mb_im
    mb_iln = Map.lookup cp _cfg_installations

    Cfg{..} = cfg
