{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cmd.Dump (cmdDump) where

import           Data.Maybe
import qualified Data.Text.IO       as T
import           Fmt
import           HS.Cfg.CfgFile
import           HS.Types
import           System.Directory
import           System.FilePath
import           Text.Enum.Text


-- | command driver to write the `cabal` wrappers out to @~/.hs/bin@
cmdDump :: Cfg -> Maybe InstallMode -> IO ()
cmdDump cfg mb = do
    bin <- bin_dir cfg
    fmtLn $ "writing to "+|bin|+":"
    sequence_
      [ write_wrapper cfg bin im tnm vrn
        | vrn <- versions
        , tnm <- [minBound..maxBound]
        ]
  where
    im = fromMaybe (_cfg_mode cfg) mb

write_wrapper :: Cfg -> FilePath -> InstallMode -> ToolName -> CompilerVersion -> IO ()
write_wrapper _ dir imd tn cv = do
    fmtLn $ " "+|tl|+""
    T.writeFile fp $ fmt $ unlinesF
      [ "#!/usr/bin/env bash"
      , "hs run "+|tl|+" --"+|imd|+" -- \"$@\""    :: Builder
      ]
    setPermissions fp . setOwnerExecutable True =<< getPermissions fp
  where
    fp = dir </> fmt tl
    tl = ""+|tn|+"-"+|cv|+"" :: Builder

bin_dir :: Cfg -> IO FilePath
bin_dir _ = mk =<< dotHs
  where
    mk dh = const bin <$> createDirectoryIfMissing True bin
      where
        bin = dh </> "bin"

versions :: [CompilerVersion]
versions = either error id $ mapM parseText
  [ "9.2.1"

  , "9.0.2"
  , "9.0.1"

  , "8.10.6"
  , "8.10.5"
  , "8.10.4"
  , "8.10.3"
  , "8.10.2"
  , "8.10.1"

  , "8.8.4"
  , "8.8.3"
  , "8.8.2"
  , "8.8.1"

  , "8.6.5"
  , "8.6.4"
  , "8.6.3"
  , "8.6.2"
  , "8.6.1"

  , "8.4.4"
  , "8.4.3"
  , "8.4.2"
  , "8.4.1"

  , "8.2.2"
  , "8.2.1"

  , "8.0.2"
  , "8.0.1"

  , "7.10.3"
  , "7.10.2"
  , "7.10.1"

  , "7.8.4"
  , "7.8.3"
  , "7.8.2"
  , "7.8.1"

  , "7.6.3"
  , "7.6.2"
  , "7.6.1"

  , "7.4.2"
  , "7.4.1"

  , "7.2.2"
  , "7.2.1"

  , "7.0.4"
  , "7.0.3"
  , "7.0.2"
  , "7.0.1"

  , "6.12.3"
  , "6.12.2"
  , "6.12.1"
  ]
