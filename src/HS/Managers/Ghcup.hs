{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Managers.Ghcup
  ( installGhcup
  , ghcupDiscover
  ) where

import           Control.Exception
import           Data.Map(Map)
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Text              as T
import           Fmt
import           HS.Cfg.Types
import           HS.Managers.Types
import           HS.Types.CompilerTool
import           System.Directory
import           System.Process.Typed
import           Text.Enum.Text


-- | use @ghcup@ to install a toolchain
installGhcup :: Cfg -> CompilerVersion -> IO Installation
installGhcup cfg cv = do
    runProcess_ $ proc "ghcup" ["install","ghc","ghc-"+|cv|+""]
    dir <- ghcupInstallationDir cfg cv
    return
      Installation
        { _iln_compiler = cv
        , _iln_manager  = ghcup
        , _iln_dir      = dir
        }

-- | locate all of the toolchains being managed by the local @ghcup@ installation
ghcupDiscover :: Cfg -> IO (Map Compiler Installation)
ghcupDiscover cfg = handle hdl $ mk =<< listDirectory =<< ghcupStashDir cfg
  where
    mk :: [FilePath] -> IO (Map Compiler Installation)
    mk fps = mk' fps <$> ghcupStashDir cfg

    mk' :: [FilePath] -> FilePath -> Map Compiler Installation
    mk' fps sr = Map.fromList $ catMaybes $ map (chk sr) fps

    chk :: FilePath -> FilePath -> Maybe (Compiler,Installation)
    chk sr fp = do
        cv  <- either (const Nothing) Just $ parseText $ T.pack fp
        return $ (,) (compiler cv)
          Installation
            { _iln_compiler = cv
            , _iln_manager  = ghcup
            , _iln_dir      = ghcupInstallationDir' cfg cv sr
            }

    hdl :: SomeException -> IO (Map Compiler Installation)
    hdl _ = return mempty

ghcupInstallationDir :: Cfg -> CompilerVersion -> IO FilePath
ghcupInstallationDir cfg cv = ghcupInstallationDir' cfg cv <$> ghcupStashDir cfg

ghcupInstallationDir' :: Cfg -> CompilerVersion -> FilePath -> FilePath
ghcupInstallationDir' _ cv sr = ""+|sr|+"/"+|cv|+""

ghcupStashDir :: Cfg -> IO FilePath
ghcupStashDir _ = mk <$> getHomeDirectory
  where
    mk :: FilePath -> FilePath
    mk hme = ""+|hme|+"/.ghcup/ghc"
