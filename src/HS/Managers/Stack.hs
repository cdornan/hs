{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Managers.Stack
  ( installStack
  , stackDiscover
  ) where

import           Control.Exception
import           Data.Char
import           Data.Map(Map)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Text(Text)
import qualified Data.Text              as T
import           Fmt
import           HS.Cfg.Types
import           HS.Managers.Types
import           HS.Types.CompilerTool
import           System.Directory
import           System.FilePath
import           System.Info(os,arch)
import           System.Process.Typed
import           Text.Enum.Text


-- | use @stack@ to install a toolchain
installStack :: Cfg -> CompilerVersion -> IO Installation
installStack cfg cv = do
    runProcess_ $ proc "stack" ["setup","ghc-"+|cv|+""]
    dir <- stackInstallationDir cfg cv
    return
      Installation
        { _iln_compiler   = cv
        , _iln_manager    = stack
        , _iln_dir        = dir
        }

-- | locate all of the toolchains being managed by the local stack installation
stackDiscover :: Cfg -> IO (Map Compiler Installation)
stackDiscover cfg = handle hdl $ mk =<< listDirectory =<< stackStashDir cfg
  where
    mk :: [FilePath] -> IO (Map Compiler Installation)
    mk fps = mk' fps <$> stackStashDir cfg

    mk' :: [FilePath] -> FilePath -> Map Compiler Installation
    mk' fps sr = Map.fromList $ catMaybes $ map (chk sr) fps

    chk :: FilePath -> FilePath -> Maybe (Compiler,Installation)
    chk sr fp = do
        cv  <- either (const Nothing) Just $ parseText cv_t
        return $ (,) (Compiler $ Just cv)
          Installation
            { _iln_compiler   = cv
            , _iln_manager    = stack
            , _iln_dir        = sr </> fp
            }
      where
        cv_t :: Text
        cv_t = T.takeWhileEnd isvc $ T.pack fp
          where
            isvc c = isDigit c || c=='.'

    hdl :: SomeException -> IO (Map Compiler Installation)
    hdl _ = return mempty

stackInstallationDir :: Cfg -> CompilerVersion -> IO FilePath
stackInstallationDir cfg cv = stackInstallationDir' cfg cv <$> stackStashDir cfg

stackInstallationDir' :: Cfg -> CompilerVersion -> FilePath -> FilePath
stackInstallationDir' _ cv sr = ""+|sr|+"/ghc-"+|cv|+""

stackStashDir :: Cfg -> IO FilePath
stackStashDir _ = mk <$> getHomeDirectory
  where
    mk :: FilePath -> FilePath
    mk hme = ""+|hme|+"/.stack/programs/"+|arch|+"-"+|os'|+""

    os' :: String
    os' =
      if | os=="darwin" -> "osx"
         | otherwise    -> os
