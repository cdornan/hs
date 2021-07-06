{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cfg.Types where

import           Data.Default
import           Data.Map(Map)
import qualified Data.Map           as Map
import           Data.Maybe
import qualified Data.Text          as T
import           Fmt
import           HS.Managers.Types
import           HS.Types.CompilerTool
import           HS.Types.InstallMode
import           HS.Types.Manager
import           System.FilePath
import           Text.Enum.Text


data Cfg =
  Cfg
    { _cfg_managers      :: Managers
    , _cfg_mode          :: InstallMode
    , _cfg_compiler      :: CompilerVersion
    , _cfg_installations :: Map Compiler Installation
    }
  deriving (Show)

data Installation =
  Installation
    { _iln_compiler   :: CompilerVersion
    , _iln_manager    :: Manager
    , _iln_dir        :: FilePath
    }
  deriving (Show)

newtype Managers = Managers { getManagers :: [Manager] }
  deriving stock (Eq,Ord,Show)

instance Buildable Installation where
  build Installation{..} =
      "" +|padRightF 10 ' ' _iln_compiler|+
      " "+|padRightF  5 ' ' _iln_manager |+
      " "+|                 _iln_dir     |+
      ""

instance Buildable Managers where
  build = unwordsF . getManagers

instance TextParsable Managers where
  parseText = fmap Managers . mapM parseText . T.words

instance Default Managers where
  def = Managers [stack,ghcup]

_iln_executable :: Installation -> FilePath
_iln_executable iln = _iln_tool_executable iln TN_ghc

_iln_tool_executable :: Installation -> ToolName -> FilePath
_iln_tool_executable iln tnm = _iln_bin iln </> (fmt $ build tnm)

_iln_bin :: Installation -> FilePath
_iln_bin Installation{..} = _iln_dir </> "bin"

resolveCompilerVersion :: Cfg -> Compiler -> CompilerVersion
resolveCompilerVersion cfg cp = fromMaybe _cfg_compiler $ compilerVersion cp
  where
    Cfg{..} = cfg


----------------------------------------------------------------------------------------------------
-- testing tools
----------------------------------------------------------------------------------------------------

ghc, ghc_8'10'4, ghc_9'0'2 :: Compiler
ghc        = Compiler   Nothing
ghc_8'10'4 = Compiler $ Just v8'10'4
ghc_9'0'2  = Compiler $ Just v9'0'2

v8'10'4, v9'0'2 :: CompilerVersion
v8'10'4 = CompilerVersion (8,10,4)
v9'0'2  = CompilerVersion (9,0,2)

testCfg :: Cfg
testCfg =
    Cfg
      { _cfg_managers      = Managers [stack,ghcup]
      , _cfg_mode          = def
      , _cfg_compiler      = v8'10'4
      , _cfg_installations = Map.fromList as
      }
  where
    as :: [(Compiler,Installation)]
    as =
      [ (,) ghc        $ Installation v8'10'4 stack "/Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4"
      , (,) ghc_8'10'4 $ Installation v8'10'4 stack "/Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4"
      , (,) ghc_9'0'2  $ Installation v9'0'2  ghcup "/Users/chris/.ghcup/ghc/9.0.1"
      ]
