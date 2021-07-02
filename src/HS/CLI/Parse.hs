{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module HS.CLI.Parse where

import           HS.CLI.OptParse
import           HS.CLI.Types
import           Options.Applicative
import           System.Environment


parseCLI :: IO CLI
parseCLI = getArgs >>= parseIO cli_p

cli_p :: Psr CLI
cli_p = subparser $ mconcat
    [ cmd "version" vrn $ pure CLI_version
    , cmd "whereis" whr $      CLI_whereis   <$> imd_p <*> opt ghc_p
    , cmd "run"     run $      CLI_run       <$> imd_p <*> opt ghc_p
    , cmd "use"     use $      CLI_use       <$>           mny pvr_p
    , cmd "list"    lst $      CLI_list      <$>           opt ghc_p
    ]
  where
    vrn = "print out the version of this program"
    whr = "identify the location of a (GHC) compiler"
    run = "run the designated (GHC) compiler, installing needed and permitted, passing through the arguments"
    use = "specify the priority of the sources of (GHC) toolchain installations"
    lst = "list the visible (GHC) toolchains"

ghc_p :: Psr GHC
ghc_p = arg_p "<ghc-x.y.z>" "compiler to use"

imd_p :: Psr InstallMode
imd_p = arg_et_p "<install-mode>"

pvr_p :: Psr Provider
pvr_p = arg_p "<provider>" "provider of GHC installation (stack|ghcup|...)"
