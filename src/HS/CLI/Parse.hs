{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.CLI.Parse where

import           HS.CLI.OptParse
import           HS.CLI.ToolArgs
import           HS.CLI.CLI
import           Options.Applicative


-- | parse the command line
parseCLI :: IO CLI
parseCLI = parseArgs cli_p

cli_p :: ToolArgs -> Psr CLI
cli_p tas = subparser $ mconcat
    [ cmd "version"           vrn $ pure CLI_version
    , cmd "whereis"           whr $      CLI_whereis       <$> opt inmd_p <*> cplr_p
    , cmd "run"               run $      CLI_run           <$> opt inmd_p <*> tool_p <*> pure tas
    , cmd "list"              lst $      CLI_list              <$>            opt cplr_p
    , cmd "use"               use $      CLI_use               <$>            mny mngr_p
    , cmd "use-install-mode"  uim $      CLI_use_install_mode  <$>            opt imda_p
    , cmd "use-compiler"      ucp $      CLI_use_compiler      <$>            opt cpvn_p
    , cmd "dump-ghc-wrappers" dgw $      CLI_dump_ghc_wrappers <$>            opt imda_p
    ]
  where
    vrn = "print out the version of this program"
    whr = "identify the location of a (Compiler) compiler"
    run = "run the designated (Compiler) compiler, installing needed and permitted, passing through the arguments"
    use = "specify the priority of the sources of (Compiler) toolchain installations"
    lst = "list the visible (Compiler) toolchains"
    uim = "set the default install mode"
    ucp = "set the default compiler"
    dgw = "dump the named directory GHC tool wrappers the indirect through hs"

-- flags/switches

inmd_p :: Psr InstallMode
inmd_p = enum_switches_p

-- arguments

imda_p :: Psr InstallMode
imda_p = arg_et_p "<installation-mode>"

cplr_p :: Psr Compiler
cplr_p = arg_p "<ghc-x.y.z>" "compiler to use"

cpvn_p :: Psr CompilerVersion
cpvn_p = arg_p "x.y.z" "version of the compiler to use"

tool_p :: Psr Tool
tool_p = arg_p "<tool-x.y.z>" "compiler to use"

mngr_p :: Psr Manager
mngr_p = arg_p "<manager>" "manager of Compiler installation (stack|ghcup|...)"
