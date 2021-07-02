module Main where

import           HS.CLI
import           HS.Cmd

main :: IO ()
main = do
  cli <- parseCLI
  case cli of
    CLI_version         -> cmdVersion
    CLI_whereis imd mb  -> cmdWhereis imd mb
    CLI_run     imd mb  -> cmdRun     imd mb
    CLI_use         pvs -> cmdUse         pvs
    CLI_list        mb  -> cmdList        mb
    
