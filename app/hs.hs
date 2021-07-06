{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           HS
import           HS.CLI


main :: IO ()
main = hs =<< parseCLI
