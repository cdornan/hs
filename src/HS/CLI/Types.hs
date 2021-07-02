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

module HS.CLI.Types where

import           Data.Possibly
import           Data.String
import           Data.Text(Text)
import qualified Data.Text              as T
import           Fmt
import           Text.Enum.Text


data CLI
  = CLI_version
  | CLI_whereis InstallMode (Maybe GHC)
  | CLI_run     InstallMode (Maybe GHC)
  | CLI_use                 [Provider]
  | CLI_list                (Maybe GHC)
  deriving (Show)

data InstallMode
  = IM_no_install
  | IM_install
  | IM_ask_install
  deriving stock    (Bounded,Enum,Eq,Ord,Show)
  deriving anyclass (EnumText)
  deriving (Buildable,TextParsable) via UsingEnumText InstallMode

newtype Provider = Provider { getProvider :: Text }
  deriving stock (Eq,Ord,Show)
  deriving newtype (Buildable,IsString,TextParsable)

newtype GHC = GHC { getGHC :: (Int,Int,Int) }
  deriving (Show)

instance Buildable    GHC where build     = buildGHC
instance TextParsable GHC where parseText = parseGHC


----------------------------------------------------------------------------------------------------
-- buildGHC, parseGHC
----------------------------------------------------------------------------------------------------

buildGHC :: GHC -> Builder
buildGHC (GHC (a,b,c))= "ghc-"+|a|+"."+|b|+"."+|c|+""

parseGHC :: Text -> Possibly GHC
parseGHC txt0 = fmap GHC $ do
    vrn <- check_pfx txt0
    case T.splitOn "." vrn of
      [a_t,b_t,c_t] -> (,,) <$> parseText a_t <*> parseText b_t <*> parseText c_t
      _             -> Left $ "expected ghc-a.b.c : "+|txt0|+""
  where
    check_pfx :: Text -> Possibly Text
    check_pfx txt
      | "ghc-" `T.isPrefixOf` txt = return $ T.drop 4 txt
      | otherwise               = Left $ "expected compiler to start with 'ghc-' : "+|txt|+""
