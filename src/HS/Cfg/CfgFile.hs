{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Cfg.CfgFile where

import           Data.Default
import           Data.Text(Text)
import qualified Data.Text.IO         as T
import           Fmt
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Enum.Text


-- | enumeration for the @hs@ configuration files
data CfgFile
  = CF_managers
  | CF_mode
  | CF_compiler
  deriving stock    (Bounded,Enum,Eq,Ord,Show)
  deriving anyclass (EnumText)
  deriving (Buildable,TextParsable) via UsingEnumText CfgFile

-- | load a configuration file
load :: forall a . (Default a,TextParsable a) => CfgFile -> IO a
load cf = rd =<< cfgFile cf
  where
    rd :: FilePath -> IO a
    rd fp = do
      yup <- doesFileExist fp
      if | yup       -> prs fp =<< T.readFile fp
         | otherwise -> return def

    prs :: FilePath -> Text -> IO a
    prs fp = either (oops fp) return . parseText

    oops :: FilePath -> String -> IO a
    oops fp msg = do
      hPutStr stderr $
        "hs: warning: failed to parse "+||fp||+" with "+||msg||+"; using default"
      return def

-- | save a configuration file
save :: Buildable a => CfgFile -> a -> IO ()
save cf x = flip T.writeFile (fmt $ build x) =<< cfgFile cf

-- | generate the 'FilePath' for a 'CfgFile'
cfgFile :: CfgFile -> IO FilePath
cfgFile cf = (</> (fmt $ build cf)) <$> dotHs

-- | generate the @hs@ configuration directory 'FilePath'
dotHs :: IO FilePath
dotHs = do
    dhs <- (</> ".hs") <$> getHomeDirectory
    const dhs <$> createDirectoryIfMissing True dhs
