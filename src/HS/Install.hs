{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module HS.Install(install) where

import qualified Data.Text              as T
import           HS.Managers
import           HS.Types
import           System.IO


-- | call out to the configured installer to install a bindist
install :: Cfg -> InstallMode -> Compiler -> IO Installation
install cfg im cp = case getManagers $ _cfg_managers cfg of
  []    -> not_found cp "no managers"
  mgr:_ -> case im of
    IM_no_install  -> not_found cp "not in auto-installing mode"
    IM_install     -> install' cfg mgr cp
    IM_ask_install -> do
        yup <- ask cp
        if | yup -> install' cfg mgr cp
           | otherwise -> not_found cp "declined"

ask :: Compiler -> IO Bool
ask cp = do
    bm <- hGetBuffering stderr
    hSetBuffering stderr NoBuffering
    hPutStrLn stderr $ "toolchain "+|cp|+" is required but not installed"
    hPutStr   stderr $ "would you like to install it? (y/n) : "
    tx <- T.strip . T.pack <$> getLine
    hSetBuffering stderr bm
    return $ tx `elem` ["y","yes"]

install' :: Cfg -> Manager -> Compiler -> IO Installation
install' cfg mgr cp
    | mgr==stack = installStack cfg cv
    | mgr==ghcup = installGhcup cfg cv
    | otherwise  = not_found cp $ "manager "+|mgr|+" not recognised"
  where
    cv = resolveCompilerVersion cfg cp

not_found :: Compiler -> String -> IO a
not_found cp ctx = fail $ "toolchain "+|cp|+" not installed ("+|ctx|+")"
