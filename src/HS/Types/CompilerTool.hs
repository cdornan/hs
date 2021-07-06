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

module HS.Types.CompilerTool
  ( Compiler(..)
  , Tool(..)
  , CompilerVersion(..)
  , ToolName(..)
  , compiler
  , compiler'
  , compilerVersion
  , toolVersion
  , compilerToGhcTool
  , compilerToTool
  , toolToCompiler
  , toolToCompiler'
  ) where

import           Data.Char
import           Data.Coerce
import           Data.Default
import           Data.Possibly
import           Data.Text(Text)
import qualified Data.Text              as T
import           Fmt
import           Text.Enum.Text


-- | @ghc@ or @ghc-8.10.4@, etc.
newtype Compiler = Compiler { getCompiler :: Maybe CompilerVersion }
  deriving (Eq,Ord,Show)

-- | @ghci@ or @ghc-pkg-9.0.1@, etc.
newtype Tool = Tool { getTool :: (ToolName,Maybe CompilerVersion) }
  deriving (Eq,Ord,Show)

-- | @8.6.5@, etc.
newtype CompilerVersion = CompilerVersion { getCompilerVersion :: (Int,Int,Int) }
  deriving (Eq,Ord,Show)

-- | @ghc@, @ghc-pkg@, @ghci@, etc.
data ToolName
  = TN_ghc
  | TN_ghc_pkg
  | TN_ghci
  | TN_haddock
  | TN_h2ps
  | TN_hpc
  | TN_hsc2hs
  | TN_runghc
  | TN_runhaskell
  deriving stock    (Bounded,Enum,Eq,Ord,Show)
  deriving anyclass (EnumText)
  deriving (Buildable,TextParsable) via UsingEnumText ToolName

instance Buildable    Compiler where build     = build_compiler
instance TextParsable Compiler where parseText = parse_compiler
instance Buildable    Tool     where build     = build_tool
instance TextParsable Tool     where parseText = parse_tool

instance Buildable CompilerVersion where
  build (CompilerVersion (a,b,c)) = ""+|a|+"."+|b|+"."+|c|+""

instance TextParsable CompilerVersion where
  parseText vrn =
    case T.splitOn "." vrn of
      [a_t,b_t,c_t] -> fmap CompilerVersion $
                            (,,) <$> parseText a_t <*> parseText b_t <*> parseText c_t
      _             -> Left $ "expected version a.b.c : "+|vrn|+""

instance Default CompilerVersion where
  def = CompilerVersion (8,10,4)

compiler :: CompilerVersion -> Compiler
compiler = compiler' . Just

compiler' :: Maybe CompilerVersion -> Compiler
compiler' = coerce

compilerVersion :: Compiler -> Maybe CompilerVersion
compilerVersion = coerce

toolVersion :: Tool -> Maybe CompilerVersion
toolVersion = compilerVersion . toolToCompiler

compilerToGhcTool :: Compiler -> Tool
compilerToGhcTool = compilerToTool . (,) TN_ghc

compilerToTool :: (ToolName,Compiler) -> Tool
compilerToTool = coerce

toolToCompiler :: Tool -> Compiler
toolToCompiler = snd . toolToCompiler'

toolToCompiler' :: Tool -> (ToolName,Compiler)
toolToCompiler' = coerce


----------------------------------------------------------------------------------------------------
-- build_compiler, parse_compiler
----------------------------------------------------------------------------------------------------

build_compiler :: Compiler -> Builder
build_compiler = build . compilerToGhcTool

parse_compiler :: Text -> Possibly Compiler
parse_compiler txt = chk =<< parseText txt
  where
    chk :: Tool -> Possibly Compiler
    chk tl | tn == TN_ghc = return cp
           | otherwise    = Left $ "expected 'ghc' but saw "+|tn|+" in: "+|txt|+""
      where
        (tn,cp) = toolToCompiler' tl


----------------------------------------------------------------------------------------------------
-- build_tool, parse_tool
----------------------------------------------------------------------------------------------------

build_tool :: Tool -> Builder
build_tool tl = ""+|tn|++|maybe "" (("-"<>) . build) mb_vrn
  where
    mb_vrn  = compilerVersion cp
    (tn,cp) = toolToCompiler' tl

parse_tool :: Text -> Possibly Tool
parse_tool txt0 = case T.breakOnEnd "-" txt0 of
    (frt,vrn) | not (T.null frt) && plausible_vrn vrn
                          -> with_vrn (T.dropWhileEnd (=='-') frt) vrn
              | otherwise -> Tool . (,Nothing) <$> parseText txt0
  where
    with_vrn :: Text -> Text -> Possibly Tool
    with_vrn tool vrn = fmap Tool $
          (,) <$> parseText tool <*> (Just <$> parseText vrn)

plausible_vrn :: Text -> Bool
plausible_vrn = T.all is_vrn_c
  where
    is_vrn_c :: Char -> Bool
    is_vrn_c c = isDigit c || c=='.'
