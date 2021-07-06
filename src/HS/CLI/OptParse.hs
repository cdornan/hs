{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HS.CLI.OptParse where

import           Control.Applicative
import           Data.Char
import           Data.Default
import           Data.Maybe
import           Data.Possibly
import qualified Data.Text                   as T
import           Fmt
import           HS.CLI.ToolArgs
import qualified Options.Applicative         as OP
import           Options.Applicative.Builder
import           System.Environment
import           Text.Enum.Text


-- | the OA parser
type Psr a = OP.Parser a

-- | the OA optional operator
opt :: Psr a -> Psr (Maybe a)
opt = OP.optional

-- | the OA Kleene closure operator
mny :: Psr a -> Psr [a]
mny = OP.many


--------------------------------------------------------------------------------
-- parseArgs
--------------------------------------------------------------------------------

-- | main OA driver function
parseArgs :: forall a . (ToolArgs->Psr a) -> IO a
parseArgs psr = prs =<< getArgs
  where
    prs :: [String] -> IO a
    prs as0 = parseIO (psr tas) as
      where
        tas = ToolArgs $ map T.pack $ tail' dd_tas
        (as,dd_tas) = break (=="--") as0

        tail' :: [b] -> [b]
        tail' []    = []
        tail' (_:t) = t


--------------------------------------------------------------------------------
-- the low-level drivers
--------------------------------------------------------------------------------

parserPrefs :: OP.ParserPrefs
parserPrefs = OP.prefs showHelpOnEmpty

-- | making an IO parser
parseIO :: Psr a -> [String] -> IO a
parseIO psr as = OP.handleParseResult $
    OP.execParserPure parserPrefs (hsParserInfo $ psr) as

-- | making a functional parser
pureParse :: Psr a -> [String] -> Maybe a
pureParse p =
    OP.getParseResult . OP.execParserPure parserPrefs (hsParserInfo p)

-- | testing CLI parsers
testCLI :: Show a => Psr a -> [String] -> IO ()
testCLI psr ss = do
    x <- OP.handleParseResult $
              OP.execParserPure parserPrefs (hsParserInfo psr) ss
    print x


--------------------------------------------------------------------------------
-- hsParserInfo
--------------------------------------------------------------------------------

-- | given a 'Psr' makes up a corresponding @ParserInfo@
hsParserInfo :: Psr a -> OP.ParserInfo a
hsParserInfo p =
    OP.info (OP.helper <*> p)
         $  fullDesc
         <> progDesc "GHC installation manager manager"
         <> header   "towards a unified Haskell Development Environment"
         <> footer   "see --help for details of each sub-command"


--------------------------------------------------------------------------------
-- cmd
--------------------------------------------------------------------------------

-- | construct a sub-command parser from command name, description and parser
cmd :: String -> String -> Psr a -> OP.Mod OP.CommandFields a
cmd nme dsc psr = command nme $ info (OP.helper <*> psr) $ progDesc dsc


--------------------------------------------------------------------------------
-- parser builders
--------------------------------------------------------------------------------

cmd_et_p :: EnumText a => String -> (a->String) -> Psr a
cmd_et_p hlp c_hlp = subparser $ mconcat $
    commandGroup hlp :
      [ cmd (fmt $ build c) (c_hlp c) $ pure c
        | c <- [minBound..maxBound]
        ]

-- | parsing an argument EnumText argument
arg_et_optd :: forall a . EnumText a => String -> a -> Psr a
arg_et_optd var df = fromMaybe df <$> ss_p
  where
    ss_p :: Psr (Maybe a)
    ss_p = opt $ arg_et_p var

-- | parsing an argument EnumText argument
arg_et_p :: forall a . (Bounded a,Enum a,Buildable a,TextParsable a) => String -> Psr a
arg_et_p var = arg_p var hlp
  where
    hlp = T.unpack $ T.intercalate "|" $
                map (fmt . build) [minBound..maxBound :: a]

-- | pasring an EnumText option
opt_et_p :: forall a . EnumText a => Char -> String -> Psr a
opt_et_p c var = opt_p c var hlp
  where
    hlp = T.unpack $ T.intercalate "|" $
                map (fmt . build) [minBound..maxBound :: a]

-- | pasring a TextParsable argument
arg_p :: TextParsable a => String -> String -> Psr a
arg_p = arg_p' parseText

-- | pasring an argument ParseText, when passed the parser explicitly
arg_p' :: (T.Text->Possibly a) -> String -> String -> Psr a
arg_p' prs var hlp = argument (eitherReader $ prs . T.pack)
      $  metavar var
      <> help    hlp

-- | parsing a TextParsable option
opt_p :: TextParsable a => Char -> String -> String -> Psr a
opt_p ch nme hlp = option (eitherReader parseString)
      $  metavar var
      <> short   ch
      <> long    lng
      <> help    hlp
  where
    var = map toUpper nme
    lng = map toLower nme

enum_switches_with_def_p :: forall a . (Default a,EnumText a) => Psr a
enum_switches_with_def_p = fmap (fromMaybe def) $ opt $ short_enum_switches_p $ const Nothing

enum_switches_p :: forall a . EnumText a => Psr a
enum_switches_p = short_enum_switches_p $ const Nothing

short_enum_switches_p :: forall a . EnumText a => (a->Maybe Char) -> Psr a
short_enum_switches_p sh_f = foldr (<|>) empty $ map mk [minBound..maxBound]
  where
    mk :: a -> Psr a
    mk x = OP.flag' x $ (long $ fmt $ build x) <> shrt
      where
        shrt = case sh_f x of
          Nothing -> mempty
          Just c  -> short c

parseString :: TextParsable a => String -> Possibly a
parseString = parseText . T.pack
