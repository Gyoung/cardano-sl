{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP                   #-}

module Main ( module Main ) where

import           Universum

import           Control.Monad (forM)
import           Data.List (intersperse)
import           Data.Text (Text)
import           Data.Version (showVersion)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, execParser, footer, fullDesc,
                     header, help, helper, info, infoOption, long, metavar,
                     progDesc, strOption)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.Environment (getProgName)
import           System.FilePath.Posix ((<.>), (</>))
import           System.Process (readProcess)

#ifdef VERSION_everything
import           Paths_everything (version)
#else
import           Paths_cardano_sl (version)
#endif

type Help     = Text
type Markdown = Text

data CLIDocsOptions = CLIDocsOptions
    { pathToBinDirectory :: !FilePath
    }

optionsParser :: Parser CLIDocsOptions
optionsParser = do
    pathToBinDirectory <- strOption $
        long    "bin-dir" <>
        help    "Path to the directory with Cardano SL executable files." <>
        metavar "PATH"
    return CLIDocsOptions{..}

getCLIDocsOptions :: FilePath -> IO CLIDocsOptions
getCLIDocsOptions pathToMarkdownFile = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc "Generate Markdown chapter for cardanodocs.com."
                 <> header "Tool to generate CLI-docs for Cardano SL executable files."
                 <> footer ("Assumed that this program will run on CI. " <>
                            "Produced file '" <> pathToMarkdownFile <> "' will be " <>
                            "renamed in a chapter and pushed in cardanodocs.com repository.")

    versionOption = infoOption
        ("cardano-cli-docs-" <> showVersion version)
        (long "version" <> help "Show version.")

main :: IO ()
main = do
    myName <- getProgName
    let pathToMarkdownFile = myName <.> "md"
    CLIDocsOptions{..} <- getCLIDocsOptions pathToMarkdownFile
    makeSureDirectoryExists pathToBinDirectory
    executables <- getExecutables pathToBinDirectory
    helpInfo <- forM executables $ getHelpInfo pathToBinDirectory
    writeFile pathToMarkdownFile $ generateDocsChapter helpInfo
    putStrLn $ "Done. See file '" <> pathToMarkdownFile <> "'."
  where
    makeSureDirectoryExists binDir = unlessM (doesDirectoryExist binDir) . die $
        "Directory '" <> binDir <> "' doesn't exist." <>
        "Please make sure you defined correct path (in '.stack-work' directory)."

    getExecutables binDir = filter cardanoExeOnly <$> listDirectory binDir
      where
        cardanoExeOnly exe = "cardano-" `isPrefixOf` exe

    -- | We run executable to get its help info.
    getHelpInfo binDir exe = do
        helpOutput <- readProcess fullPath args stdIn
        return (exe, toText helpOutput)
      where
        fullPath = binDir </> exe
        args     = ["--help"]
        stdIn    = ""

generateDocsChapter :: [(FilePath, Help)] -> Markdown
generateDocsChapter helpInfo = mconcat . intersperse doubleNL $
    topSection : map generateSection helpInfo

-- | Top section for Markdown chapter.
topSection :: Markdown
topSection = [text|
---
layout: default
title:  Cardano SL CLI Options
permalink: /technical/cli-options/
group: technical
---

<!-- THIS IS AUTOGENERATED CHAPTER. DO NOT CHANGE IT MANUALLY! -->

Cardano SL CLI Options
----------------------

This guide describes all executable files that are used in Cardano SL and all corresponding CLI-options/parameters.
|]

-- | Markdown-section for one executable file.
generateSection :: (FilePath, Help) -> Markdown
generateSection (exe, helpInfoForThisExecutable) = mconcat
    [ subHeader
    , doubleNL
    , pre
    , nl
    , helpInfoForThisExecutable
    , pre
    ]
  where
    subHeader = "## " <> toText exe
    pre       = "~~~"

nl :: Text
nl = "\n"

doubleNL :: Text
doubleNL = nl <> nl
