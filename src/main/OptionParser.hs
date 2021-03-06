module OptionParser
  ( options
  , CmdOptions(..)
  ) where

import           Options.Applicative

import           OwO.Options

data CmdOptions = CmdOptions
  { compilerInputFile           :: Maybe FilePath
  , compilerIncludePaths        :: [FilePath]
  , compilerDumpToken           :: Bool
  , compilerDumpPsi             :: Bool
  , compilerDumpAst             :: Bool
  , compilerDumpHideLoc         :: Bool
  , showVersion                 :: Bool
  , showHelp                    :: Bool
  , pragmaSafe                  :: Bool
  , pragmaNoPositivityCheck     :: Bool
  , pragmaNoTerminationCheck    :: Bool
  , pragmaNoExhaustivenessCheck :: Bool
  }

options :: IO CmdOptions
options = customExecParser pref information
  where
    information = info (helper <*> opts)
      $  fullDesc
      <> header "The Compiler for the OwO Programming Language."
      <> footer "Website: https://www.owo-lang.org/"
    pref = prefs $  showHelpOnError
                 <> showHelpOnEmpty
                 <> disambiguate
                 <> columns 80
    opts = CmdOptions
      <$> optional
        (strOption $  long "src"
                   <> help "Source file path"
                   <> metavar "PATH"
                   <> short 'c'
                   )
      <*> many
        (strOption $  long "include-dir"
                   <> help "Include paths"
                   <> metavar "DIR"
                   <> short 'I'
                   )
      <*> switch
          (  long "dump-tokens"
          <> help "Scan the file and print tokens"
          )
      <*> switch
          (  long "dump-psi"
          <> help "Parse the file and print the concrete syntax tree"
          )
      <*> switch
          (  long "dump-ast"
          <> help "Parse the file and print the abstract syntax tree"
          )
      <*> switch
          (  long "dump-hide-location"
          <> help "Hide location when dumping Tokens/AST"
          )
      <*> switch
          (  long "version"
          <> help "Show OwO compiler version"
          <> short 'V'
          )
      <*> switch
          (  long "help"
          <> help "Print this message"
          <> short 'h'
          )
      <*> switch
          (  long "safe"
          <> help "Enable safe mode"
          )
      <*> switch
          (  long "no-positivity-check"
          <> help "Disable positivity checks"
          )
      <*> switch
          (  long "no-termination-check"
          <> help "Disable termination checks"
          )
      <*> switch
          (  long "no-exhaustiveness-check"
          <> help "Disable exhaustiveness check"
          )
