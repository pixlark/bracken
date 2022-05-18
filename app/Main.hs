module Main where

import Options.Applicative as A
import Data.Semigroup ((<>))

import qualified CMajor

--
-- Simply a layer over the cMajor entry point which builds up compiler
-- options from the command line (using optparse-applicative)
--

argTemplate :: A.Parser CMajor.Options
argTemplate = CMajor.Options
              <$> strOption ( long "source"
                            <> short 's'
                            <> metavar "PATH"
                            <> help "Source file to be compiled" )
              <*> switch ( long "noExecutable"
                         <> short 'x'
                         <> help "Only generate C code, don't compile it" )
              <*> switch ( long "noEntry"
                         <> short 'e'
                         <> help "Don't require a specified entry point" )

main :: IO ()
main = do
  opts <- execParser (info (argTemplate <**> helper)
                       $ fullDesc
                      <> progDesc "Run the cMajor compiler"
                      <> header "cMajor - superpowered C")
  CMajor.run opts
