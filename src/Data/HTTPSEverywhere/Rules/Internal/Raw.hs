module Data.HTTPSEverywhere.Rules.Internal.Raw (
  getRule,
  getRules
) where

import Prelude hiding (readFile)
import Data.Functor.Infix ((<$$>))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (combine, takeExtension)

import Paths_https_everywhere_rules (getDataFileName)

getRule :: FilePath -> IO Text
getRule = readFile

getRules :: IO [FilePath]
getRules = getDataFileName [] >>= filter isXML <$$> getAbsoluteDirectoryContents
  where isXML = (== ".xml") . takeExtension

getAbsoluteDirectoryContents :: FilePath -> IO [FilePath]
getAbsoluteDirectoryContents x = combine x <$$> getDirectoryContents x
