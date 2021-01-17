module Test.MySolutions where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parTraverse)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence_, traverse_)
import Effect.Aff
  (Aff, Error, Milliseconds(..), attempt, delay, launchAff_, parallel, sequential)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, readTextFile, writeTextFile)
import Node.Path (FilePath, dirname)
import Node.Path (concat) as P

-- Note to reader: Add your solutions to this file
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 outFile = do
  data1 <- readTextFile UTF8 file1
  writeTextFile UTF8 outFile data1
  data2 <- readTextFile UTF8 file2
  appendTextFile UTF8 outFile data2

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany inFs outFile =
  traverse_
    ( \inf -> do
        txt <- readTextFile UTF8 inf
        appendTextFile UTF8 outFile txt
    )
    inFs

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = map (map length) $ attempt (readTextFile UTF8 file)

writeGet :: String -> FilePath -> Aff Unit
writeGet url outFile = do
  result <- AX.get ResponseFormat.string url
  case result of
    Left err -> pure unit
    Right response -> writeTextFile UTF8 outFile response.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel inFiles outFile = do
  infilesData <- parTraverse (readTextFile UTF8) inFiles
  traverse_ (appendTextFile UTF8 outFile) infilesData

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout timeout url =
  sequential
    $ oneOf
        [ parallel
            ( do
                result <- AX.get ResponseFormat.string url
                case result of
                  Left err -> pure Nothing
                  Right response -> pure $ Just response.body
            )
        , parallel (Nothing <$ delay (Milliseconds timeout))
        ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles root =
  let
    curPath = dirname root
  in
    do
      nextFiles <- readTextFile UTF8 root
      if nextFiles == "" then
        pure [ root ]
      else
        let
          nFs = map (\fn -> P.concat [ curPath, fn ]) (split (Pattern "\n") nextFiles)
        in
          (((<>) [ root ]) <<< concat) <$> parTraverse recurseFiles nFs
