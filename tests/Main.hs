{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable
import Data.Traversable
import Control.Monad

import System.Directory
import System.FilePath
import System.Exit

import Test.Tasty
import Test.Tasty.HUnit

import Shower

main :: IO ()
main = do
  inOutTests <- mkInOutTests
  defaultMain $
    testGroup "Shower" [inOutTests]

mkInOutTests :: IO TestTree
mkInOutTests = do
  currentDir <- getCurrentDirectory
  let inOutDir = currentDir </> "tests/in-out/"
  inOutFilePaths <- listDirectory inOutDir
  case zipInOutFilePaths (map (inOutDir </>) inOutFilePaths) of
    Left ZipInOutFail{..} -> do
      let
        reportPathSet xs msg =
          unless (Set.null xs) $ do
            putStrLn msg
            traverse_ (putStrLn . ("  "++)) xs
      reportPathSet ziofBadExtension "Bad extension, expected .in or .out:"
      reportPathSet ziofInWithoutOut "In-file has no accompanying out-file:"
      reportPathSet ziofOutWithoutIn "Out-file has no accompanying in-file:"
      exitFailure
    Right zippedPaths -> do
      testCases <- for (Map.toList zippedPaths) $
        \(testName, InOut inFilePath outFilePath) -> do
          inFile <- readFile inFilePath
          outFile <- readFile outFilePath
          return $ testCase testName $
            case showerString inFile of
              Left parseError -> assertFailure parseError
              Right s -> assertEqual "" (normalize s) (normalize outFile)
      return (testGroup "in/out" testCases)

normalize :: String -> String
normalize = unlines . lines

data ZipInOutFail =
  ZipInOutFail
    { ziofBadExtension :: Set FilePath,
      ziofInWithoutOut :: Set FilePath,
      ziofOutWithoutIn :: Set FilePath }

data InOut a = InOut a a

zipInOutFilePaths :: [FilePath] -> Either ZipInOutFail (Map TestName (InOut FilePath))
zipInOutFilePaths filePaths =
  let
    (badExt, inExt, outExt) = go ([], [], []) filePaths
    inWithoutOut = (Set.fromList . Map.elems) (inExt Map.\\ outExt)
    outWithoutIn = (Set.fromList . Map.elems) (outExt Map.\\ inExt)
    zippedPaths = Map.intersectionWith InOut inExt outExt
  in
    if
      Set.null badExt &&
      Set.null inWithoutOut &&
      Set.null outWithoutIn
    then
      Right zippedPaths
    else
      Left ZipInOutFail
        { ziofBadExtension = badExt,
          ziofInWithoutOut = inWithoutOut,
          ziofOutWithoutIn = outWithoutIn }
  where
    go (accBadExt, accInExt, accOutExt) [] =
      ( Set.fromList accBadExt,
        Map.fromList accInExt,
        Map.fromList accOutExt )
    go (accBadExt, accInExt, accOutExt) (p:ps) =
      let
        fileName = takeFileName p
        (name, ext) = splitExtensions fileName
      in
        case ext of
          ".in"  -> go (accBadExt, (name, p) : accInExt, accOutExt) ps
          ".out" -> go (accBadExt, accInExt, (name, p) : accOutExt) ps
          _ -> go (p:accBadExt, accInExt, accOutExt) ps

