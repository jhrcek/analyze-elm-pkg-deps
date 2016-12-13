module Main where

import System.Process (readProcess)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import Control.Monad (when, void, forM_)
import System.FilePath ((</>))
main :: IO ()
main = do
  downloadLatestPackageList
  allPkgs <- readPkgsInfo <$> readFile newPackages
  let pkgsToDownload = filter (`notElem` brokenPackages) allPkgs
  createDirectoryIfMissing True "all-packages"
  setCurrentDirectory "all-packages"
  forM_ pkgsToDownload makePkg

brokenPackages = ["Fresheyeball/elm-nearly-eq", "lattenwald/elm-base64", "omarroth/elm-dom", "omarroth/elm-parts", "swiftsnamesake/euclidean-space"]

makePkg :: String -> IO ()
makePkg pkg = do
  putStrLn $ "Downloading package " ++ pkg
  initialDir <- getCurrentDirectory
  createDirectoryIfMissing True pkg
  setCurrentDirectory $ initialDir </> pkg
  readProcess "elm" ["package", "install", pkg, "--yes"] "" >>= putStrLn
  setCurrentDirectory initialDir

readPkgsInfo :: String -> [String]
readPkgsInfo = read

downloadLatestPackageList :: IO ()
downloadLatestPackageList = do
   exists <- doesFileExist newPackages
   when exists $ removeFile newPackages
   void $ readProcess "wget" ["http://package.elm-lang.org/" ++ newPackages] ""

newPackages :: FilePath
newPackages = "new-packages"
