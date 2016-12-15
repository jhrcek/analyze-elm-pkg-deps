{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, void, when)
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         getCurrentDirectory, removeFile, setCurrentDirectory)
import System.FilePath (splitDirectories, (</>))
import System.Process (readCreateProcess, readProcess, shell)

main :: IO ()
main = do
    downloadLatestPackageList
    allPkgs <- readPkgsInfo <$> readFile newPackages
    let pkgsToDownload = filter (`notElem` brokenPackages) allPkgs
    createDirectoryIfMissing True "all-packages"
    setCurrentDirectory "all-packages"
    --mapM_ pkgsToDownload makePkg
    xs <- mapM findElmPkgFiles pkgsToDownload
    mapM_ print xs
    --forM_ pkgsToDownload buildDepGraph


findElmPkgFiles :: FilePath -> IO [PkgDeps]
findElmPkgFiles authorAndPkg = do
    putStrLn $ "elm-package.json files in " ++ authorAndPkg
    elmPkgJsons <- lines <$> readCreateProcess (shell $ "find " ++ authorAndPkg ++ " -mindepth 6 -maxdepth 6 -name elm-package.json") ""
    mapM dependenciesOf elmPkgJsons
                      -- analyzed package - its dependency -- deps of the dependency
data PkgDeps = PkgDeps String String [String] deriving Show
data Pkg = Pkg String String

instance FromJSON PkgDeps where
    parseJSON (Object v) =
        PkgDeps "" "" <$>
        (v .: "dependencies" >>= parseDepKeys)

    parseJSON invalid    = typeMismatch "PkgDeps" invalid

parseDepKeys :: Value -> Parser [String]
parseDepKeys = withObject "dependencies" (return . map T.unpack . HM.keys)

dependenciesOf :: FilePath -> IO PkgDeps
dependenciesOf pathToElmPkgJson = do
    -- ingara/elm-asoiaf-api/elm-stuff/packages/elm-lang/virtual-dom/2.0.2/elm-package.json
    let (author:package:_elm_stuff:_packages:depAuthor:depProject:_) = splitDirectories pathToElmPkgJson
    json <- B.readFile pathToElmPkgJson
    let decodeResult = eitherDecode json :: Either String PkgDeps
    case decodeResult of
        Left err -> error err
        Right (PkgDeps _ _ deps) -> return (PkgDeps (author </> package) (depAuthor </> depProject) deps)


buildDepGraph :: FilePath -> IO String
buildDepGraph authorAndPkg = return "TODO"

downloadLatestPackageList :: IO ()
downloadLatestPackageList = do
     exists <- doesFileExist newPackages
     when exists $ removeFile newPackages
     void $ readProcess "wget" ["http://package.elm-lang.org/" ++ newPackages] ""

newPackages :: FilePath
newPackages = "new-packages"

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
