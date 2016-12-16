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
import Data.List (intercalate)
main :: IO ()
main = do
    downloadLatestPackageList
    allPkgs <- loadLatestPkgList
    let pkgsToDownload = filter (`notElem` brokenPackages) allPkgs
    createDirectoryIfMissing True "all-packages"
    setCurrentDirectory "all-packages"
    mapM_ elmInstall pkgsToDownload
    depTrees <- mapM getDependencyTree pkgsToDownload
    setCurrentDirectory "../depTrees"
    mapM_ createDependenciesImage depTrees

loadLatestPkgList :: IO [Pkg]
loadLatestPkgList = parsePkgInfo <$> readFile newPackages
  where
    parsePkgInfo  :: String -> [Pkg]
    parsePkgInfo = map strToPkg . read

getDependencyTree :: Pkg -> IO PkgDependencies
getDependencyTree p@(Pkg author pkg) = do
    putStrLn $ "elm-package.json files in " ++ show p
    elmPkgJsons <- lines <$> readCreateProcess (shell $ "find " ++ (author </> pkg) ++ " -mindepth 6 -maxdepth 6 -name elm-package.json") ""
    consolidate <$> mapM analyzeElmPkgJson elmPkgJsons


-- analyzed package - its dependency -- deps of the dependency
data ElmPkgJson = ElmPkgJson Pkg Pkg [Pkg] deriving Show

data Pkg = Pkg String String deriving (Show, Eq)

dummyPkg = Pkg "" ""

data PkgDependencies = PkgDependencies Pkg [(Pkg, [Pkg])]

consolidate :: [ElmPkgJson] -> PkgDependencies
consolidate dps@((ElmPkgJson p _ _):_) = PkgDependencies p (map (\(ElmPkgJson _ a bs) -> (a,bs)) dps)
consolidate [] = error "unexpected empty pkg deps list"

instance FromJSON ElmPkgJson where
    parseJSON (Object v) =
        ElmPkgJson dummyPkg dummyPkg <$>
        (v .: "dependencies" >>= parseDepKeys)

    parseJSON invalid    = typeMismatch "ElmPkgJson" invalid

parseDepKeys :: Value -> Parser [Pkg]
parseDepKeys = withObject "dependencies" (return . map (strToPkg . T.unpack) . HM.keys)

analyzeElmPkgJson :: FilePath -> IO ElmPkgJson
analyzeElmPkgJson pathToElmPkgJson = do
    -- ingara/elm-asoiaf-api/elm-stuff/packages/elm-lang/virtual-dom/2.0.2/elm-package.json
    let (author:package:_elm_stuff:_packages:depAuthor:depProject:_) = splitDirectories pathToElmPkgJson
    json <- B.readFile pathToElmPkgJson
    let decodeResult = eitherDecode json :: Either String ElmPkgJson
    case decodeResult of
        Left err -> error err
        Right (ElmPkgJson _ _ deps) -> return (ElmPkgJson (Pkg author package) (Pkg depAuthor depProject) deps)

type Dot = String

writeDot :: PkgDependencies -> IO FilePath
writeDot pds@(PkgDependencies (Pkg author pkg) _) = do
  let outFile = author ++ "_" ++ pkg ++ ".dot"
  writeFile outFile (toDot pds)
  return outFile

createDependenciesImage :: PkgDependencies -> IO ()
createDependenciesImage pds = do
    dotFile <- writeDot pds
    void $ readProcess "dot" [dotFile, "-Tpng", "-O"] ""

toDot :: PkgDependencies -> Dot
toDot (PkgDependencies (Pkg author pkg) deps) =
  "strict digraph \"" ++ author ++ "_" ++ pkg ++ "\" {\n" ++
    unlines (map toDot' deps)
  ++ "}"

toDot' :: (Pkg, [Pkg]) -> String
toDot' (p, deps) = if null deps then "" else toDot'' p ++ "->{\n" ++ (intercalate "," $ map toDot'' deps) ++"\n}"

toDot'' :: Pkg -> String
toDot'' (Pkg a p)   = "\""++a++"/"++p++"\""

downloadLatestPackageList :: IO ()
downloadLatestPackageList = do
     exists <- doesFileExist newPackages
     when exists $ removeFile newPackages
     void $ readProcess "wget" ["http://package.elm-lang.org/" ++ newPackages] ""

newPackages :: FilePath
newPackages = "new-packages"

brokenPackages =
    [ Pkg "Fresheyeball" "elm-nearly-eq"
    , Pkg "lattenwald" "elm-base64"
    , Pkg "omarroth" "elm-dom"
    , Pkg "omarroth" "elm-parts"
    , Pkg "swiftsnamesake" "euclidean-space"
    ]

elmInstall :: Pkg -> IO ()
elmInstall p@(Pkg author pkg) = do
    let pkgDir = pkgToDir p
    putStrLn $ "Downloading package " ++ show p
    initialDir <- getCurrentDirectory
    createDirectoryIfMissing True pkgDir
    setCurrentDirectory $ initialDir </> pkgDir
    readProcess "elm" ["package", "install", pkgDir, "--yes"] "" >>= putStrLn
    setCurrentDirectory initialDir

strToPkg :: String -> Pkg
strToPkg s =
    let (author, '/':package) = break (=='/') s
    in Pkg author package

pkgToDir :: Pkg -> FilePath
pkgToDir (Pkg author pkg) = author </> pkg
