#!/usr/bin/env runhaskell
-- script to generate a very repetitive makefile. Ignores "header.tex".
import Data.List (delete)
import System.FilePath.Posix (dropExtension, takeExtension)
import System.Directory (getCurrentDirectory, getDirectoryContents)

suffixToPdf :: FilePath -> FilePath
suffixToPdf s = dropExtension s ++ ".pdf"

dropHeader :: [FilePath] -> [FilePath]
dropHeader = delete "header.tex"

getAll :: [FilePath] -> String
getAll = (++) "all: " . unwords . map suffixToPdf . dropHeader

getRule :: FilePath -> [String]
getRule texFile = [suffixToPdf texFile ++ ": " ++ texFile ++ " header.tex",
                   "\t@pdflatex -halt-on-error -file-line-error " ++ texFile
                   ++ " | python filter.py"]

getMakefile :: [FilePath] -> String
getMakefile texFiles = unlines $ getAll texFiles : "" : concat
                       (map (\ s -> getRule s ++ [""]) $ dropHeader texFiles)

main :: IO ()
main = fmap getMakefile texFiles' >>= putStrLn
  where texFiles' = fmap (filter (\ s -> takeExtension s == ".tex")) $
                    getCurrentDirectory >>= getDirectoryContents
