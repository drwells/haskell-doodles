#!/usr/bin/env runhaskell
-- Script to parse the output from pdflatex and only print error messages.
import Data.Maybe (mapMaybe)
import System.Exit (exitFailure, exitSuccess)
import Text.Regex.Posix ((=~))

-- regular expression matching an error message from pdflatex. Unfortunately,
-- pdflatex only uses stdout, so it is necessary to filter for this.
hasError :: String -> Maybe String
hasError s
  | s =~ ".*:[0-9]*:.*" = Just s
  | otherwise = Nothing

getCode :: Int -> IO a
getCode 0 = exitSuccess
getCode _ = exitFailure

main :: IO ()
main = getContents >>= \ contents ->
  let errorList = mapMaybe hasError $ lines contents
  in putStr (unlines errorList) >> getCode (length errorList)
