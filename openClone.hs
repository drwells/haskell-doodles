#!/usr/bin/env runhaskell
-- Clone of the NeXT/OS X command line utility open
import System.Environment (getArgs)
import System.Process (runCommand)
import System.Posix.Env (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map (Map, lookup, fromList)

data FlagOption = Default | Application | Editor | Help deriving (Eq, Show)

-- work with all the input values (except flags) and return a list of strings
-- to pass to /bin/sh
attachCommands :: Either String (FlagOption, [String]) -> [String]
attachCommands input = case input of
  Right (flagValue, inputArgs) -> attachApplications (flagValue, inputArgs)
-- if given an error message, return it to stderr.
  Left errorMessage -> error errorMessage

attachApplications :: (FlagOption, [String]) -> [String]
attachApplications (flagValue, inputArgs) = case flagValue of
-- in the default case, look up the application associated with the first file.
  Default     -> case Map.lookup (suffix (head inputArgs)) applicationTable of
    Just app  -> prependApplication app inputArgs
    Nothing   -> error "application not defined"
-- if the flag is for an application, that application is the first argument.
  Application -> prependApplication (head inputArgs) (tail inputArgs)
-- if the flag is for the editor, launch it with the editor variable.
  Editor      -> [editorName ++ " " ++ (unwords inputArgs)]
-- if the flag is for help, echo the help message.
  Help        -> ["echo " ++ "'" ++ usageMessage ++ "'"]
-- function to prefix files with the appropriate application.
  where prependApplication app files
          | length files == 0 = [app]
          | otherwise = map ((app ++ " ") ++) (map addBackslash files)
        suffix = reverse . takeWhile (/='.') . reverse

addBackslash :: String -> String
addBackslash = foldl1 (++) . map test
                 where test = \ t -> if elem t " ()[]" then "\\" ++ [t] else [t]

-- Given some list of command line arguments, return either an error or a tuple
-- of the correct action and arguments.
findFlag :: [String] -> Either String (FlagOption, [String])
findFlag commandLineArgs
  -- if the first arguement is a flag, parse it appropriately.
  | head firstArg == '-' = case (head commandLineArgs) of
    "-a" -> Right (Application, tail commandLineArgs)
    "-e" -> Right (Editor, tail commandLineArgs)
    "-H" -> Right (Help, tail commandLineArgs)
  | head firstArg == '-' = Left "undefined flag"
  | otherwise = Right (Default, commandLineArgs)
    where firstArg = head commandLineArgs

-- This needs to be replaced by a proper parser with a dot file.
applicationTable :: Map.Map String String
applicationTable = Map.fromList [("pdf", "mupdf"), ("jpeg", "feh"),
                                 ("org", "emacs"),
                                 ("odt", "libreoffice"), ("ods", "libreoffice"),
                                 ("avi", "vlc"), ("flac", "vlc"), ("mp4", "vlc")]

-- Grab the editor name.
editorName :: String
editorName = case (unsafePerformIO $ (getEnv "EDITOR")) of
  Nothing  -> error "EDITOR variable not set"
  Just app -> app

-- print message for usage
usageMessage = unlines $ ["Usage: open [-e] [-H] [-a <application>]",
                          "Help: Open opens files from a shell."]

-- process a list of commands to run by mapping runCommand (creates a process)
-- over the list.
main = getArgs >>= mapM_ runCommand . attachCommands . findFlag
