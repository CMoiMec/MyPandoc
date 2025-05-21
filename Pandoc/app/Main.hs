{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- Main program for Pandoc document converter
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Data.List (isPrefixOf)
import Document
import ParseFile (runParser)
import JsonParser (parseJsonValue)
import JsonPrinter (printJson, jsonToDocument, documentToJson)
import MarkdownParser (parseMarkdown)
import MarkdownPrinter (printMarkdown)
import XmlParser (parseXmlDocument, xmlToDocument, documentToXml)
import XmlPrinter (printXml)

-- | Command line options data type
data Options = Options {
    inputFile :: String,         -- Path to input file
    outputFile :: Maybe String,  -- Path to output file (optional)
    outputFormat :: String,      -- Output format (xml, json, markdown)
    inputFormat :: Maybe String  -- Input format (optional, will be detected if not provided)
} deriving (Show)

-- | Parse command line arguments into Options
parseArgs :: [String] -> Maybe Options
parseArgs args = parseLoop args Nothing Nothing Nothing Nothing

-- | Helper for parseArgs to process arguments recursively
parseLoop :: [String] -> Maybe String -> Maybe String -> Maybe String 
          -> Maybe String -> Maybe Options
parseLoop [] ifile ofile ofmt ifmt = finalizeOptions ifile ofile ofmt ifmt
parseLoop ("-i":file:rest) _ ofile ofmt ifmt =
    parseLoop rest (Just file) ofile ofmt ifmt
parseLoop ("-o":file:rest) ifile _ ofmt ifmt =
    parseLoop rest ifile (Just file) ofmt ifmt
parseLoop ("-f":fmt:rest) ifile ofile _ ifmt =
    parseLoop rest ifile ofile (Just fmt) ifmt
parseLoop ("-e":fmt:rest) ifile ofile ofmt _ =
    parseLoop rest ifile ofile ofmt (Just fmt)
parseLoop _ _ _ _ _ = Nothing

-- | Create Options from parsed arguments
finalizeOptions :: Maybe String -> Maybe String -> Maybe String 
                -> Maybe String -> Maybe Options
finalizeOptions ifile ofile ofmt ifmt =
    case (ifile, ofmt) of
        (Just i, Just f) -> Just $ Options i ofile f ifmt
        _ -> Nothing

-- | Display usage message
displayUsage :: IO ()
displayUsage = getProgName >>= printUsage

-- | Print usage information with program name
printUsage :: String -> IO ()
printUsage prgName = 
    let usageMsg = "USAGE: " ++ prgName
    in putStrLn (usageMsg ++ " -i ifile -f oformat [-o ofile] [-e iformat]") >>
       putStrLn "  ifile       path to the file to convert" >>
       putStrLn "  oformat     output format (xml, json, markdown)" >>
       putStrLn "  ofile       path to the output file" >>
       putStrLn "  iformat     input format (xml, json, markdown)"

-- | Detect input format from file content
detectFormat :: String -> Maybe String
detectFormat content
    | "<document>" `isPrefixOf` (dropWhitespace content) = Just "xml"
    | "{" `isPrefixOf` (dropWhitespace content) = Just "json"
    | "---" `isPrefixOf` (dropWhitespace content) = Just "markdown"
    | otherwise = Nothing
  where
    isSpace :: Char -> Bool
    isSpace c = c `elem` " \t\n\r"
    dropWhitespace :: String -> String
    dropWhitespace = dropWhile isSpace

-- | Parse document based on format
parseDocument :: String -> String -> Maybe Document
parseDocument "json" content =
    case runParser parseJsonValue content of
        Just (jsonVal, _) -> jsonToDocument jsonVal
        Nothing -> Nothing
parseDocument "markdown" content =
    parseMarkdown content
parseDocument "xml" content =
    case runParser parseXmlDocument content of
        Just (xmlNode, _) -> xmlToDocument xmlNode
        Nothing -> Nothing
parseDocument _ _ = Nothing

-- | Convert document to specified output format
formatDocument :: String -> Document -> String
formatDocument "json" doc = printJson (documentToJson doc)
formatDocument "markdown" doc = printMarkdown doc
formatDocument "xml" doc = printXml (documentToXml doc)
formatDocument _ _ = ""

-- | Process a document from input to output
processDocument :: Options -> IO ()
processDocument opts = do
    content <- readFile (inputFile opts)
    processContent opts content

-- | Process content based on detected format
processContent :: Options -> String -> IO ()
processContent opt content =
    case determineFormat opt content of
        Nothing -> exitWithError "Could not detect input format"
        Just fmt ->
            case parseDocument fmt content of
                Nothing -> exitWithError "Failed to parse document"
                Just doc->writeOutput opt(formatDocument(outputFormat opt)doc)

-- | Determine input format from options or content
determineFormat :: Options -> String -> Maybe String
determineFormat opts content = case inputFormat opts of
                               Just fmt -> Just fmt
                               Nothing -> detectFormat content

-- | Write output to file or stdout
writeOutput :: Options -> String -> IO ()
writeOutput opts output = case outputFile opts of
    Just file -> writeFile file output
    Nothing -> putStr output

-- | Exit with error message
exitWithError :: String -> IO a
exitWithError msg = do
    _ <- hPutStrLn stderr msg
    exitWith (ExitFailure 84)

-- | Main function
main :: IO ()
main = getArgs >>= handleArgs

-- | Handle command line arguments
handleArgs :: [String] -> IO ()
handleArgs args = case parseArgs args of
    Nothing -> reportInvalidArgs
    Just opts -> processDocument opts

-- | Report invalid arguments and exit
reportInvalidArgs :: IO ()
reportInvalidArgs = hPutStrLn stderr "Invalid arguments" >>
                   displayUsage >>
                   exitWith (ExitFailure 84)