{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- MarkdownParser
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module MarkdownParser where

import Document (Document(Document), Content(Paragraph), Header(..), ContentElement(TextElement))
import Data.List (isPrefixOf)

parseMarkdown :: String -> Maybe Document
parseMarkdown input =
  case lines input of
    (l1:rest) | isYamlStart l1 -> parseYaml rest
    _ -> Nothing

isYamlStart :: String -> Bool
isYamlStart l = "---" `elem` words l

parseYaml :: [String] -> Maybe Document
parseYaml ls =
  let (metaLines, contentLines) = break (== "---") ls
      meta = foldl parseMeta (Header "" Nothing Nothing) metaLines
      bodyContent = parseBodyContent (dropWhile null (drop 1 contentLines))
  in Just $ Document meta bodyContent

parseBodyContent :: [String] -> [Content]
parseBodyContent [] = []
parseBodyContent allLines =
  -- For simplicity, just convert each line to a paragraph with a text element
  map (\line -> Paragraph [TextElement line]) (filter (not . null) allLines)

parseMeta :: Header -> String -> Header
parseMeta h line
  | "title:" `isPrefixOf` line = h { title = dropPrefix "title: " line }
  | "author:" `isPrefixOf` line = h { author = Just (dropPrefix"author: "line)}
  | "date:" `isPrefixOf` line = h { date = Just (dropPrefix "date: " line) }
  | otherwise = h

dropPrefix :: String -> String -> String
dropPrefix pre s = drop (length pre) s