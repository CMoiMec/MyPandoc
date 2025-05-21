{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- MarkdownParser
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MarkdownPrinter where
import Document


printMarkdown :: Document -> String
printMarkdown (Document hdr contents) =
  "---\n" ++
  "title: " ++ title hdr ++ "\n" ++
  maybe "" (\a -> "author: " ++ a ++ "\n") (author hdr) ++
  maybe "" (\d -> "date: " ++ d ++ "\n") (date hdr) ++
  "---\n\n" ++
  concatMap (printContent 0) contents

printContent :: Int -> Content -> String
printContent _ (Paragraph elements) =
  concatMap printContentElement elements ++ "\n\n"

printContent indent (Section (Just ttl) sub) =
  replicate (headerLevel indent) '#' ++ " " ++ ttl ++ "\n\n" ++
  concatMap (printContent (indent + 1)) sub

printContent indent (Section Nothing sub) =
  concatMap (printContent (indent + 1)) sub

printContent _ (CodeBlock allLines) =
  "```\n" ++ unlines allLines ++ "```\n\n"

printContent _ (ListContent items) =
  unlines
    [ "- " ++ concatMap printContentElement item
    | item <- items
    ] ++ "\n"

headerLevel :: Int -> Int
headerLevel depth = min 6 (depth + 1)

printContentElement :: ContentElement -> String
printContentElement (TextElement text) = text
printContentElement (BoldElement text) = "**" ++ text ++ "**"
printContentElement (ItalicElement text) = "*" ++ text ++ "*"
printContentElement (CodeElement text) = "`" ++ text ++ "`"
printContentElement (LinkElement url content) =
  "[" ++ concatMap printContentElement content ++ "](" ++ url ++ ")"
printContentElement (ImageElement url alt) =
  "![" ++ concatMap printContentElement alt ++ "](" ++ url ++ ")"
