{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- Document
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module Document where

data Header = Header {
  title :: String,
  author :: Maybe String,
  date :: Maybe String
} deriving (Show, Eq)

data ContentElement = 
    TextElement String
  | BoldElement String
  | ItalicElement String
  | CodeElement String
  | LinkElement String [ContentElement]  -- url and content
  | ImageElement String [ContentElement]  -- url and alt text
  deriving (Show, Eq)

data Content =
    Paragraph [ContentElement]
  | Section (Maybe String) [Content]
  | CodeBlock [String]
  | ListContent [[ContentElement]]
  deriving (Show, Eq)

data Document = Document {
  header :: Header,
  body :: [Content]
} deriving (Show, Eq)