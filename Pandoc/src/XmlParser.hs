{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- XmlParser
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module XmlParser where

import Document
import ParseFile (Parser, parseChar, parseAnyChar, parseMany, parseSome)
import Control.Applicative (Alternative(..), (<|>))

data XmlNode = XmlElement String [(String, String)] [XmlNode]
             | XmlText String
             deriving (Show, Eq)

-- Basic parsers
parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar " \t\n\r")

parseToken :: Parser a -> Parser a
parseToken p = p <* parseWhitespace

parseName :: Parser String
parseName =
  parseToken $ parseSome(parseAnyChar (['a'..'z'] ++
  ['A'..'Z'] ++ ['0'..'9'] ++ "_-:"))

parseAttributeValue :: Parser String
parseAttributeValue = parseToken (
  parseChar '"' *> parseMany (
    parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++
    ['0'..'9'] ++ " _-.:,;!?()[]{}<>=+*/\\@#$%^&~`'") <|>
    (parseChar '\\' *> parseChar '"' >> return '"')
  ) <* parseChar '"'
  )

parseAttribute :: Parser (String, String)
parseAttribute = do
  name <- parseName
  _ <- parseToken (parseChar '=')
  value <- parseAttributeValue
  return (name, value)

parseAttributes :: Parser [(String, String)]
parseAttributes = parseMany (parseToken parseAttribute)

parseTextContent :: Parser XmlNode
parseTextContent = XmlText . trim <$> parseSome parseTextChar
  where
    parseTextChar =
      parseAnyChar (filter (/= '<') [' '..'~']) <|> parseAnyChar "\n\r\t"
    trim = unwords . words  -- Remove extra whitespace

parseOpenTag :: Parser (String, [(String, String)])
parseOpenTag = do
  _ <- parseToken (parseChar '<')
  name <- parseName
  attrs <- parseAttributes
  _ <- parseToken (parseChar '>')
  return (name, attrs)

parseCloseTag :: Parser String
parseCloseTag = do
  _ <- parseToken (parseChar '<')
  _ <- parseToken (parseChar '/')
  name <- parseName
  _ <- parseToken (parseChar '>')
  return name

parseSelfClosingTag :: Parser (String, [(String, String)])
parseSelfClosingTag = do
  _ <- parseToken (parseChar '<')
  name <- parseName
  attrs <- parseAttributes
  _ <- parseToken (parseChar '/')
  _ <- parseToken (parseChar '>')
  return (name, attrs)

parseElement :: Parser XmlNode
parseElement = do
  (name, attrs) <- parseOpenTag
  content <- parseMany parseXmlNode
  closeName <- parseCloseTag
  if name == closeName
    then return $ XmlElement name attrs content
    else empty

parseSelfClosingElement :: Parser XmlNode
parseSelfClosingElement = do
  (name, attrs) <- parseSelfClosingTag
  return $ XmlElement name attrs []

parseXmlNode :: Parser XmlNode
parseXmlNode = parseWhitespace *>
  (parseElement <|> parseSelfClosingElement <|> parseTextContent)

parseXmlDocument :: Parser XmlNode
parseXmlDocument = parseWhitespace *> parseXmlNode <* parseWhitespace

-- Convert XmlNode to Document
xmlToDocument :: XmlNode -> Maybe Document
xmlToDocument (XmlElement "document" _ children) = do
  headerNode <- findChild "header" children
  bodyNode <- findChild "body" children
  hdr <- xmlToHeader headerNode
  bdy <- xmlToBody bodyNode
  return $ Document hdr bdy
xmlToDocument _ = Nothing

xmlToHeader :: XmlNode -> Maybe Header
xmlToHeader (XmlElement "header" attrs children) = do
  ttl <- lookup "title" attrs
  let auth = getChildText "author" children <|> lookup "author" attrs
      dte = getChildText "date" children <|> lookup "date" attrs
  return $ Header ttl auth dte
xmlToHeader _ = Nothing

xmlToBody :: XmlNode -> Maybe [Content]
xmlToBody (XmlElement "body" _ children) = 
  mapM xmlToContent children
xmlToBody _ = Nothing

xmlToContent :: XmlNode -> Maybe Content
xmlToContent (XmlElement "paragraph" _ children) = do
  elements <- mapM xmlToContentElement children
  return $ Paragraph elements
xmlToContent (XmlElement "section" attrs children) = do
  let ttl = case lookup "title" attrs of
               Just "" -> Nothing
               t -> t
  contents <- mapM xmlToContent children
  return $ Section ttl contents
xmlToContent (XmlElement "codeblock" _ children) = do
  allLines <- mapM getCodeBlockLine children
  return $ CodeBlock allLines
xmlToContent (XmlElement "list" _ children) = do
  items <- mapM xmlToListItem children
  return $ ListContent items
xmlToContent _ = Nothing

xmlToListItem :: XmlNode -> Maybe [ContentElement]
xmlToListItem (XmlElement "item" _ children) = 
  mapM xmlToContentElement children
xmlToListItem (XmlElement "paragraph" _ children) = 
  mapM xmlToContentElement children
xmlToListItem (XmlText text) = Just [TextElement text]
xmlToListItem node = xmlToContentElement node >>= \curElem -> Just [curElem]

xmlToContentElement :: XmlNode -> Maybe ContentElement
xmlToContentElement (XmlText text) = Just $ TextElement text
xmlToContentElement (XmlElement "bold" _ children) = 
  case children of
    [XmlText text] -> Just $ BoldElement text
    _ -> Just $ BoldElement (concatMap getTextFromChildren children)
xmlToContentElement (XmlElement "italic" _ children) = 
  case children of
    [XmlText text] -> Just $ ItalicElement text
    _ -> Just $ ItalicElement (concatMap getTextFromChildren children)
xmlToContentElement (XmlElement "code" _ children) = 
  case children of
    [XmlText text] -> Just $ CodeElement text
    _ -> Just $ CodeElement (concatMap getTextFromChildren children)
xmlToContentElement (XmlElement "link" attrs children) = do
  url <- lookup "url" attrs
  content <- mapM xmlToContentElement children
  return $ LinkElement url content
xmlToContentElement (XmlElement "image" attrs children) = do
  url <- lookup "url" attrs
  alt <- mapM xmlToContentElement children
  return $ ImageElement url alt
xmlToContentElement _ = Nothing

-- Helper functions
findChild :: String -> [XmlNode] -> Maybe XmlNode
findChild _ [] = Nothing
findChild name (n@(XmlElement eName _ _):rest)
  | name == eName = Just n
  | otherwise = findChild name rest
findChild name (_:rest) = findChild name rest

getChildText :: String -> [XmlNode] -> Maybe String
getChildText name children = do
  child <- findChild name children
  case child of
    XmlElement _ _ [XmlText text] -> Just text
    _ -> Nothing

getTextFromNode :: XmlNode -> String
getTextFromNode (XmlText text) = text
getTextFromNode _ = ""

getCodeBlockLine :: XmlNode -> Maybe String
getCodeBlockLine (XmlElement "paragraph" _ children) = 
  Just $ concatMap getTextFromChildren children
getCodeBlockLine (XmlText text) = Just text
getCodeBlockLine _ = Just ""

getTextFromChildren :: XmlNode -> String
getTextFromChildren (XmlText text) = text
getTextFromChildren (XmlElement _ _ children) =
  concatMap getTextFromChildren children

-- Convert Document to XmlNode
documentToXml :: Document -> XmlNode
documentToXml (Document hdr bdy) = 
  XmlElement "document" [] [
    headerToXml hdr,
    XmlElement "body" [] (map contentToXml bdy)
  ]

headerToXml :: Header -> XmlNode
headerToXml (Header t a d) = 
  XmlElement "header" [("title", t)] $
    maybe [] (\auth -> [XmlElement "author" [] [XmlText auth]]) a ++
    maybe [] (\dte -> [XmlElement "date" [] [XmlText dte]]) d

contentToXml :: Content -> XmlNode
contentToXml (Paragraph elements) = 
  XmlElement "paragraph" [] (map contentElementToXml elements)
contentToXml (Section ttl contents) = 
  XmlElement "section" (maybe [] (\t -> [("title", t)]) ttl) 
    (map contentToXml contents)
contentToXml (CodeBlock allLines) = 
  XmlElement "codeblock" [] (map XmlText allLines)
contentToXml (ListContent items) = 
  XmlElement "list" [] (map listItemToXml items)

listItemToXml :: [ContentElement] -> XmlNode
listItemToXml elements = 
  XmlElement "paragraph" [] (map contentElementToXml elements)

contentElementToXml :: ContentElement -> XmlNode
contentElementToXml (TextElement text) = XmlText text
contentElementToXml (BoldElement text) = XmlElement "bold" [] [XmlText text]
contentElementToXml (ItalicElement text) = XmlElement "italic" [][XmlText text]
contentElementToXml (CodeElement text) = XmlElement "code" [] [XmlText text]
contentElementToXml (LinkElement url content) = 
  XmlElement "link" [("url", url)] (map contentElementToXml content)
contentElementToXml (ImageElement url alt) = 
  XmlElement "image" [("url", url)] (map contentElementToXml alt)