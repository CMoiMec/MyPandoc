{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- XmlPrinter
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module XmlPrinter where

import XmlParser (XmlNode(..))

printXml :: XmlNode -> String
printXml = printXmlWithIndent 0

printXmlWithIndent :: Int -> XmlNode -> String
printXmlWithIndent indent (XmlText text) = 
    indentStr indent ++ text
  where
    indentStr n = replicate (n * 4) ' '
-- tag with no children
printXmlWithIndent indent (XmlElement name attrs []) =
    indentStr indent ++ "<" ++ name ++ printAttrs attrs ++ "/>"
  where
    indentStr n = replicate (n * 4) ' '
    printAttrs [] = ""
    printAttrs attList =
      " " ++ unwords [k ++ "=\"" ++ v ++ "\"" | (k, v) <- attList]
-- element with children - special case (bold, italic, code, link)
printXmlWithIndent ind (XmlElement n a c)
  | isInlineElement n && allTextChildren c =
      tagStart ++ concatMap printXmlInline c ++ tagEnd
  | otherwise =
      tagStart ++ "\n" ++ inner ++ indent ++ tagEnd
  where
    indent = replicate (ind * 4) ' '
    tagStart = indent ++ "<" ++ n ++ iCantComeUpWithAName a ++ ">"
    tagEnd = "</" ++ n ++ ">"
    inner = concatMap (\ch -> printXmlWithIndent (ind + 1) ch ++ "\n") c

iCantComeUpWithAName :: [(String, String)] -> String
iCantComeUpWithAName [] = ""
iCantComeUpWithAName as = ' ' : unwords [k ++ "=\"" ++ v ++ "\"" | (k, v)<-as]

isInlineElement :: String -> Bool
isInlineElement e = e `elem` ["bold", "italic", "code", "link", "image"]

allTextChildren :: [XmlNode] -> Bool
allTextChildren = all isText where isText (XmlText _) = True; isText _ = False

printXmlInline :: XmlNode -> String
printXmlInline (XmlText t) = t
printXmlInline _ = ""


formatXml :: XmlNode -> String
formatXml node = printXml node