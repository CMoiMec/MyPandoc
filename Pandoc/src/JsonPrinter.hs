{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- JsonPrinter
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module JsonPrinter where
import JsonParser (JsonValue(..))
import Data.List (intercalate)
import Document

-- | Pretty print JSON with indentation
printJson :: JsonValue -> String
printJson = printJsonIndent 0

-- | Pretty print JSON with given indentation level
printJsonIndent :: Int -> JsonValue -> String
printJsonIndent _ JsonNull = "null"
printJsonIndent _ (JsonBool True) = "true"
printJsonIndent _ (JsonBool False) = "false"
printJsonIndent _ (JsonNumber n) = show n
printJsonIndent _ (JsonString s) = "\"" ++ s ++ "\""
printJsonIndent indent (JsonArray values) = 
    "[\n" ++ 
    intercalate ",\n" (map (addIndent (indent + 4) .
    printJsonIndent (indent + 4)) values) ++ "\n" ++ addIndent indent "]"
printJsonIndent indent (JsonObject kvs) = 
    "{\n" ++ 
    intercalate ",\n" (map (printKeyValue (indent + 4)) kvs) ++ 
    "\n" ++ addIndent indent "}"
    where
        printKeyValue :: Int -> (String, JsonValue) -> String
        printKeyValue i (k, v) =
            addIndent i ("\"" ++ k ++ "\": " ++ printJsonIndent i v)

-- | Add indentation spaces
addIndent :: Int -> String -> String
addIndent n s = replicate n ' ' ++ s

-- | Convert JsonValue to Document
jsonToDocument :: JsonValue -> Maybe Document
jsonToDocument (JsonObject kvs) = do
    headerObj <- lookupJsonValue "header" kvs >>= asJsonObject
    bodyArr <- lookupJsonValue "body" kvs >>= asJsonArray
    hdr <- parseHeaderFromJson headerObj
    bodyContents <- parseBody bodyArr
    return $ Document hdr bodyContents
jsonToDocument _ = Nothing

-- | Parse the document body from JSON
parseBody :: [JsonValue] -> Maybe [Content]
parseBody jsonValues = mapM parseContent jsonValues

-- | Parse a content element from JSON
parseContent :: JsonValue -> Maybe Content
parseContent (JsonArray elements) = do
    contentElements <- mapM parseContentElement elements
    return $ Paragraph contentElements
parseContent (JsonObject obj) = 
    parseSpecialContent obj
parseContent _ = Nothing

-- | Parse special content types (section, codeblock, list)
parseSpecialContent :: [(String, JsonValue)] -> Maybe Content
parseSpecialContent obj
    | Just sectionObj <- lookupJsonValue "section" obj >>=
        asJsonObject = parseSection sectionObj
    | Just codeLines <- lookupJsonValue "codeblock" obj >>=
        asJsonArray = parseCodeBlock codeLines
    | Just listItems <- lookupJsonValue "list" obj >>=
        asJsonArray = parseList listItems
    | otherwise = Nothing

-- | Parse a section from JSON
parseSection :: [(String, JsonValue)] -> Maybe Content
parseSection obj = do
    ttl <- case lookupJsonValue "title" obj >>= asJsonString of
                Just t -> Just t
                Nothing -> Just ""
    content <- lookupJsonValue "content" obj >>= asJsonArray >>= parseBody
    return $ Section (if null ttl then Nothing else Just ttl) content

-- | Parse a code block from JSON
parseCodeBlock :: [JsonValue] -> Maybe Content
parseCodeBlock allLines = do
    codeLines <- mapM asJsonString allLines
    return $ CodeBlock codeLines

-- | Parse a list from JSON
parseList :: [JsonValue] -> Maybe Content
parseList items = do
    listItems <- mapM parseListItem items
    return $ ListContent listItems

-- | Parse a list item from JSON
parseListItem :: JsonValue -> Maybe [ContentElement]
parseListItem (JsonArray elements) = mapM parseContentElement elements
parseListItem _ = Nothing

-- | Parse a content element from JSON
parseContentElement :: JsonValue -> Maybe ContentElement
parseContentElement (JsonString s) = Just $ TextElement s
parseContentElement (JsonObject obj)
    | Just bold<-lookupJsonValue"bold"obj>>=asJsonString=Just$BoldElement bold
    | Just italic <- lookupJsonValue "italic" obj >>=
        asJsonString = Just $ ItalicElement italic
    | Just code<-lookupJsonValue"code"obj>>=asJsonString=Just$CodeElement code
    | Just linkObj <- lookupJsonValue "link" obj >>=
        asJsonObject = parseLinkElement linkObj
    | Just imgObj <- lookupJsonValue "image" obj >>=
        asJsonObject = parseImageElement imgObj
    | otherwise = Nothing
parseContentElement _ = Nothing

-- | Parse a link element from JSON
parseLinkElement :: [(String, JsonValue)] -> Maybe ContentElement
parseLinkElement obj = do
    url <- lookupJsonValue "url" obj >>= asJsonString
    content <- lookupJsonValue "content" obj >>= asJsonArray >>=
        mapM parseContentElement
    return $ LinkElement url content

-- | Parse an image element from JSON
parseImageElement :: [(String, JsonValue)] -> Maybe ContentElement
parseImageElement obj = do
    url <- lookupJsonValue "url" obj >>= asJsonString
    alt <- lookupJsonValue "alt" obj>>=asJsonArray>>=mapM parseContentElement
    return $ ImageElement url alt

-- | Parse header information from JSON
parseHeaderFromJson :: [(String, JsonValue)] -> Maybe Header
parseHeaderFromJson obj = do
    ttl <- lookupJsonValue "title" obj >>= asJsonString
    let auth = lookupJsonValue "author" obj >>= asJsonString
        dt = lookupJsonValue "date" obj >>= asJsonString
    return $ Header ttl auth dt

-- | Convert Document to JSON representation
documentToJson :: Document -> JsonValue
documentToJson (Document hdr bdy) =
    JsonObject [
        ("header", headerToJson hdr),
        ("body", JsonArray (map contentToJson bdy))
    ]

-- | Convert Header to JSON
headerToJson :: Header -> JsonValue
headerToJson hdr =
    JsonObject $ 
        [("title", JsonString (title hdr))] ++
        maybe [] (\a -> [("author", JsonString a)]) (author hdr) ++
        maybe [] (\d -> [("date", JsonString d)]) (date hdr)

-- | Convert Content to JSON
contentToJson :: Content -> JsonValue
contentToJson (Paragraph elements) = 
    JsonArray (map contentElementToJson elements)
contentToJson (Section ttl contents) =
    JsonObject [
        ("section", JsonObject [
            ("title", maybe (JsonString "") JsonString ttl),
            ("content", JsonArray (map contentToJson contents))
        ])
    ]
contentToJson (CodeBlock allLines) =
    JsonObject [
        ("codeblock", JsonArray (map JsonString allLines))
    ]
contentToJson (ListContent items) =
    JsonObject [
        ("list",
        JsonArray (map (\item -> JsonArray (map contentElementToJson item))
        items))
    ]

-- | Convert ContentElement to JSON
contentElementToJson :: ContentElement -> JsonValue
contentElementToJson (TextElement text) = JsonString text
contentElementToJson (BoldElement text) = 
    JsonObject [("bold", JsonString text)]
contentElementToJson (ItalicElement text) = 
    JsonObject [("italic", JsonString text)]
contentElementToJson (CodeElement text) = 
    JsonObject [("code", JsonString text)]
contentElementToJson (LinkElement url content) =
    JsonObject [
        ("link", JsonObject [
            ("url", JsonString url),
            ("content", JsonArray (map contentElementToJson content))
        ])
    ]
contentElementToJson (ImageElement url alt) =
    JsonObject [
        ("image", JsonObject [
            ("url", JsonString url),
            ("alt", JsonArray (map contentElementToJson alt))
        ])
    ]

-- | Helper functions for JSON conversion
asJsonString :: JsonValue -> Maybe String
asJsonString (JsonString s) = Just s
asJsonString _ = Nothing

asJsonObject :: JsonValue -> Maybe [(String, JsonValue)]
asJsonObject (JsonObject obj) = Just obj
asJsonObject _ = Nothing

asJsonArray :: JsonValue -> Maybe [JsonValue]
asJsonArray (JsonArray arr) = Just arr
asJsonArray _ = Nothing

lookupJsonValue :: String -> [(String, JsonValue)] -> Maybe JsonValue
lookupJsonValue key = lookup key