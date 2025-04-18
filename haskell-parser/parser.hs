module Main where

import Control.Monad (void)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String
import System.Environment (getArgs)
import System.IO (readFile)
-- New imports for alias functionality (all from standard library)
import Data.Map (Map)                -- For storing alias-to-tagname mappings
import qualified Data.Map as Map     -- For Map operations
import Data.List (nub)               -- For finding duplicate aliases

-- Define our tag types
data TagType = Normal | Dud
  deriving (Show, Eq)

-- Define our tag structure
data Tag = Tag 
  { tagName :: String
  , tagType :: TagType
  , tagAlias :: String
  , parent :: Maybe String  -- Parent tag name, Nothing for root tags
  , children :: [Tag]
  } deriving (Show, Eq)

-- Define a flat tag representation
data FlatTag = FlatTag
  { flatTagName :: String
  , flatTagType :: TagType
  , flatTagParent :: String  -- "root" for top-level tags
  , flatTagChildren :: [String]  -- List of child tag names
  } deriving (Show, Eq)

-- Define the overall structure including aliases and tags
data ENTSData = ENTSData
  { aliases :: Map String String  -- Map from alias to tag name
  , tags :: [Tag]  -- Original hierarchical structure (for processing)
  } deriving (Show, Eq)

-- Define the flat structure for JSON output
data FlatENTSData = FlatENTSData
  { flatAliases :: Map String String    -- Map from alias to tag name
  , flatTags :: [FlatTag]               -- List of flat tags to preserve order
  } deriving (Show, Eq)

-- Make TagType convertible to JSON
instance ToJSON TagType where
  toJSON Normal = JSON.String (T.pack "normal")
  toJSON Dud = JSON.String (T.pack "dud")

-- Make FlatTag convertible to JSON
instance ToJSON FlatTag where
  toJSON (FlatTag name tagType parent children) = JSON.Object $ KeyMap.fromList
    [ (Key.fromString "name", JSON.String (T.pack name))
    , (Key.fromString "type", toJSON tagType)
    , (Key.fromString "parent", JSON.String (T.pack parent))
    , (Key.fromString "children", toJSON (map T.pack children))
    ]

-- Make FlatENTSData convertible to JSON
instance ToJSON FlatENTSData where
  toJSON (FlatENTSData aliasMap tagList) = JSON.Object $ KeyMap.fromList
    [ (Key.fromString "aliases", aliasesToJSON aliasMap)
    , (Key.fromString "tags", toJSON tagList)
    ]
    where
      aliasesToJSON :: Map String String -> JSON.Value
      aliasesToJSON m = JSON.Object $ 
        KeyMap.fromList [ (Key.fromString k, JSON.String (T.pack v)) | (k, v) <- Map.toList m ]

-- Parse tag type based on prefix
tagTypeParser :: Parser TagType
tagTypeParser = 
  (char '+' >> return Dud) <|>
  (char '-' >> return Normal)

-- Parse whitespace but not newlines
spaceNoNL :: Parser ()
spaceNoNL = void $ many (satisfy (\c -> isSpace c && c /= '\n' && c /= '\r'))

-- Helper function to handle escaped parentheses
escapedChar :: Char -> Parser Char
escapedChar c = try (string ['\\', c] >> return c)

-- Parse the alias in parentheses
aliasParser :: Parser String
aliasParser = try $ do
  -- Parse opening parenthesis
  char '('
  
  -- Parse alias content (allowing escaped parentheses)
  alias <- many (noneOf ")" <|> escapedChar ')')
  
  -- Parse closing parenthesis with error message
  char ')' <?> "closing parenthesis for alias"
  
  -- Ensure nothing follows except newline, EOF, or another alias
  lookAhead $ choice [
    void newline,
    void eof,
    try (void $ char '('),
    fail "expected newline or end of file after alias"
    ]
  
  -- Return the trimmed alias
  return $ trim alias
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Parse tag name, which is text after prefix until colon, newline, or opening parenthesis
tagNameParser :: Parser (String, String)
tagNameParser = do
  spaceNoNL
  name <- many1 (noneOf ":()\r\n" <|> escapedChar '(' <|> escapedChar ')')
  -- Try to parse an alias if present
  alias <- option "" aliasParser
  -- Optional colon after name or alias
  optional (char ':')
  return (trim name, alias)
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Count indentation spaces
indentation :: Parser Int
indentation = do
  spaces <- many (char ' ')
  return $ length spaces

-- Main tag parser that handles indentation and hierarchy
tagParser :: Int -> Maybe String -> Parser Tag
tagParser indentLevel parentName = do
  currentIndent <- indentation
  if currentIndent /= indentLevel
    then fail $ "Expected " ++ show indentLevel ++ " spaces, got " ++ show currentIndent
    else do
      tagType <- tagTypeParser
      (name, alias) <- tagNameParser
      newline <|> (eof >> return '\n')
      -- Parse children at a deeper indentation level
      children <- many $ try $ lookAhead (do
        indent <- indentation
        return $ indent == indentLevel + 4) >> tagParser (indentLevel + 4) (Just name)
      return $ Tag name tagType alias parentName children

-- Parse the entire ENTS file
parseENTS :: String -> Either ParseError [Tag]
parseENTS input = parse (many (tagParser 0 Nothing) <* eof) "ENTS" input

-- Extract all aliases from a tag tree
extractAliases :: [Tag] -> Map String String
extractAliases tags = Map.fromList $ extractAliasesHelper tags
  where
    extractAliasesHelper :: [Tag] -> [(String, String)]
    extractAliasesHelper [] = []
    extractAliasesHelper (Tag name _ alias _ children : rest) = 
      (if alias /= "" then [(alias, name)] else []) ++ 
      extractAliasesHelper children ++ 
      extractAliasesHelper rest

-- Validate that there are no duplicate aliases
validateAliases :: Map String String -> Either String (Map String String)
validateAliases aliasMap =
  let aliases = Map.keys aliasMap
      uniqueAliases = nub aliases
  in if length aliases == length uniqueAliases
       then Right aliasMap
       else Left "Duplicate aliases found"

-- Convert hierarchical tags to flat structure
flattenTags :: [Tag] -> [FlatTag]
flattenTags tags = flattenTagsList tags []
  where
    -- Process tags in order, keeping track of processed tags to avoid duplicates
    flattenTagsList :: [Tag] -> [FlatTag] -> [FlatTag]
    flattenTagsList [] acc = acc
    flattenTagsList (tag:rest) acc =
      -- Process this tag and its children
      let currentTagResult = flattenTagWithChildren tag acc
      in flattenTagsList rest currentTagResult
    
    -- Process a tag and all its children, adding them to the accumulator
    flattenTagWithChildren :: Tag -> [FlatTag] -> [FlatTag]
    flattenTagWithChildren tag acc =
      let 
        name = tagName tag
        tType = tagType tag
        parentName = parent tag
        childTags = children tag
        childNames = map tagName childTags
        parentStr = case parentName of
                      Nothing -> "root"
                      Just p -> p
        flatTag = FlatTag name tType parentStr childNames
        -- First add the current tag if not already in the accumulator
        newAcc = if any (\ft -> flatTagName ft == name) acc
                 then acc
                 else acc ++ [flatTag]
      in
        -- Then process all children, preserving their order
        foldl (\a c -> flattenTagWithChildren c a) newAcc childTags

-- Combine parsing and conversion to flat structure
parseENTSFlat :: String -> Either String FlatENTSData
parseENTSFlat input = do
  parsedTags <- case parseENTS input of
                  Left err -> Left $ "Parse error: " ++ show err
                  Right tags -> Right tags
  
  let aliasMap = extractAliases parsedTags
  validatedAliases <- validateAliases aliasMap
  
  let flatTagList = flattenTags parsedTags
  return $ FlatENTSData validatedAliases flatTagList

-- Convert AST to JSON string
entsToJSON :: FlatENTSData -> BS.ByteString
entsToJSON = JSON.encode

-- Main function to parse ENTS and output JSON
parseENTSToJSON :: String -> Either String BS.ByteString
parseENTSToJSON input = entsToJSON <$> parseENTSFlat input

-- Main function to handle command-line args
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      input <- readFile inputFile
      case parseENTSToJSON input of
        Left err -> putStrLn $ "Error: " ++ err
        Right jsonData -> BS.writeFile outputFile jsonData
      putStrLn $ "Successfully parsed " ++ inputFile ++ " to " ++ outputFile
    _ -> putStrLn "Usage: ./parser [input.ents] [output.json]"
