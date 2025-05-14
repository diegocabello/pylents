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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub)

data TagType = Normal | Dud
  deriving (Show, Eq)

data Tag = Tag 
  { tagName :: String
  , tagType :: TagType
  , tagAlias :: String
  , parent :: Maybe String
  , children :: [Tag]
  } deriving (Show, Eq)

data FlatTag = FlatTag
  { flatTagName :: String
  , flatTagType :: TagType
  , flatTagParent :: String
  , flatTagChildren :: [String]
  } deriving (Show, Eq)

data ENTSData = ENTSData
  { aliases :: Map String String
  , tags :: [Tag]
  } deriving (Show, Eq)

data FlatENTSData = FlatENTSData
  { flatAliases :: Map String String
  , flatTags :: [FlatTag]
  } deriving (Show, Eq)

instance ToJSON TagType where
  toJSON Normal = JSON.String (T.pack "normal")
  toJSON Dud = JSON.String (T.pack "dud")

instance ToJSON FlatTag where
  toJSON (FlatTag name tagType parent children) = JSON.Object $ KeyMap.fromList
    [ (Key.fromString "name", JSON.String (T.pack name))
    , (Key.fromString "type", toJSON tagType)
    , (Key.fromString "parent", JSON.String (T.pack parent))
    , (Key.fromString "children", toJSON (map T.pack children))
    ]

instance ToJSON FlatENTSData where
  toJSON (FlatENTSData aliasMap tagList) = JSON.Object $ KeyMap.fromList
    [ (Key.fromString "aliases", aliasesToJSON aliasMap)
    , (Key.fromString "tags", toJSON tagList)
    ]
    where
      aliasesToJSON :: Map String String -> JSON.Value
      aliasesToJSON m = JSON.Object $ 
        KeyMap.fromList [ (Key.fromString k, JSON.String (T.pack v)) | (k, v) <- Map.toList m ]

tagTypeParser :: Parser TagType
tagTypeParser = 
  (char '+' >> return Dud) <|>
  (char '-' >> return Normal)

spaceNoNL :: Parser ()
spaceNoNL = void $ many (satisfy (\c -> isSpace c && c /= '\n' && c /= '\r'))

escapedChar :: Char -> Parser Char
escapedChar c = try (string ['\\', c] >> return c)

aliasParser :: Parser String
aliasParser = try $ do
  char '('
  
  alias <- many (noneOf ")" <|> escapedChar ')')
  
  char ')' <?> "closing parenthesis for alias"
  
  spaceNoNL  -- Allow spaces after closing parenthesis
  
  lookAhead $ choice [
    void newline,
    void eof,
    try (void $ char '('),
    fail "expected newline or end of file after alias"
    ]
  
  return $ trim alias
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

tagNameParser :: Parser (String, String)
tagNameParser = do
  spaceNoNL
  name <- many1 (noneOf ":()\r\n" <|> escapedChar '(' <|> escapedChar ')')
  
  spaceNoNL  -- Allow spaces between name and alias
  
  alias <- option "" aliasParser
  
  optional (char ':')
  return (trim name, alias)
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

indentation :: Parser Int
indentation = do
  spaces <- many (char ' ')
  return $ length spaces

tagParser :: Int -> Maybe String -> Parser Tag
tagParser indentLevel parentName = do
  currentIndent <- indentation
  if currentIndent /= indentLevel
    then fail $ "Expected " ++ show indentLevel ++ " spaces, got " ++ show currentIndent
    else do
      tagType <- tagTypeParser
      (name, alias) <- tagNameParser
      newline <|> (eof >> return '\n')
      
      children <- many $ try $ lookAhead (do
        indent <- indentation
        return $ indent == indentLevel + 4) >> tagParser (indentLevel + 4) (Just name)
      
      return $ Tag name tagType alias parentName children

parseENTS :: String -> Either ParseError [Tag]
parseENTS input = parse (many (tagParser 0 Nothing) <* eof) "ENTS" input

extractAliases :: [Tag] -> Map String String
extractAliases tags = Map.fromList $ extractAliasesHelper tags
  where
    extractAliasesHelper :: [Tag] -> [(String, String)]
    extractAliasesHelper [] = []
    extractAliasesHelper (Tag name _ alias _ children : rest) = 
      (if alias /= "" then [(alias, name)] else []) ++ 
      extractAliasesHelper children ++ 
      extractAliasesHelper rest

validateAliases :: Map String String -> Either String (Map String String)
validateAliases aliasMap =
  let aliases = Map.keys aliasMap
      uniqueAliases = nub aliases
  in if length aliases == length uniqueAliases
       then Right aliasMap
       else Left "Duplicate aliases found"

validateDudTags :: [Tag] -> Either String [Tag]
validateDudTags tags = 
  if any hasDudTagWithAlias tags
    then Left "Dud tags cannot have aliases"
    else Right tags
  where
    hasDudTagWithAlias :: Tag -> Bool
    hasDudTagWithAlias (Tag _ Dud alias _ _) = alias /= ""
    hasDudTagWithAlias (Tag _ _ _ _ children) = any hasDudTagWithAlias children

flattenTags :: [Tag] -> [FlatTag]
flattenTags tags = flattenTagsList tags []
  where
    flattenTagsList :: [Tag] -> [FlatTag] -> [FlatTag]
    flattenTagsList [] acc = acc
    flattenTagsList (tag:rest) acc =
      let currentTagResult = flattenTagWithChildren tag acc
      in flattenTagsList rest currentTagResult
    
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
        newAcc = if any (\ft -> flatTagName ft == name) acc
                 then acc
                 else acc ++ [flatTag]
      in
        foldl (\a c -> flattenTagWithChildren c a) newAcc childTags

parseENTSFlat :: String -> Either String FlatENTSData
parseENTSFlat input = do
  parsedTags <- case parseENTS input of
                  Left err -> Left $ "Parse error: " ++ show err
                  Right tags -> Right tags
  
  let aliasMap = extractAliases parsedTags
  validatedAliases <- validateAliases aliasMap
  
  -- Uncomment the following line if you want to enforce that dud tags cannot have aliases
  -- validatedTags <- validateDudTags parsedTags
  
  let flatTagList = flattenTags parsedTags
  return $ FlatENTSData validatedAliases flatTagList

entsToJSON :: FlatENTSData -> BS.ByteString
entsToJSON = JSON.encode

parseENTSToJSON :: String -> Either String BS.ByteString
parseENTSToJSON input = entsToJSON <$> parseENTSFlat input

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
