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

-- Updated tag types to include Exclusive
data TagType = Normal | Dud | Exclusive
  deriving (Show, Eq)

data Tag = Tag 
  { tagName :: String
  , tagType :: TagType
  , tagAlias :: String
  , parent :: Maybe String
  , children :: [Tag]
  } deriving (Show, Eq)

-- Updated FlatTag to include ancestry
data FlatTag = FlatTag
  { flatTagName :: String
  , flatTagType :: TagType
  , flatTagParent :: String
  , flatTagChildren :: [String]
  , flatTagAncestry :: [String]  -- Added ancestry field
  } deriving (Show, Eq)

data ENTSData = ENTSData
  { aliases :: Map String String
  , tags :: [Tag]
  } deriving (Show, Eq)

data FlatENTSData = FlatENTSData
  { flatAliases :: Map String String
  , flatTags :: [FlatTag]
  } deriving (Show, Eq)

-- Updated ToJSON for TagType to include Exclusive
instance ToJSON TagType where
  toJSON Normal = JSON.String (T.pack "normal")
  toJSON Dud = JSON.String (T.pack "dud")
  toJSON Exclusive = JSON.String (T.pack "exclusive")

-- Updated ToJSON for FlatTag to include ancestry
instance ToJSON FlatTag where
  toJSON (FlatTag name tagType parent children ancestry) = JSON.Object $ KeyMap.fromList
    [ (Key.fromString "name", JSON.String (T.pack name))
    , (Key.fromString "type", toJSON tagType)
    , (Key.fromString "parent", JSON.String (T.pack parent))
    , (Key.fromString "children", toJSON (map T.pack children))
    , (Key.fromString "ancestry", toJSON (map T.pack ancestry))  -- Added ancestry field
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

-- Functions to calculate ancestry
-- Build a map from tag names to their parent
buildParentMap :: [FlatTag] -> Map String String
buildParentMap tags = Map.fromList [(flatTagName tag, flatTagParent tag) | tag <- tags]

-- Calculate ancestry for a tag using the parent map
calculateAncestry :: Map String String -> String -> [String]
calculateAncestry parentMap tagName = 
  let
    getAncestry :: String -> [String]
    getAncestry name = 
      case Map.lookup name parentMap of
        Nothing -> []  -- Should not happen
        Just "root" -> []  -- Root has no ancestors
        Just parent -> getAncestry parent ++ [parent]
  in
    getAncestry tagName

-- Add ancestry to flattened tags
addAncestry :: [FlatTag] -> [FlatTag]
addAncestry tags = 
  let parentMap = buildParentMap tags
  in [addAncestryToTag parentMap tag | tag <- tags]
  where
    addAncestryToTag :: Map String String -> FlatTag -> FlatTag
    addAncestryToTag parentMap tag = 
      let name = flatTagName tag
          ancestry = calculateAncestry parentMap name
      in tag {flatTagAncestry = ancestry}

-- Updated tag type parser to handle exclusive tags with -+ or +-
tagTypeParser :: Parser TagType
tagTypeParser = try exclusiveParser <|> normalDudParser
  where
    exclusiveParser = do
      choice [
        try (string "+-"),
        try (string "-+")
        ]
      return Exclusive
      
    normalDudParser = 
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

-- Updated flattenTags to initialize tags with empty ancestry
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
        flatTag = FlatTag name tType parentStr childNames []  -- Empty ancestry initially
        newAcc = if any (\ft -> flatTagName ft == name) acc
                 then acc
                 else acc ++ [flatTag]
      in
        foldl (\a c -> flattenTagWithChildren c a) newAcc childTags

-- Updated parseENTSFlat to add ancestry to the tags
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
      flatTagListWithAncestry = addAncestry flatTagList
  return $ FlatENTSData validatedAliases flatTagListWithAncestry

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
