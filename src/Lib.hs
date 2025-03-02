module Lib  where

import Data.Void
import Text.Megaparsec

import qualified Text.Megaparsec.Char as C
import System.Environment (getArgs)

run :: IO ()
run  = do 
    args <- getArgs
    case args of 
        [filePath] -> do 
            content <- readFile filePath
            let parseResult = parse value "" content 
            case parseResult of 
                Left err -> putStr $ errorBundlePretty err 
                Right v -> putStr $ prettyJson v 0
        _ -> putStr "Usage: program <filepath> \n"


type Parser = Parsec Void String

data VNumber = VInt Integer |  VDouble Double
    deriving (Show, Eq)

data VJson =
    VNumber VNumber
    | VString String
    | VBool Bool
    | VNull
    | VArray [VJson]
    | VObject [(String, VJson)]
    deriving (Show, Eq)




stringLiteral :: Parser VJson
stringLiteral = do
    _ <- C.char '"'
    content <- many (chunk "\\\"" <|> (:[]) <$> anySingleBut '"')
    _ <- C.char '"'
    return $ VString (concat content)

zero :: Parser String
zero = C.string "0"

digitFromOne :: Parser Char
digitFromOne = oneOf ['1'..'9']

wholePart :: Parser String
wholePart = choice [
    zero,
     ((++) . (: []) <$> digitFromOne) <*> many C.digitChar]

fractPart :: Parser String
fractPart  = do
    dot <- C.char '.'
    fract <- some C.digitChar
    return $ dot : fract

expPart  :: Parser String
expPart = do
    e <- oneOf ['E', 'e']
    sign <- option "" ((:[]) <$> oneOf ['+', '-'])
    num <- some C.digitChar
    return $ e : (sign ++ num)



number :: Parser VJson
number = do
    sign <- option "" ((:[]) <$> oneOf ['+', '-'])
    wh <- wholePart <?> "whole part of number"
    fract <- option "" fractPart
    e <- option "" expPart
    if null fract  && null e then
        return $ VNumber (VInt $ read (sign ++ wh))
    else
        return $ VNumber (VDouble $ read (sign ++ wh ++ fract ++ e))


literal :: Parser VJson
literal = do
    lit <- choice [C.string "true", C.string "false", C.string "null"]
         <?> "only 'true', 'false' and 'null' literals are valid"
    _ <- notFollowedBy C.alphaNumChar <?> "only 'true', 'false' and 'null' literals are valid"
    if lit == "true" then
        return $ VBool True
    else if lit == "false" then
        return $ VBool False
    else
        return VNull

keyValuePair :: Parser (String, VJson)
keyValuePair = do
    C.space
    VString key <- stringLiteral
    C.space
    _ <- C.char ':'
    v <- value
    return (key, v)

array :: Parser VJson
array = do
    _ <- C.char '['
    arr <- value `sepBy` C.char ','
    _ <- C.char ']'
    return $ VArray arr

object :: Parser VJson
object = do
    _ <- C.char '{'
    kvs <- keyValuePair `sepBy` C.char ','
    _ <- C.char '}'
    return $ VObject kvs

value :: Parser VJson
value = do
    C.space
    v <- try literal <|> try stringLiteral <|> try number <|> try array <|> object
    C.space
    return v

prettyJson :: VJson -> Int -> String
prettyJson VNull _ = "null"
prettyJson (VBool True) _ = "true"
prettyJson (VBool False) _ = "false"
prettyJson (VNumber (VInt i)) _ = show i
prettyJson (VString s) _ = "\"" ++ s ++ "\""
prettyJson (VNumber (VDouble d)) _ = show d
prettyJson (VArray arr) ind = "[" ++ printElems arr (ind+2) ++ "\n" ++ indent ind "]\n"
    where 
        printElems (a:as) i = foldl 
            (\acc x -> acc ++ ",\n" ++ indent i "" ++ prettyJson x i) ("\n" ++ indent i "" ++ prettyJson a i) as
        printElems [] _ = ""
        indent i s = replicate i ' ' ++ s
prettyJson (VObject arr) ind = "{" ++ printElems arr (ind+2) ++ "\n" ++ indent ind "}\n"
    where 
        printElems ((k, v):as) i = foldl 
            (\acc (k2, v2) -> acc ++ ",\n" ++ indent i "" ++ "\"" ++ k2 ++ "\" : " ++ prettyJson v2 i) 
            ("\n" ++ indent i "" ++ "\"" ++ k ++ "\" : " ++ prettyJson v i) 
            as
        printElems [] _ = ""
        indent i s = replicate i ' ' ++ s











