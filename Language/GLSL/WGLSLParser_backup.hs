module Language.GLSL.WGSLParser where

import Control.Monad (join, replicateM_, void)
import Data.Char
import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos

import Prelude hiding (break, exponent)
import Debug.Trace(trace)

import Language.GLSL.Syntax
import Language.GLSL.Parser


data W = W

type Par a = GenParser Char W a

type Name = String
type Position = Int

parseFromFile' :: Par a -> FilePath -> IO (Either ParseError a)
parseFromFile' p fname = do
  input <- readFile fname
  return (runParser p Language.GLSL.WGSLParser.W fname input)

comment' :: Par ()
comment' = do
  _ <- char '/'
  _ <-
    choice
      [ do
          _ <- char '*'
          manyTill anyChar (try $ string "*/"),
        do
          _ <- char '/'
          manyTill anyChar ((newline >> return ()) <|> eof)
      ]
  return ()

blank' :: Par ()
blank' = try comment' <|> (space >> return ())

colon' :: Par ()
colon' = lexeme' $ char ':' >> return ()

curly :: Par Char
curly = char '{' <|> char '}'

-- Try to parse a given string, making sure it is not a
-- prefix of an identifier.
tilKeyword :: String -> Par String
tilKeyword w = manyTill anyChar (string w)

-- Try to parse a given string, and allow identifier characters
-- (or anything else) to directly follow.
operator' :: String -> Par String
operator' = lexeme' . try . string

-- -- Acts like p and discards any following space character.
lexeme' :: Par a -> Par a
lexeme' p = do
  x <- p
  skipMany blank'
  return x

symbol :: Char -> Par ()
symbol c = void $ lexeme' $ char c

manyany :: Par end -> Par String
manyany = manyTill anyChar

linez :: Par String
linez = many $ char '\n'

spacez :: Par String 
spacez = many $ char ' '

spacelinez :: Par String
spacelinez = many $ char ' ' <|> char '\n'

wholeText :: Par String
wholeText = do
  manyTill anyChar eof

-- the line number of a given position starts at the 13th character when calling
-- getPosition in a parser. This functions turns the substring starting
-- at the 13th character into an integer, thus getting the integer line number.
getLineNumber :: String -> Int
getLineNumber s = read $ takeWhile isDigit $ substring 13 (13+3) s

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

-- gets content as a function of scope: 0 is global, and increasing numbers are chronological
parseScope :: Int -> [Char] -> Either ParseError (Position, String)
parseScope n = runParser getScoped W "WGSL"
  where getScoped = do
          skipMany blank'
          choice [try $ betweenBraces n, return (-11, "error")]

-- the argument is the nth scope to explore
betweenBraces :: Int -> Par (Position, String)
betweenBraces n = do
  -- replicateM_ n (manyTill anyChar $ char '{')
  pos <- getPosition
  scopedContent <- getScopedContent 1
  -- void $ putStrLn scopedContent
  return (getLineNumber (show pos), scopedContent)

-- for a given opening curly brace, finds the correspond closing curly brace, 
-- and returns the content between them

  








-- have to detect updates of type p.x =,  p.yw =












getScopedContent :: Int -> Par String
-- getScopedContent _ = do choice [try $ manyTill anyChar eof, return " "]
getScopedContent _ = do manyTill anyChar eof

-- getScopedContent 0 = return " "
-- getScopedContent i = do
--   b <- choice [try $ manyTill anyChar (lookAhead curly) <> fmap (: []) curly, return " "]
--   c <- case last b of
--     '{' -> getScopedContent (i + 1)
--     '}' -> getScopedContent (i -1)
--     _ -> return ""

--   return $ b ++ c


-- finds "let" tokens and return their position so that we can raplace them with vars if applicable
findLetParse :: [Char] -> Either ParseError [(Position, Name)]
findLetParse =
  runParser manyLets   W   "WGSL"

manyLets :: Par [(Position, Name)]
manyLets = do 
  many oneLet

-- finds the next "let" keyword. Returns its name and position in the scope
oneLet :: Par (Position, Name) 
oneLet = do
  void $ try $ tilKeyword "let "
  pos <- getPosition
  -- maybeName <- option (False, "") $ try $ lookAhead isRepeated
  
  -- case maybeName of
  --   Just name -> return (getLineNumber (show pos), name) 
  --   Nothing -> return (-15, "not found")
   
  (varUpdateExists, varName) <- option (False, "") $ try $ lookAhead isRepeated
  -- trace $ varUpdateExists
  
  if varUpdateExists
    then return (getLineNumber (show pos), varName) 
    else return (-15, varName)
   
  
-- -- finds whether or not a declared variable is being updated later in the file
-- isRepeated :: Par (Maybe Name)
-- isRepeated = do
--   name <- manyany $ char ' ' <|> char ':'
--   c <-  option "hmm" $  manyany $ lookAhead $ try $ string (name ++ " = ") 
--   col <- case c of 
--       "" -> return "nah"
--       _  -> manyany $  char ' ' <|> char ':'
--   _ <- manyany eof

--   case col of 
--     "" -> return Nothing
--     _  -> return $ Just name

-- finds whether or not a declared variable is being updated later in the file
isRepeated :: Par (Bool, Name)
isRepeated = do
  name <- manyany $ char ' ' <|> char ':'
  c <-  option "hmm" $  manyany $ lookAhead $ try $ string (name ++ " = ") 
  col <- case c of 
      "" -> return "nah"
      _  -> manyany $  char ' ' <|> char ':'
  _ <- manyany eof

  case col of 
    "" -> return (False, "err")
    _  -> return (True, name)