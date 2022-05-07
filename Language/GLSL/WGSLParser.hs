module Language.GLSL.WGSLParser (Leto (Errco, Leto), Name, Position, Par, findLetParse, parseWhole) where

import Control.Monad (join, replicateM, void)
import Data.Char
import Debug.Trace (trace)
-- import Language.GLSL.Parser
import Language.GLSL.Syntax
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Expr
import Prelude hiding (break, exponent)

data S = S

type Par a = GenParser Char S a

type Name = String

type Position = Int

data Leto = Leto Name Position | Errco
  deriving (Show)

-- parseFromFile' :: Par a -> FilePath -> IO (Either ParseError a)
-- parseFromFile' p fname = do
--   input <- readFile fname
--   return (runParser p Language.GLSL.WGSLParser.W fname input)

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

colon :: Par ()
colon = lexeme' $ char ':' >> return ()

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
getLineNumber s = read $ takeWhile isDigit $ substring 13 (13 + 3) s

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

-- -- gets content as a function of scope: 0 is global, and increasing numbers are chronological
-- parseScope :: Int -> [Char] -> Either ParseError (Position, String)
-- parseScope n = runParser getScoped W "WGSL"
--   where getScoped = do
--           skipMany blank'
--           choice [try $ betweenBraces n, return (-11, "error")]

-- -- the argument is the nth scope to explore
-- betweenBraces :: Int -> Par (Position, String)
-- betweenBraces n = do
--   -- replicateM_ n (manyTill anyChar $ char '{')
--   pos <- getPosition
--   scopedContent <- getScopedContent 1
--   -- void $ putStrLn scopedContent
--   return (getLineNumber (show pos), scopedContent)

-- for a given opening curly brace, finds the correspond closing curly brace,
-- and returns the content between them

parseWhole :: [Char] -> Either ParseError String
parseWhole = runParser getScoped S "WGSL"
  where
    getScoped = do
      choice [try readAll, return "error"]

readAll :: Par String
readAll = do manyTill anyChar eof

-- -- finds "let" tokens and return their position so that we can raplace them with vars if applicable
-- findLetParse :: [Char] -> Either ParseError [(Position, Name)]
-- findLetParse =
--   runParser manyLets W "WGSL"

-- finds "let" tokens and return their position so that we can raplace them with vars if applicable
-- findLetParse :: [Char] -> Either ParseError [(Position, Name)]
findLetParse :: [Char] -> Either ParseError String
findLetParse =
  runParser manyLets S "WGSL"

-- manyLets :: Par [(Position, Name)]
manyLets :: Par String
manyLets = fmap join $ do manyTill (try checkLets) eof

-- oneLet :: Par (Position, Name)
-- oneLet = do
--   choice [letPos, readAll >> return (-1, "err")]

-- [letPos, return (-1, "")]

-- letPos :: Par (Position, Name)
-- letPos = do
--   void $ (tilKeyword "let")
--   intpos <- getLineNumber . show <$> getPosition
--   return (intpos, "let")

-- keywordLet :: GenParser Char st ()
-- keywordLet =
--   try
--     ( do
--         _ <- string "let"
--         notFollowedBy alphaNum
--     )

-- IT WORKS:
-- 0. return let positions that have updates
-- 1. make sure the update of type p.xy are detected
-- 2. replace lets

-- either return LET name = ...
-- or            VAR name = ...
-- either return ___ name: type = ...
-- or            ___ name = ...

checkLets :: Par String
checkLets = do
  choice
    [ try $ do
        meh <- manyTill anyChar $ try $ string "let "
        -- void $ try (string "let ")

        name <- manyTill anyChar $ lookAhead typeOrSpace
        ts <- typeOrSpace
        -- c <- anyChar

        -- ts <- typeOrSpace
        -- typeOrSpace <-

        maybeName <- lookAhead $ isRepeated name
        if maybeName
          then return (meh ++ "var " ++ name ++ ts)
          else return (meh ++ "let " ++ name ++ ts),
      manyTill anyChar eof >> return ""
    ]

typeOrSpace :: Par String
typeOrSpace = do
  choice [try readType, string " "]

-- letOrVar :: Par String
-- letOrVar = do
--   option "let " $
--     (try $ do
--         name <- manyTill anyChar (choice [try readType, string " "])

--     )

readType :: Par String
readType = do
  c <- string ": "
  t <- manyTill anyChar (char ' ')
  return $ ": " ++ t ++ " "

-- finds whether or not a declared variable is being updated later in the file
isRepeated :: String -> Par Bool
isRepeated name = do
  option
    False
    $ do
      void $ try $ lookAhead $ manyany $ try $ string (name ++ " = ")
      return True

--   return "Nothing"
-- ]
-- case c of
--   "Nothing" -> return Nothing
--   x -> return $ Just name

-- -- finds whether or not a declared variable is being updated later in the file
-- isRepeated :: String -> Par (Maybe Name)
-- isRepeated name = do
--   c <-
--     option
--       "Nothing"
--       (try $ lookAhead $ manyany $ try $ string (name ++ " = ") >> return "just")
--   --   return "Nothing"
--   -- ]
--   case c of
--     "Nothing" -> return Nothing
--     x -> return $ Just name

-- choice
--   [ try $ do
--       meh <- manyTill anyChar $ lookAhead $ string "let"
--       -- moh <- return "let "
--       moh <- string "let"

--       pos <- show <$> getPosition
--       return $ Leto (meh ++ moh) (getLineNumber pos),
--     -- return $ Leto (meh ++ join moh) (22),
--     do
--       manyTill anyChar eof
--       return $ Leto "error" (-1)
--   ]

-- getLineNumber . show <$> getPosition

-- -- finds the next "let" keyword. Returns its name and position in the scope
-- oneLet :: Par (Position, Name)
-- oneLet = do
--   void $ try $ tilKeyword "let "
--   pos <- getPosition
--   maybeName <- option Nothing $ try $ lookAhead isRepeated
--   case maybeName of
--     Just name -> return (getLineNumber (show pos), name)
--     Nothing -> return (-15, "not found")

-- -- finds whether or not a declared variable is being updated later in the file
-- isRepeated :: String -> Par (Maybe Name)
-- isRepeated name = do
--   -- name <- manyany $ char ' ' <|> char ':'
--   c <- option "hmm" $ manyany $ lookAhead $ try $ string (name ++ " = ")
--   col <- case c of
--     "" -> return "nah"
--     _ -> manyany $ char ' ' <|> char ':'
--   _ <- manyany eof

--   case col of
--     "" -> return Nothing
--     _ -> return $ Just name
