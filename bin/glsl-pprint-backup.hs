-- replace a value in a list at position "pos"
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- pattern match with "let " or " let " and replace it by "var " or "  var "
replaceLet :: String -> String
replaceLet ('l':'e':'t':' ':xs) = 'v' : 'a' : 'r' : ' ': xs
replaceLet [] = []
replaceLet (x:xs)       = go xs
  where go (' ':'l':'e':'t':' ':ys) = ' ': ' ' : 'v' : 'a' : 'r' : ' ': go ys
        go (y:ys)   = y : go ys
        go []       = []

-- represents the scope of within a wgsl file. Scope 0 represents the global scope
type Scope = Int


-- replace all "let " by "var ", if applicable
codePass :: Scope -> String -> IO String 
codePass n content = do 
  -- parse content by identifying the "let" keywords
  posAndScoped <- qt $ parseScope n content
  print $ posAndScoped
  posAndNames <- qt' (fst posAndScoped) $ findLetParse $ snd posAndScoped
  print $ posAndNames

  let contentLines = lines content -- turn string into list of lines
  let poss = map fst posAndNames -- extract the positions
  --
  -- replace by "var" all "let" keywords that have been identified by the parser 
  return $ unlines $ foldl f contentLines poss
        
        
  where f acc x | x < 0 = acc -- if an error occured, do nothing
        f acc lineNum = replace lineNum (letToVar acc lineNum) acc 
        letToVar acc lineNum = replaceLet (acc !! lineNum)



-- extract the parsed content, with the "let" keywords (and their position 
-- relative to their scope) that need to be changed to "var, and put them 
-- in an IO context
qt :: Either ParseError (Position, String) -> IO (Position, String)
qt s = case s of
  Left err -> print err >> do return (-1, messageString $ head (errorMessages err))
  Right s' -> 
    -- putStrLn (snd s') >> 
    return s'
  
-- extract the final version of the new wgsl script
qt' :: Position -> Either ParseError [(Position, Name)] -> IO [(Position, Name)] 
qt' scopePos s = case s of
  Left err -> print err >> do return [(-1, messageString $ head (errorMessages err))]
  Right pairs -> do 

    let q = fmap f pairs :: [(Position, Name)] -- update position to absolute
    -- putStrLn $ q >>= f'
    return q

      where f (pos, name) =  (pos + scopePos - 2, name) -- compute absolute position
            f' (pos, name) = "\n    " ++ name ++ " at " ++ show pos
