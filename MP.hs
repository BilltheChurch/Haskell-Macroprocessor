module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp input items = [y | (x, y) <- items, x == input]
  

splitText :: [Char] -> String -> (String, [String])
splitText _ ""
  = ("", [""])
splitText sps (c : cs)
  | c `elem` sps = (c : ts, words)
  | otherwise    = (ts, (c : w) : ws)
  where
    (ts, words@(w : ws)) = splitText sps cs



combine :: String -> [String] -> [String]
combine "" x = x
combine (a:as) (b:bs) = b : head [[a]: b']
  where
    b' = combine as bs
  

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (f : fs) = (k, unwords kd) : kwds
  where 
    (_, k:kd) = splitText " \n" f
    kwds = getKeywordDefs fs

expand :: FileContents -> FileContents -> FileContents
expand ""_ = ""
expand file def = foldr1 (++) ([replaceWord p defs | p <- q])
  where
    (_, def') = splitText "\n" def
    (m , n)   = splitText separators file
    q         = combine m n 
    defs      = getKeywordDefs def'

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord str [] = str
replaceWord str (c : cs)
  | str == a = b
  | otherwise = replaceWord str cs
  where
    (a,b) = c

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")