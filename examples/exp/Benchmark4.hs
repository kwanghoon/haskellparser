-- Richar Bird's Book Thinking Functionall with Haskell (Chapter 10)

module Main where

import Control.Monad.ST
import Data.Array.ST

import Data.Char (toLower)
import Data.List (sort,words)
import System.IO

-- | Quick sort

qsort :: Ord a => [a] -> [a]
qsort xs = runST $
           do {xa <- newListArray (0,n-1) xs;
               qsortST xa (0,n);
               getElems xa}
           where n = length xs

qsortST :: Ord a => STArray s Int a -> 
           (Int,Int) -> ST s ()
qsortST xa (a,b)
  | a == b    = return ()
  | otherwise = do {m <- partition xa (a,b);
                    qsortST xa (a,m);
                    qsortST xa (m+1,b)}

partition :: Ord a => STArray s Int a -> 
             (Int,Int) -> ST s Int
partition xa (a,b)
 = do {x <- readArray xa a;
       let loop (j,k)
            = if j==k
              then do {swap xa a (k-1);
                       return (k-1)}
              else do {y <- readArray xa j;
                       if y < x then loop (j+1,k)
              else do {swap xa j (k-1);
                       loop (j,k-1)}}
       in loop (a+1,b)}

swap :: STArray s Int a -> Int -> Int -> ST s ()
swap xa i j =  do {v <- readArray xa i;
                   w <- readArray xa j;
                   writeArray xa i w;
                   writeArray xa j v}

-- | Hangman

hangman :: IO ()
hangman = do {xs <- readFile "Words.tex";
              play (words xs)}

play (w:ws) = do {putStrLn "I am thinking of a word:";
                  putStrLn (replicate (length w) '-');
                  putStrLn "Try and guess it.";
                  guess w ws}
guess w ws   = do {putStr "guess: ";
                  w' <- getLine;
                  if length w' /= length w
                  then do {putStrLn "Incorrect number of letters!";
                           guess w ws}
                  else if w' == w
                  then do {putStrLn "You got it!";
                           putStrLn "Play again? (yes or no)";
                           ans <- getLine;
                           if ans == "yes"
                           then play ws
                           else putStrLn "Bye!"}
                  else do {putStrLn (match w' w);
                           guess w ws}}
match w' w = map check w
   where check x = if x `elem` w' then x else '-'

-- | Common words

type Word = [Char]
type Code = (Int,Word)

sortWords :: [Word] -> [Word]
sortWords = sort

codeRuns :: [Word] -> [Code]
codeRuns []     = []
codeRuns (w:ws) = (1+length us,w):codeRuns vs
                   where (us,vs) = span (==w) ws

sortCodes :: [Code] -> [Code]
sortCodes = reverse . sort

showCode :: Code -> [Char]
showCode (n,w) = w ++": " ++ show n ++ "\n"

commonWords :: Int -> [Char] -> [Char]
commonWords n = concat . map showCode . take n . sortCodes .
                codeRuns . sortWords . words . map toLower 


cwords :: Int -> FilePath -> FilePath -> IO()
cwords n ifile ofile
     = do {text <- readFile ifile;
           writeFile ofile (commonWords n text);
           putStrLn "cwords done!"}

mainCwords :: IO ()
mainCwords 
  = do {hSetBuffering stdout NoBuffering; --- don't ask!
        putStrLn "Take text from where: ";
        ifile <- getLine;
        putStrLn "How many words: ";
        n <- getLine;
        putStrLn "Put results where: ";
        ofile <- getLine;
        text <- readFile ifile;
        writeFile ofile (commonWords (read n) text);
        putStrLn "cwords done!" }

