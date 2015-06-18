module Grep where
import IO
import System.Exit

a :: String -> String -> [Bool]
a l p = [ True | x <- [length l..length p], y <- [0..(length p-length l)],(l == drop y (take x p))]

b :: [Bool] -> Bool
b [] = False
b _ = True

strpresence :: String -> String -> Bool
strpresence l p = b (a l p)


findlines :: [String] -> String ->Int -> [String]
findlines s p k
    |(s == []) = []
    |strpresence p (head s) = ((show k) ++ ":" ++ (head s)) : (findlines (tail s) p (k+1))
    |otherwise = findlines (tail s) p (k+1)

prn :: [String] -> IO ()
prn (x:xs) |(xs==[]) = putStr (x++"\n")
         |otherwise = do
                        putStr (x ++ "\n")
                        prn xs

openhandler :: IOError -> IO Handle
openhandler e|isDoesNotExistError e = do 
                                  putStr ("Input file does not exist" ++ "\n")
                                  exitWith ExitSuccess
             |otherwise = ioError e                
 

mymain :: IO ()
mymain = do
          putStr "Enter search string: \n"
          srch <- getLine
          putStr "Enter file path: \n"
          path <- getLine
          handler <- catch (openFile path ReadMode) openhandler
          s <- hGetContents handler
          prn (findlines (lines s) srch 1)
          hClose handler


 
