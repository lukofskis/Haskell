import Data.Array
import System.IO
import Control.Monad
import Data.Maybe
--paima sveikąjį skaičių n ir grąžina žingsnių skaičių, reikalingą norint pasiekti 1 Collatz sekoje, pradedant nuo n.
collatzSteps :: Int -> Int
collatzSteps 1 = 0
collatzSteps n =  1 +  collatzSteps (collatz n) 

--taikome reikalinga taisykle
collatz :: Int -> Int
collatz n | even n    = n `div` 2
          | otherwise = 3 * n + 1


collatzMax :: (Int,Int) -> [(Int, Int)]
collatzMax (a, n) = map (\x -> (collatzSteps x, x)) [a .. n]

getMax :: (Int,Int) -> Int
getMax (a,n) = fst(maximum (collatzMax (a,n))) + 1

getData :: [String] -> Maybe (Int,Int)
getData [x,y] = do
                let a = read x :: Int
                let b = read y :: Int 
                return (a,b)
--Spausdiname 
printData :: [String] -> [Int] -> String
printData [] [] = ""
printData [x] [y] = (show x) ++ " " ++ (show y)
printData (x:xs) (y:ys) = 
        (show x) ++ " " ++ (show y) ++ "\n" ++ printData xs ys

--mainas iskvieciame funkcijas
main = do  
        contents <- readFile "data.txt"
        let beleka = lines contents
        let beleka2 = [words n | n <- beleka]
        let beleka3 = [getData n | n <- beleka2]
        let beleka4 = [getMax (fromMaybe (1,1) n) | n <- beleka3]
        let output = printData beleka beleka4
        putStrLn $ filter (/='"') output
      

