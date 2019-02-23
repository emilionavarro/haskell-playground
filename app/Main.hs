module Main where

import Lib

main :: IO ()
main = do
    let testList = [1, 4, 7, 9]
    putStrLn "Find last elemenet in test list"
    print(getLastElement testList)
    putStrLn "Find second to last element in list"
    print(getSecondToLastElement testList)
    putStrLn "Find kth element in list"
    print(getKthElement testList 2)

getLastElement :: [a] -> a
getLastElement [] = error "No end for empty lists!"
getLastElement [x] = x
getLastElement (_:xs) = getLastElement xs

getSecondToLastElement :: [a] -> a
getSecondToLastElement [x] = error "A bigger array is necessary."
getSecondToLastElement [x,_] = x
getSecondToLastElement (_:xs) = getSecondToLastElement xs

getKthElement :: [a] -> Int -> a
getKthElement (x:_) 1 = x
getKthElement [] _ = error "Impossible"
getKthElement (_:xs) k
    | k < 1 = error "out of bounds"
    | otherwise = getKthElement xs (k-1)