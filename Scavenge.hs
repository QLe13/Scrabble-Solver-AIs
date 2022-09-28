module Scavenge where
import Dictionaries
import Data.List (sort)
import Debug.Trace
import Data.List (delete)


type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String] 


-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://scrabble.hasbro.com/en-us/faq
charScore :: Char -> Integer
charScore char 
    | (char =='A'||char =='E'||char == 'I'||char == 'O'|| char =='U'||char == 'L'||char == 'N'||char == 'S'||char == 'T'||char == 'R') = 1
    | (char =='D'||char =='G') = 2
    | (char == 'B'||char== 'C'||char == 'M'||char== 'P') = 3
    | (char=='F'|| char == 'H'||char == 'V'|| char== 'W'||char == 'Y' )= 4
    | (char == 'K') = 5
    | (char == 'J' || char == 'X') = 8
    | (char == 'Q' || char == 'Z') = 10
    | otherwise = error "Invalid input"

score :: Move -> Integer
score [] = 0
score move = sum[charScore x | x<-move]
-- score "QA" == 11
-- score "JF" == 12

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay play = sum[score x | x<-play ]
-- scorePlay ["KB", "QA"] == 19 


remove :: Eq a => a -> [a] -> [a]   
remove num []=error "no elem"
remove num (x:xs)
    | num == x = xs
    | otherwise = x:remove num xs
-- remove 7 [7,3,1,7,5] = [3,1,7,5] 


updateHand :: Hand -> Move -> Hand
updateHand hand [] = hand
updateHand hand (x:xs) = updateHand (remove x hand) xs
-- updateHand "HELLO" "LO" = "HEL"

-- canMake takes a hand and a move, and tells you if that move can be made with that hand.
canMake :: Hand -> Move -> Bool
canMake hand move = helper (sort hand) (sort move)
            where helper (x:xs) (y:ys) 
                    | x == y = helper xs ys
                    | x > y = False
                    | otherwise = helper xs (y:ys)
                  helper hand [] = True
                  helper [] move = False
-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dict hand move = if((elem move dict)==False) then False else (canMake hand move)
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE

-- isValidPlay checks if a play is valid. 
isValidPlay :: Dictionary -> Hand -> Play -> Bool

isValidPlay dict hand [] = True
isValidPlay dict hand (x:xs) = if (isValidMove dict hand x) then (isValidPlay dict (updateHand hand x) xs) else False 
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False
    
-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves dict hand = [x | x <- dict , canMake hand x]


-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]

 
-- powerset: return the powerset of the input, i.e. the list of all sub-lists.

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
                let theShort = powerset xs
                in map (x:) (theShort) ++ theShort
        

maxPlay :: [Play] -> Play
maxPlay lst = snd (sort lst)
    where sort [x] = (scorePlay x, x)
          sort [] = error "Empty"
          sort (x:xs) = max (scorePlay x, x) (sort xs)
naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dict hand = maxPlay[x | x<- powerset dict, length(concat x)<=length hand, isValidPlay dict hand x]
                        
    

smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dict hand = naiveBrutePlay (validMoves dict hand) hand


-- --- Greedy Algorithm

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.
maxTup :: Ord a => [(t, a)] -> (t, a)
maxTup []     = error "maximum of empty list"
maxTup (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dict hand = 
  let moves = validMoves dict hand
      max = fst (maxTup [(x, score x)| x <- moves])
      handleft = updateHand hand max
  in if (null moves) then [] else max : greedyPlay dict (updateHand hand max)


-- --- Recursive Game-Tree Algorithm: Dynamically created to solve the best play efficiently
bestPlay:: Dictionary -> Hand -> Play 
bestPlay dict hand = if (valids == []) then [] else maxPlay [plays moves | moves <- valids]
    where valids = validMoves dict hand
          plays play = let
                          handleft = updateHand hand play
                          dictUp = [x | x <- valids, length x >= length play]
                        in play:(bestPlay dictUp handleft)
          










