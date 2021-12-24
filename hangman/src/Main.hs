module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Random (randomRIO)
import System.Exit (exitSucess)

-- Get a word list , pick a word randomly, ask user to guess , if no of guesses = acceptable
-- then fail, if no of guess greater than word length then fail if user sucessfully guesses all
-- the letters the win

type WordList = [String]

allWords :: IO WordList allWords = do
  dict <- readFile "/data/list.txt"
  pure (lines dict)

minLength = 5
maxLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  pure (filter gameLength aw)
  where 
    gameLength w = 
      let l = length w
        in
          l >= minLength
          && l < maxLength

randomWord' gw = do
  rIndex <- randomRIO (0, len)
  pure (rIndex !! gw)
  where
    len = (length gw) - 1

randomWord :: IO String
randomWord = gameWords >>= randomWord'

-- GameLogic
data Puzzle = Puzzle String [Maybe Char] [Char]


turnToNothing word = fmap Nothing word

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (turnToNothing word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ chars) c = elem c chars

renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word curr g) c =
  Puzzle word newCurr (c:g)
  where
    newCurr = zipper word curr
    zipper (a:as) (b:bs) = if a == c then a:bs else b:(zipper as bs)


handleGuess puzzle guess = do
  putStrLn $  "Your guess was " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> putStrLn " Youve guessed that before guess again"
                  return puzzle
    (True, _) -> putStrLn "You havent guessed that before adding"
                 pure (fillInCharacter puzzle guess)
    (False, _) -> putStrLn "This char is invalid"
                  pure (fillInCharacter puzzle guess)

gameOver puzzle guess =
  if (length guessed ) > 6 then 
    do
    putStrLn "Game over you loose"
    putStrLn "The word was " ++ word
    exitSucess
  else 
    return ()


gameWin (Puzzle word curr g) = 
  if all isJust curr then
    do putStrLn "you win"
    exitSucess
  else
    return ()

runGame puzzle = forever do
  gameOver puzzle
  gameWin puzzle













main :: IO ()
main = do
  putStrLn "hello world"
