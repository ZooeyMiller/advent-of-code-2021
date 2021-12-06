import Control.Monad
import Data.List

main = do
  let samplePath = "./inputs/Day4Sample.txt"
      fullPath = "./inputs/Day4.txt"
  (numbers, boards) <- parseInput fullPath
  print $ solvePartOne numbers boards

parseInput path = do
  file <- readFile path
  let inLines = lines file
      numbers = head inLines
      boards = tail inLines
      numbersArr = split (== ',') numbers
      boardsArr = split (== "") boards
      boardsSplit = fmap (Board . fmap (fmap (\x -> (x, False)) . split (== ' '))) boardsArr
  return (numbersArr, boardsSplit)

data Board = Board [[(String, Bool)]] deriving (Eq, Show)

split :: (a -> Bool) -> [a] -> [[a]]
split p s = case dropWhile p s of
  [] -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

boardWins :: Board -> Bool
boardWins (Board board) = rowWins matches || rowWins columns
  where
    matches = fmap (fmap snd) board
    rowWins [] = False
    rowWins (row : rows) = if and row then True else rowWins rows
    columns = transpose matches

markBoard :: String -> Board -> Board
markBoard str (Board board) = Board $ fmap (fmap f) board
  where
    f x = if fst x == str then (fst x, True) else x

markAllBoards :: String -> [Board] -> [Board]
markAllBoards str = fmap (markBoard str)

markAndCheckAll :: String -> [Board] -> ([Board], Maybe Board)
markAndCheckAll str boards = (marked, find boardWins marked)
  where
    marked = markAllBoards str boards

getFirstWinningBoard :: [String] -> [Board] -> Maybe (String, Board)
getFirstWinningBoard (str : strs) boards = next
  where
    (rest, res) = markAndCheckAll str boards
    next = case res of
      Just board -> Just (str, board)
      Nothing -> getFirstWinningBoard strs rest
getFirstWinningBoard [] _ = Nothing

getResFromBoard :: String -> Board -> Int
getResFromBoard str (Board board) = read str * sum numbers
  where
    flat = join board
    unmarked = filter (not . snd) flat
    numbers = fmap (read . fst) unmarked

solvePartOne :: [String] -> [Board] -> Maybe Int
solvePartOne strs boards = res
  where
    winner = getFirstWinningBoard strs boards
    res = fmap (uncurry getResFromBoard) winner
