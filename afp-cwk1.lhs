G52AFP Coursework 1 - Connect Four Game
   
Your full name(s):
Minhao Li, Jou-Yin Huang

Your full email address(es):
scyml4@nottingham.ac.uk
scyjh2@nottingham.ac.uk

----------------------------------------------------------------------

We use some functions from the list library

> import Data.List
> import Data.Char
> import System.IO
> import Data.Char

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn ("\n" ++ unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------

The first player to go is pre-defined, so we can get the second player in turn

> firstPlayer :: Player
> firstPlayer = O
>
> secondPlayer :: Player
> secondPlayer
>   | firstPlayer == X = O
>   | otherwise = X


The following functions together do the job of getting whose turn it is

> countPlayerInRow :: Row -> Player -> Int
> countPlayerInRow [] _ = 0
> countPlayerInRow (p: ps) player
>   | p == player = 1 + countPlayerInRow ps player
>   | otherwise = countPlayerInRow ps player
>
> countPlayer :: Board -> Player -> Int
> countPlayer [] _ = 0
> countPlayer (ps: pss) p = (countPlayerInRow ps p) + (countPlayer pss p)
>
> turn :: Board -> Player
> turn b
>   | countPlayer b firstPlayer > countPlayer b secondPlayer = secondPlayer
>   | otherwise = firstPlayer


The following functions check if the given player has got a sequence indicated 
to win in the row

> hasRow :: Player -> Row -> Bool
> hasRow p [] = False
> hasRow p r | head r == p = count p (tail r) (win-1) || hasRow p (tail r) 
>            | otherwise = hasRow p (tail r) 

> count :: Player -> Row -> Int -> Bool
> count p r 0 = True
> count p [] w = False
> count p r w | head r == p = count p (tail r) (w-1)
>             | otherwise = False

> testPlayers :: Row
> testPlayers = [O, O, O, O, X, B]

> testPlayers2 :: Row
> testPlayers2 = [O, O, O, X, B, O, O, O, O]

> testPlayers3 :: Row
> testPlayers3 = [O, O, O, X, O, B, B]

> testBoard :: Board
> testBoard = [[B,B,B,B,B,B,B],
>              [B,B,B,B,B,B,B],
>              [B,B,B,B,B,B,B],
>              [B,B,B,X,X,B,B],
>              [B,B,O,O,X,B,B],
>              [B,O,O,X,X,X,O]]

The following functions intended to check if some player has won

> getRows :: Board -> [Row]
> getRows = id
>
> getCols :: Board -> [Row]
> getCols = transpose
>
> getDgnls :: Board -> [Row]
> getDgnls b
>   | length b < 2 || length (b!!0) < 2 = getDgnl b
>   | otherwise = getDgnl b ++ getDgnl (map reverse b)
>
> getDgnl :: Board -> [Row]
> getDgnl = tail . getDgnl' [] 
>   where
>       getDgnl' :: Board -> [Row] -> [Row]
>       getDgnl' rs [] = ([h | h:t <- rs]) : (transpose [t | h:t <- rs])
>       getDgnl' rs (e:es) = [h | h:_ <- rs] : getDgnl' (e:[t | _:t <- rs]) es
>
> hasWon :: Player -> Board -> Bool
> hasWon p b = any (hasRow p) (getRows b ++ getCols b ++ getDgnls b)

The following functions add the give player to the board at the indicated column 

> move :: Player -> Int -> Board -> Board
> move p c b = transpose (move' p c (transpose b))

> move' :: Player -> Int -> Board -> Board
> move' p c b = (take c b) ++ [reverse (moverow p (reverse (b !! c)))] ++ (drop (c+1) b)

> moverow :: Player -> Row -> Row
> moverow player (x:xs) | x == B = (player:xs)
>                       | otherwise = x : moverow player xs


The main functions of the game - to interact with the users and 
run the game accordingly

> initPlayer :: Player
> initPlayer = O

> initBoard :: Board
> initBoard = replicate rows (replicate cols B)

> secBoard :: Board
> secBoard = [[B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,O,B,B,B,B,B]]

> connectfour :: IO()
> connectfour = play initBoard initPlayer

> fullBoard :: Board
> fullBoard = [[X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O],
>              [X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O],
>              [X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O]]

> full :: Board -> Bool
> full b = not (any (elem B) b)

> play :: Board -> Player -> IO()
> play b p | hasWon O b = do showBoard b
>                            putStrLn "Player O has won!"
>          | hasWon X b = do showBoard b
>                            putStrLn "Player X has won!"
>          | full b = do showBoard b 
>                        putStrLn "Draw!"
>          | otherwise = do showBoard b
>                           putStrLn ("\nPlayer " ++ show p ++ " enter your move:")
>                           c <- getInt b            
>                           play (move p c b) (turn (move p c b))
>                                    


> getInt :: Board -> IO Int
> getInt b = do ns <- getLine
>               if length ns > 0 && all isDigit ns && testInput b (read ns) then 
>                  return (read ns)
>               else
>                  do putStrLn "Invalid input. Try again:"
>                     getInt b
>
> testInput :: Board -> Int -> Bool
> testInput b c = c < cols && length b > 0 && length (b!!0) > c && b!!0!!c == B

