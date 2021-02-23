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
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
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
> hasRow player [] = False
> hasRow player row | head row == player = countSeqPlayerInRow player (tail row) (win-1) || hasRow player (tail row) 
>                   | otherwise = hasRow player (tail row) 

> testPlayers :: Row
> testPlayers = [O, O, O, O, X, B]

> testPlayers2 :: Row
> testPlayers2 = [O, O, O, X, B, O, O, O, O]

> testPlayers3 :: Row
> testPlayers3 = [O, O, O, X, O, B, B]

> countSeqPlayerInRow :: Player -> Row -> Int -> Bool
> countSeqPlayerInRow player row 0 = True
> countSeqPlayerInRow player [] win = False
> countSeqPlayerInRow player row win | head row == player = countSeqPlayerInRow player (tail row) (win-1)
>                                    | otherwise = False

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
> move player colNum board = myTranspose (addPlayerToBoard player colNum (myTranspose board))

> addPlayerToBoard :: Player -> Int -> Board -> Board
> addPlayerToBoard player colNum board = (take colNum board) ++ [reverse (addPlayerToRow player (reverse (board !! colNum)))] ++ (drop (colNum+1) board)

> addPlayerToRow :: Player -> Row -> Row
> addPlayerToRow player (x:xs) | x == B = (player:xs)
>                              | otherwise = x : addPlayerToRow player xs

> myTranspose :: Board -> [Row]
> myTranspose [] = []
> myTranspose board | head board == [] = []
>                   | otherwise = heads board : myTranspose (removeHeads board)

> heads :: Board -> Row 
> heads [] = []
> heads (x:xs) | x == [] = []
>              | otherwise = head x : heads xs

> removeHeads :: Board -> [Row]
> removeHeads [] = []
> removeHeads (x:xs) | x == [] = []
>                    | otherwise = tail x : removeHeads xs



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

> connectFour :: IO()
> connectFour = play initBoard initPlayer

> play :: Board -> Player -> IO()
> play board player | hasWon O board = do putStr("\n")
>                                         showBoard board 
>                                         putStrLn "Player O has won!"
>                   | hasWon X board = do putStr("\n")
>                                         showBoard board 
>                                         putStrLn "Player X has won!"
>                   | otherwise = do putStr("\n")
>                                    showBoard board
>                                    putStrLn ("\nPlayer " ++ show player ++ " enter your move:")
>                                    colInput <- getInt                
>                                    play (move player colInput board) (turn (move player colInput board))
>                                       

> getInt :: IO Int
> getInt = do charInput <- getChar
>             if (isDigit charInput) then 
>                return (read [charInput])
>             else 
>                return 0
