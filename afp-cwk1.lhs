G52AFP Coursework 1 - Connect Four Game
   
Your full name(s):
Minhao Li, Jou-Yin Huang

Your full email address(es):
scyml4@nottingham.ac.uk
scyjh2@nottingham.ac.uk

----------------------------------------------------------------------

We use some functions from the following libraries

> import Data.List
> import Data.Char
> import System.IO
> import Data.Char
> import System.IO.Unsafe
> import System.Random

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
The initial game board is defined as below

> initB :: Board
> initB = replicate rows (replicate cols B)

The first player to go is pre-defined, so the second player can be obtained 
in turn

> p1 :: Player
> p1 = O
>
> p2 :: Player
> p2 | p1 == X = O
>    | otherwise = X

The following function does the job of indicating the next player

> turn :: Board -> Player
> turn b = if firsts > seconds then p2 else p1
>           where
>               firsts = length (filter (== p1) ps)
>               seconds = length (filter (== p2) ps)
>               ps = concat b

The following functions check if the given player has got a sequence indicated 
to win in the row

> hasRow :: Player -> Row -> Bool
> hasRow = count win
>
>
> count :: Int -> Player -> Row -> Bool
> count 0 _ _ = True
> count _ _ []= False
> count w p (r:rs) | r == p = count (w - 1) p rs
>                  | otherwise = count win p rs

The following functions are intended to check if any player has won by looking 
into rows, cols and dignals from the board

> getRows :: Board -> [Row]
> getRows = id
>
> getCols :: Board -> [Row]
> getCols = transpose
>
> getDgnls :: Board -> [Row]
> getDgnls b = getDgnl b ++ getDgnl (reverse b)
>           where getDgnl = tail .getDgnl' []
> 
> getDgnl' :: Board -> [Row] -> [Row]
> getDgnl' b rs
>   | length rs == 0 = hs : (transpose ts)
>   | otherwise = hs : getDgnl' (h:ts) t
>       where
>           hs = [h | h:t <- b]
>           ts = [t | h:t <- b]
>           h = head rs
>           t = tail rs
>
> hasWon :: Player -> Board -> Bool
> hasWon p b = any (hasRow p) (getRows b) 
>           || any (hasRow p) (getCols b) 
>           || any (hasRow p) (getDgnls b)

The funciton full checks if the board given has no move blank cell

> full :: Board -> Bool
> full b = not (any (elem B) b)

The following functions add the given player to the board at the indicated 
column 

> move :: Player -> Int -> Board -> Board
> move p c b = transpose (h ++ [r] ++ tail t)
>                where 
>                    r = reverse (moveR p (reverse (tb !! c)))
>                    (h, t) = splitAt c tb
>                    tb = transpose b

>
> moveR :: Player -> Row -> Row
> moveR p (x:xs) | x == B = (p:xs)
>                | otherwise = x : moveR p xs
>

The functions below checks if the given column numbers to move if valid
- a valid digit indicating the column number that is within the defined 
board size and still has at least one empty cell

> getInt :: Board -> IO Int
> getInt b = do ns <- getLine
>               if not (null ns) && all isDigit ns && valid b (read ns) then 
>                  return (read ns)
>               else
>                  do putStrLn "Invalid input. Try again:"
>                     getInt b
>
> valid :: Board -> Int -> Bool
<<<<<<< HEAD
> valid b c = c < cols && rows > 0 && cols > c && b!!0!!c == B
=======
> valid b c = c < cols && b!!0!!c == B
>>>>>>> ac022bb60cd1cf4a809ec3f2a52e8c57576e1a41

Game tree is defined as below with the pre-defined size 

> data Tree x = Node x [Tree x] deriving Show
>
> gameTree :: Player -> Board -> Tree (Board, Player)
> gameTree = gameTree' 0
>
> gameTree' :: Int -> Player -> Board -> Tree (Board, Player)
> gameTree' d p b 
>   | hasWon p1 b = Node (b, p1) []
>   | hasWon p2 b = Node (b, p2) []
>   | d >= depth || full b = Node (b, B) []
>   | otherwise = Node (b, minimax) st
>                   where 
>                       minimax = (if turn b == O then minimum else maximum) ps
>                       ps = [p | Node (_, p) _ <- st]
>                       st = [gameTree' (d + 1) p' b' | b' <- bs]
>                       bs = [f b | f <- map (move p) ms]
>                       ms = [c | c <- (filter (valid b) [0..cols - 1]) ]
>                       p' = if p == p1 then p2 else p1

The following functions decide the next and best step the computer to move. A 
random selector is applied if multiple equally good steps exist

> nodeBoard :: Tree (Board, Player) -> Board
> nodeBoard (Node (b, _) _) = b
>
> nodePlayer :: Tree (Board, Player) -> Player
> nodePlayer (Node (_, p) _) = p
>
>
> nextMove :: Player -> Board -> Board
> nextMove p = bestMove . gameTree p
>
> bestMove :: Tree (Board, Player) -> Board
> bestMove (Node (b, p) ns) | lenXMoves > 0 = xMoves !! randomNum lenXMoves
>                           | lenBMoves > 0 = bMoves !! randomNum lenBMoves
>                           | otherwise = nodeBoard (ns !! (randomNum lenNs))
>                                where 
>                                    xMoves = getMoves X ns
>                                    bMoves = getMoves B ns
>                                    lenBMoves = length bMoves
>                                    lenXMoves = length xMoves
>                                    lenNs = length ns	
>
> getMoves :: Player -> [Tree (Board, Player)] -> [Board]
> getMoves p ns =  [nodeBoard n | n <- ns, nodePlayer n == p]
>
> randomNum :: Int -> Int
> randomNum n = unsafePerformIO (randomRIO (0,n-1))


The main functions of the game - to interact with the users and 
update the game status accordingly

> main :: IO()
> main = play initB p1
>
> play :: Board -> Player -> IO()
> play b p = do showBoard b
>               play' b p
>
> play' :: Board -> Player -> IO()
> play' b p | hasWon O b = putStrLn "Player O has won!"
>           | hasWon X b = putStrLn "Player X has won!"
>           | full b = putStrLn "Draw!"
>           | p == X = do putStrLn ("Player X is thinking...")           
>                         play (nextMove p b) O                       
>           | otherwise = do putStrLn ("\nPlayer O enter your move:")
>                            c <- getInt b
>                            play (move p c b) X

