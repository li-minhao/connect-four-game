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
> depth = 1

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


The following function does the job of getting whose turn it is

> turn :: Board -> Player
> turn b = if firsts > seconds then secondPlayer else firstPlayer
>           where
>               firsts = length (filter (== firstPlayer) ps)
>               seconds = length (filter (== secondPlayer) ps)
>               ps = concat b


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

> initBoard :: Board
> initBoard = replicate rows (replicate cols B)

> secBoard :: Board
> secBoard = [[B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,B,B,B,B,B,B],
>             [B,O,B,B,B,B,B]]

> main :: IO()
> main = do showBoard initBoard
>           play initBoard firstPlayer (Node (initBoard, firstPlayer) [])

> fullBoard :: Board
> fullBoard = [[X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O],
>              [X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O],
>              [X,O,X,O,X,O,X],
>              [X,O,O,X,O,X,O]]


> oWinBoard :: Board
> oWinBoard = [[B,B,B,B,B,B,B],
>              [B,B,B,B,B,B,B],
>              [B,B,B,B,B,B,B],
>              [X,O,B,B,B,B,B],
>              [X,O,B,B,B,B,B],
>              [X,O,B,B,B,B,B]]



> full :: Board -> Bool
> full b = not (any (elem B) b)

> play :: Board -> Player -> Tree (Board, Player) -> IO()
> play b p t | hasWon O b = putStrLn "Player O has won!"
>            | hasWon X b = putStrLn "Player X has won!"
>            | full b = putStrLn "Draw!"
>            | p == X = do putStrLn ("Player X is thinking...") 
>                          showBoard nextB            
>                          play nextB O t                           
>            | otherwise = do putStrLn ("\nPlayer O enter your move:")
>                             c <- getInt b
>                             showBoard (move p c b) 
>                             play (move p c b) X (gameTree (move p c b) X) 
>                               where 
>                                   nextB = (nextMove t)


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

Game tree is defined below

> data Tree x = Node x [Tree x] deriving Show
>
> gameTree :: Board -> Player -> Tree (Board, Player)
> gameTree = gameTree' 0
>
> gameTree' :: Int -> Board -> Player -> Tree (Board, Player)
> gameTree' d b p 
>   | hasWon firstPlayer b = Node (b, firstPlayer) []
>   | hasWon secondPlayer b = Node (b, secondPlayer) []
>   | d >= depth || full b = Node (b, B) []
>   | otherwise = Node (b, minimax) st
>                   where 
>                       minimax = (if turn b == O then minimum else maximum) ps
>                       ps = [p | Node (_, p) _ <- st]
>                       st = [gameTree' (d + 1) b' p' | b' <- bs]
>                       bs = [f b | f <- map (move p) ms]
>                       ms = [c | c <- (filter (testInput b) [0..cols - 1]) ]
>                       p' = if p == firstPlayer then secondPlayer else firstPlayer


The following functions decide the next step to move 

> testTree :: Tree (Board, Player)
> testTree = (Node (initBoard, O) [Node (initBoard, O) [Node (initBoard, O) []], Node (secBoard, X) [], Node (initBoard, X) []])
>
> testTrees :: [Tree (Board, Player)]
> testTrees = [Node (initBoard, O) [], Node (secBoard, X) []]
>
> nodeBoard :: Tree (Board, Player) -> Board
> nodeBoard (Node (b, _) _) = b
>
> nodePlayer :: Tree (Board, Player) -> Player
> nodePlayer (Node (_, p) _) = p
>
> nextMove :: Tree (Board, Player) -> Board
> nextMove (Node (b, p) ns) | lenMoves == 0 = nodeBoard (ns !! (randomNum lenNs))
>                           | otherwise = moves !! randomNum lenMoves
>                                where 
>                                    moves = getMoves ns
>                                    lenMoves = length moves
>                                    lenNs = length ns
>
> getMoves :: [Tree (Board, Player)] -> [Board]
> getMoves ns =  [nodeBoard n | n <- ns, nodePlayer n == X]
>
> randomNum :: Int -> Int
> randomNum n = unsafePerformIO (randomRIO (0,n-1))
