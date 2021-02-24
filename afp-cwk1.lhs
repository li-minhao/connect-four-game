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

The first player to go is pre-defined, so we can get the second player in turn

> p1 :: Player
> p1 = O
>
> p2 :: Player
> p2 | p1 == X = O
>    | otherwise = X

The following function does the job of getting whose turn it is

> turn :: Board -> Player
> turn b = if firsts > seconds then p2 else p1
>           where
>               firsts = length (filter (== p1) ps)
>               seconds = length (filter (== p2) ps)
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

The following functions intended to check if some player has won

> getRows :: Board -> [Row]
> getRows = id
>
> getCols :: Board -> [Row]
> getCols = transpose
>
> getDgnls :: Board -> [Row]
> getDgnls = tail . getDgnl' [] . (\b -> b ++ map reverse b)
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
> move p c b = transpose ((take c tb) ++ [r] ++ (drop (c+1) tb))
>                where 
>                    r = reverse (moveR p (reverse (tb !! c)))
>                    tb = transpose b

> moveR :: Player -> Row -> Row
> moveR p (x:xs) | x == B = (p:xs)
>                | otherwise = x : moveR p xs


The main functions of the game - to interact with the users and 
run the game accordingly

> initB :: Board
> initB = replicate rows (replicate cols B)

> main :: IO()
> main = do showBoard initB
>           play initB p1 (Node (initB, p1) [])

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
              
> getInt :: Board -> IO Int
> getInt b = do ns <- getLine
>               if length ns > 0 && all isDigit ns && valid b (read ns) then 
>                  return (read ns)
>               else
>                  do putStrLn "Invalid input. Try again:"
>                     getInt b
>
> valid :: Board -> Int -> Bool
> valid b c = c < cols && length b > 0 && length (b!!0) > c && b!!0!!c == B

Game tree is defined as below

> data Tree x = Node x [Tree x] deriving Show
>
> gameTree :: Board -> Player -> Tree (Board, Player)
> gameTree = gameTree' 0
>
> gameTree' :: Int -> Board -> Player -> Tree (Board, Player)
> gameTree' d b p 
>   | hasWon p1 b = Node (b, p1) []
>   | hasWon p2 b = Node (b, p2) []
>   | d >= depth || full b = Node (b, B) []
>   | otherwise = Node (b, minimax) st
>                   where 
>                       minimax = (if turn b == O then minimum else maximum) ps
>                       ps = [p | Node (_, p) _ <- st]
>                       st = [gameTree' (d + 1) b' p' | b' <- bs]
>                       bs = [f b | f <- map (move p) ms]
>                       ms = [c | c <- (filter (valid b) [0..cols - 1]) ]
>                       p' = if p == p1 then p2 else p1

The following functions decide the next step to move 

> nodeBoard :: Tree (Board, Player) -> Board
> nodeBoard (Node (b, _) _) = b
>
> nodePlayer :: Tree (Board, Player) -> Player
> nodePlayer (Node (_, p) _) = p
>
> nextMove :: Tree (Board, Player) -> Board
> nextMove (Node (b, p) ns) | lenXMoves > 0 = xMoves !! randomNum lenXMoves
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
