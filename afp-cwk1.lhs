G52AFP Coursework 1 - Connect Four Game
   
Your full name(s):
Minhao Li, Jou-Yin Huang

Your full email address(es):
scyml4@nottingham.ac.uk
scyjh2@nottingham.ac.uk

----------------------------------------------------------------------

We use some functions from the following libraries including System.Random.

> import Data.List
> import Data.Char
> import System.IO
> import Data.Char
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
initB: the initial game board which is entirely empty. 

> initB :: Board
> initB = replicate rows (replicate cols B)

p1: the first player to go is pre-defined.

> p1 :: Player
> p1 = O

p2: the second player to go can be deduced from who goes first.

> p2 :: Player
> p2 | p1 == X = O
>    | otherwise = X

turn: the function refers the next player from the specified current board 
      layout.

> turn :: Board -> Player
> turn b = if firsts > seconds then p2 else p1
>           where
>               firsts = length (filter (== p1) ps)
>               seconds = length (filter (== p2) ps)
>               ps = concat b

hasRow: to count if if the given player has got a sequence indicated to win in
         the given row.

> hasRow :: Player -> Row -> Bool
> hasRow = count win

count: take a counter, w, the player to count and the row to count from as
        parameters. The counter will decrement when the current cell contains
        the indicated player. If not then the counter will be reseted to the
        pre-defined variable win to start over the counting. The counting is 
        acchieved by recursion. If the counter reaches 0 meaning that the 
        sequence is found. In each recursion step the input row will be tailed.
        If the row becomes empty before the counter reaches 0 then the sequence
        is not found. 

> count :: Int -> Player -> Row -> Bool
> count 0 _ _ = True
> count _ _ []= False
> count w p (r:rs) | r == p = count (w - 1) p rs
>                  | otherwise = count win p rs

getRow: gets all the rows of the given board.

> getRows :: Board -> [Row]
> getRows = id

getCols: gets all the columns of the given board.

> getCols :: Board -> [Row]
> getCols = transpose

getDgnls: gets all the diagnals of the given board.

> getDgnls :: Board -> [Row]
> getDgnls b = getDgnl b ++ getDgnl (reverse b)
>           where getDgnl = tail . getDgnl' []

getDgnl': Gets the diagnals from top-right to bottom-left of a board.

> getDgnl' :: Board -> [Row] -> [Row]
> getDgnl' b rs
>   | null rs = hs : (transpose ts)
>   | otherwise = hs : getDgnl' (h:ts) t
>       where
>           hs = [h | h:t <- b]
>           ts = [t | h:t <- b]
>           h = head rs
>           t = tail rs

hasWon: the function checks if the specified player has won in the specified
        board. Specifically. The play wins if it has a certain number (defined 
        by win) of consecutive cells in any of the rows, columns, or diagnals.

> hasWon :: Player -> Board -> Bool
> hasWon p b = any (hasRow p) (getRows b) 
>           || any (hasRow p) (getCols b) 
>           || any (hasRow p) (getDgnls b)

full: the funciton checks if the board given has no more blank cells.

> full :: Board -> Bool
> full b = not (any (elem B) b)

move : the function add the given player to the board at the indicated column 
       by 1. getting the columns from the given board (acchieved by getCols), 
          2. extracting the specified column, 
          3. reversing the column to add player from the button 
          4. reversing the column back
          5. appending the columns back to the board
          6. getting the columns to transpose the board back

> move :: Player -> Int -> Board -> Board
> move p c b = getCols (h ++ [r] ++ tail t)
>                where 
>                    r = reverse (moveR p (reverse (tb !! c)))
>                    (h, t) = splitAt c tb
>                    tb = getCols b

moveR: the function looks for the first non-empty cell to insert the given 
        player by recursion through the row given, and then outputs the 
        processes row.

> moveR :: Player -> Row -> Row
> moveR p (x:xs) | x == B = (p:xs)
>                | otherwise = x : moveR p xs

getInt: the function checks if the given column numbers to move is not empty,
         containing only digits and valid according to the given board.  

> getInt :: Board -> IO Int
> getInt b = do ns <- getLine
>               if not (null ns) && all isDigit ns && valid b (read ns) then 
>                  return (read ns)
>               else
>                  do putStrLn "Invalid input. Try again:"
>                     getInt b

valid: this function checks if the given column number is valid - a valid
        column from the given board that is within the defined board size 
        and still has at least one empty cell.

> valid :: Board -> Int -> Bool
> valid b c = c < cols && b!!0!!c == B

Tree: the tree data structure that stores the game boards in different stages.

> data Tree x = Node x [Tree x] deriving Show

gameTree: the function uses auxiliary functions labelTree and boardTree to
          generate a tree of boards firstly and a tree of boards with a minimax
          label at each node.

> gameTree :: Player -> Board -> Tree (Board, Player)
> gameTree p = labelTree . boardTree 0 p

boardTree: the function generates a tree of boards from the specified current
           board and current player to go, and this tree has a maximum depth of
           the pre-defined value (it stops stretching when the board is full).

> boardTree :: Int -> Player -> Board -> Tree Board
> boardTree d p b 
>   | d >= depth || full b = Node b []
>   | otherwise = Node b [boardTree (d+1) p' b' | b' <- bs]
>       where
>           bs = [f b | f <- map (move p) ms]
>           ms = [c | c <- (filter (valid b) [0..cols-1])]
>           p' = if p == p1 then p2 else p1

labelTree: the function puts down a label on each node of the specified tree of
           boards.

> labelTree :: Tree Board -> Tree (Board, Player)
> labelTree (Node b []) = Node (b, p) []
>       where p = if hasWon p1 b then p1 else
>                 if hasWon p2 b then p2 else B
> labelTree (Node b st) = Node (b, p) st'
>       where
>           st' = map labelTree st
>           p = (if turn b == O then minimum else maximum) ps
>           ps = [p | Node (_, p) _ <- st']


nodeBoard: this function take a tree and outputs the board at the root node

> nodeBoard :: Tree (Board, Player) -> Board
> nodeBoard (Node (b, _) _) = b

nodePlayer: this function take a tree and outputs the player at the root node

> nodePlayer :: Tree (Board, Player) -> Player
> nodePlayer (Node (_, p) _) = p

nodeBoard: this function applies the helper function nextMoves' to generate all
           equally best moves in the form of list of boards according to the 
           given player. 

> nextMoves :: Player -> Board -> [Board]
> nextMoves p = nextMoves' . gameTree p

nextMoves': this function outputs the list of equally-best next moves in a form
            of [Board] by filtering out all the direct children of the current 
            nodes whose player is X. If the result is null then looks for nodes
            with player == B; otherwise return all the children. 

> nextMoves' :: Tree (Board, Player) -> [Board]
> nextMoves' (Node (b, p) ns) | not (null xMoves) = xMoves
>                             | not (null bMoves) = bMoves
>                             | otherwise = [nodeBoard n | n <- ns]
>                                where 
>                                    xMoves = getMoves X ns
>                                    bMoves = getMoves B ns

getMoves: this functions return all the nodes whose player is as given from the
          input tree. 

> getMoves :: Player -> [Tree (Board, Player)] -> [Board]
> getMoves p ns =  [nodeBoard n | n <- ns, nodePlayer n == p]


main: the main function of the game - to call the function play with initial
      game board initB and the first player p1. 

> main :: IO()
> main = play initB p1

play: the function that displays the board and updates the game status by 
      calling the helper function play'.

> play :: Board -> Player -> IO()
> play b p = do showBoard b
>               play' b p

play': the helper function of play to check the status of the current game board
       and displays a proper message if any of the players has won. If not then
       it will check if the game board is full - meaning that the status is 
       draw. Otherwise the function will check who is the player and seek for 
       the next move from the next player (either the human player or the 
       computer).  

> play' :: Board -> Player -> IO()
> play' b p | hasWon O b = putStrLn "Player O has won!"
>           | hasWon X b = putStrLn "Player X has won!"
>           | full b = putStrLn "Draw!"
>           | p == X = do putStrLn ("Player X is thinking...")  
>                         r <- randomRIO (0, length moves - 1)         
>                         play (moves !! r) O                       
>           | otherwise = do putStrLn ("\nPlayer O enter your move:")
>                            c <- getInt b
>                            play (move p c b) X
>                              where moves = nextMoves p b

