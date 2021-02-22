G52AFP Coursework 1 - Connect Four Game
   
Your full name(s):
Minhao Li, Jou-Yin Huang

Your full email address(es):
scyml4@nottingham.ac.uk
scyjh2@nottingham.ac.uk

----------------------------------------------------------------------

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
> countPlayerInRow (p: ps) player = (compare p player) + (countPlayerInRow ps p)
>   where
>       compare :: Player -> Player -> Int
>       compare p1 p2
>           | p1 == p2 = 1
>           | otherwise = 0
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
