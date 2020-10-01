import Data.Maybe

--Library providing data types and functions to support a text-based game of tic tac toe. some of the data types and functions are designed in a general manner so that this library could be extended to similar games such as connect four.
--Symbol that a player can place in a square. only those needed for tic-tac-toe are included but this could be extended
data Symbol = X | O deriving (Eq,Show)
--represents one space in the board for a game
data Square = Blank | Taken Symbol deriving (Eq)
--a rectangular board that can be used for a variety of simple games
data Board = Board Int Int [Square] deriving (Eq)
--a player may be Human or AI, and has a symbol that they use in a game
data Player = Human Symbol | AI Symbol deriving (Show)
--coordinates within a game board. (x,y) represents the xth row, yth column: axes are switched from standard math notation
type Coords = (Int,Int)
--all 8 directions you could go in a 2d grid
data Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest
--one step in a given direction from given coordinates.
move :: Direction -> Coords -> Coords
move East (x,y) = (x,y+1)
move West (x,y) = (x,y-1)
move North (x,y) = (x-1,y)
move South (x,y) = (x+1,y)
move Northwest (x,y) = (x-1,y-1)
move Northeast (x,y) = (x-1,y+1)
move Southwest (x,y) = (x+1,y-1)
move Southeast (x,y) = (x+1,y+1)

--data constructor for a generic game board, takes number of rows, number of columns, and default value of each square
constructBoard :: Int -> Int -> Square -> Board
constructBoard row col startSquare = Board row col (take (row*col) (repeat startSquare))

--a tic tac toe board is 3*3 and all squares are initially blank
ticTacToeBoard :: Board
ticTacToeBoard = constructBoard 3 3 Blank

--display a board with rows separated by horizontal lines
instance Show (Board) where
  show (Board row col squares) = concat (zipWith (++) (init rowStrs) (repeat separator)) ++ (last rowStrs) ++ "\n"
    where
      rowStrs = map (showRow) (getRows (Board row col squares))
      separator = "\n" ++ (take (2*col-1) (repeat '-')) ++ "\n"

--Blank square should be a space, otherwise display the square's symbol
instance Show (Square) where
  show Blank = " "
  show (Taken s) = show s

instance Enum (Symbol) where
  succ X = O
  succ O = X
  toEnum 0 = X
  toEnum 1 = O
  fromEnum X = 0
  fromEnum O = 1

--display a row of squares with vertical bars separating them
showRow :: [Square]->String
showRow rs = init (concatMap ((++ "|").(show)) rs)

--returns the Board created by placing the specified symbol at the specified location
setSquare :: Board -> Int -> Square -> Board
setSquare (Board row col squares) i sq
--if a move is out of bounds, it just does nothing
  |(notElem i [0..(row * col - 1)]) = Board row col squares
  | otherwise = Board row col (pre ++ (sq:post))
  where
    pre = fst (splitAt i squares)
    post = drop 1 (snd (splitAt i squares))

--set a square within the board based on a pair of coordinates. coordinates should be in ranges (1..row,1..col)
setSquareCoords :: Board -> Coords -> Square -> Board
setSquareCoords (Board row col squares) (x,y) sq
--invalid coordinates have no effect
  |((notElem x [1..row]) || (notElem y [1..col])) = (Board row col squares)
  |otherwise = setSquare (Board row col squares) ((x-1) * col + (y-1)) sq

--retrieve the square at a particular index
getSquare :: Board -> Int -> Maybe Square
getSquare (Board row col squares) i
  |(notElem i [0..(row * col - 1)]) = Nothing
  |otherwise = Just (squares !! i)

--retrieve the square at given coordinates. coordinates in ranges (1..row,1..col)
getSquareCoords :: Board -> Coords -> Maybe Square
getSquareCoords (Board row col squares) (x,y)
  |((notElem x [1..row]) || (notElem y [1..col])) = Nothing
  |otherwise = getSquare (Board row col squares) ((x-1) * col + (y-1))

--if all squares in the given list are Taken with the same symbol, returns True and that symbol. Otherwise, returns False and Nothing.
squaresWon :: [Square] -> (Bool,Maybe Symbol)
squaresWon [] = (False, Nothing)
squaresWon (Blank:sqs) = (False, Nothing)
squaresWon ((Taken sym):sqs)
  |and (map (==(Taken sym)) sqs) = (True, Just sym)
  |otherwise = (False, Nothing)

--starts at given coords within a board, and moves in given direction a given number of times or until the edge of the board is reached, whichever comes first.
squaresOnPath :: Board -> Coords -> Direction -> Int -> [Square]
squaresOnPath brd loc dir n
  |(n == 0) = []
  |otherwise = case (getSquareCoords brd loc) of
    (Just sq) -> sq : squaresOnPath brd (move dir loc) dir (n-1)
    Nothing -> []

--the major diagonal of a square board, one way to win tic-tac-toe
majorDiagonal :: Board -> [Square]
majorDiagonal (Board row col squares) = squaresOnPath (Board row col squares) (1,1) Southeast (min row col)

--the minor diagonal of a square board, one way to win tic-tac-toe
minorDiagonal :: Board -> [Square]
minorDiagonal (Board row col squares) = squaresOnPath (Board row col squares) (1,col) Southwest (min row col)

--gets all the horizonal rows of a board
getRows :: Board -> [[Square]]
getRows (Board row col squares) = [squaresOnPath (Board row col squares) (r,1) East col | r<-[1..row]]

--gets all the vertical columns of a board
getCols :: Board -> [[Square]]
getCols (Board row col squares) = [squaresOnPath (Board row col squares) (1,c) South row | c<-[1..col]]

--check if any row is won
rowWin :: Board -> (Bool, Maybe Symbol)
rowWin brd
  |(won == []) = (False, Nothing)
  |otherwise = head won
  where won = filter fst (map (squaresWon) (getRows brd))

--check if any column is won
colWin :: Board -> (Bool, Maybe Symbol)
colWin brd
  |(won == []) = (False, Nothing)
  |otherwise = head won
  where won = filter fst (map (squaresWon) (getCols brd))

--check if either main diagonal is won
diagonalWin :: Board -> (Bool, Maybe Symbol)
diagonalWin brd
  |(won == []) = (False, Nothing)
  |otherwise = head won
  where won = filter fst [squaresWon (majorDiagonal brd),squaresWon (minorDiagonal brd)]

--check if any row, column, or diagonal is won
ticTacToeWin :: Board -> (Bool, Maybe Symbol)
ticTacToeWin brd
  |(fst (rowWin brd)) = rowWin brd
  |(fst (colWin brd)) = colWin brd
  |(fst (diagonalWin brd)) = diagonalWin brd
  |otherwise = (False, Nothing)

--check if a board has any blank squares left. if this is false but no winner is found, the game is a tie
boardFull :: Board -> Bool
boardFull (Board _ _ sqs) = and (map (/=Blank) sqs)

--check whether a player is human or AI
isHuman :: Player -> Bool
isHuman (Human s) = True
isHuman (AI s) = False

--call the appropriate function to get a move from either a human player or AI
getMove :: Board -> Player -> IO Board
getMove brd (Human sym) = getHumanMove brd sym
getMove brd (AI sym) = computerTicTacToeMove brd sym

--get a move from a human player via standard input. repeat until they enter a valid move
getHumanMove :: Board -> Symbol -> IO Board
getHumanMove brd sym = do
  putStrLn ("Enter coordinates of your move as an ordered pair: (row,col)")
  nextLine <- getLine
  let coords = (read nextLine :: Coords)
  if (isLegalMove brd coords) then return (setSquareCoords brd coords (Taken sym)) else do
    putStrLn "Invalid move, try again"
    getHumanMove brd sym

{--
  basic AI opponent for a game of tic-tac-toe
  if there is a winning move this turn, the AI will make it.
  otherwise, if the opponent could win next turn, the AI will block it.
  if neither case applies, the AI prefers the center, then corners, then edges
--}
computerTicTacToeMove :: Board -> Symbol -> IO Board
computerTicTacToeMove brd sym = do
  let legalMoves = filter (isLegalMove brd) (allCoords 3 3)
  let possibleWins = filter  (\c -> (fst (ticTacToeWin (setSquareCoords brd c (Taken sym))))) legalMoves
  if possibleWins /= [] then return  (setSquareCoords brd (head possibleWins) (Taken sym)) else do
    let possibleLosses = filter (\c -> (fst (ticTacToeWin (setSquareCoords brd c (Taken (succ sym)))))) legalMoves
    if (possibleLosses /= []) then return (setSquareCoords brd (head possibleLosses) (Taken sym)) else do
      if (elem (2,2) legalMoves) then return (setSquareCoords brd (2,2) (Taken sym)) else do
        let corners = filter (\c -> elem c [(1,1),(1,3),(3,1),(3,3)]) legalMoves
        if (corners /= []) then return (setSquareCoords brd (head corners) (Taken sym)) else
          return (setSquareCoords brd (head legalMoves) (Taken sym))

--check if a move is legal
isLegalMove :: Board -> Coords -> Bool
isLegalMove brd c = case (getSquareCoords brd c) of
  (Just sq) -> (sq == Blank)
  (Nothing) -> False

--create a list of coordinates spanning the given dimensions
allCoords :: Int -> Int -> [Coords]
allCoords x y = concatMap (createLine [1..y]) [1..x]
  where createLine ys x = zip (repeat x) ys

--check if a game of tic tac toe is over, and return the winner if there is one
--a game is over either when a player has won or when there are no empty spaces
ticTacToeOver :: Board -> (Bool, Maybe Symbol)
ticTacToeOver brd
  |(fst (ticTacToeWin brd)) = ticTacToeWin brd
  |(boardFull brd) = (True, Nothing)
  |otherwise = (False, Nothing)

--play a game of tic-tac-toe and display the result
--list of players should be a circular / infinite list alternating between two players
playGame :: Board -> [Player] -> IO String
playGame brd (p:ps) = do
  print brd
  b <- getMove brd p
  let (gameOver,winner) = ticTacToeOver b
  if (gameOver)
    then case winner of
      (Nothing) -> do
        putStrLn (show b)
        return "Tied Game"
      (Just sym) -> do
        putStrLn (show b)
        return ("Game won by " ++ (show sym))
  else do
    playGame b ps

main = do
  result <- playGame ticTacToeBoard (cycle [Human X, AI O])
  putStrLn result
