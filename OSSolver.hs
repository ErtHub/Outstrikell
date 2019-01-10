module OSSolver where

-- Coordinate system is as follows:
-- (0, 0)-------> x
--      |
--      |
--      |
--      v y

-- Data type for representing 1D direction of moving - in positive, negative direction or none
data Direction1D = Negative | None | Positive deriving (Show, Bounded, Enum, Eq)
-- Data type for representing 2D direction of moving in x and y directions
data Direction2D = Direction2D { dir_x :: Direction1D
                               , dir_y :: Direction1D
                               } deriving (Show)

-- Data type for representing position on a 2D grid
data Position2D = Position2D { pos_x :: Int
                             , pos_y :: Int
                             } deriving (Show, Eq)

-- Data type for representing single found match for the word in a grid
-- Consists of the starting position, direction and length of the word
data Word = Word { pos :: Position2D
                 , dir :: Direction2D
                 , len :: Int
                 } deriving (Show)

-- Function for moving a position in 1D
move1D position Negative = position - 1
move1D position None = position
move1D position Positive = position + 1
-- Function for moving a position in 2D
move2D (Position2D pos_x pos_y) (Direction2D dir_x dir_y) = Position2D (move1D pos_x dir_x) (move1D pos_y dir_y)
-- Function for moving a position in 2D multiple times
move2DBy pos dir times = if times == 0
                       then pos
                       else move2DBy (move2D pos dir) dir (times - 1)

-- Function for checking whether given position is inside the grid
isInside pos size_x size_y = pos_x pos < size_x &&
                             pos_y pos < size_y &&
                             pos_x pos >= 0 &&
                             pos_y pos >= 0

-- Function for generating a set of valid directions in which we search for a word
validDirections = [Direction2D x y | x <- [None, Positive], y <- [Negative .. Positive],
                                     x /= None || y /= None,
                                     x /= None || y /= Negative]

crop _ _ 0 = []
crop (c:str) 0 y = c:(crop str 0 (y - 1))
crop (c:str) x y = crop str (x - 1) y

pinpointSubstr [] _ = error "Trying to pinpoint an empty substring!"
pinpointSubstr substr str = [x | x <- [0..(length str - length substr)], substr == crop str x (length substr)]

-- Function which creates a sublist from a 2D grid starting from a given position
-- and going in a given direction till the border
getSublistInDirection [[]] _ _ = []
getSublistInDirection _ _ (Direction2D None None) = []
getSublistInDirection grid pos dir = if isInside pos size_x size_y
                                     then [grid !! pos_y pos !! pos_x pos] ++
                                           getSublistInDirection grid (move2D pos dir) dir
                                     else []
                                     where size_y = length grid
                                           size_x = length (head grid)

-- Function which searches for a given substring in a grid and returns a list of matches
findSubstr substr grid = [Word (move2DBy (Position2D x y) dir match) dir (length substr) |
        x <- [0..(length (head grid))],
        y <- [0..(length grid)],
        dir <- validDirections,
        match <- pinpointSubstr substr (getSublistInDirection grid (Position2D x y) dir),
        (dir_x dir == Positive && x == 0) ||
        (dir_y dir == Positive && y == 0) ||
        (dir_y dir == Negative && y == (length grid) - 1)
        ]
        
        

findAllSubstr [] grid = []
findAllSubstr (w:words) grid = findSubstr w grid : findAllSubstr words grid


--Function which returns positions for all letters in a word
getWordPos (Word pos dir len) | len  == 0 = []
                              | otherwise = pos:getWordPos (Word (move2D pos dir) dir (len - 1 ))
                              
                                                    
  
--Function which returns positions for all words  
getAllWordsPos [] = []
getAllWordsPos (w:words) = getWordPos w:getAllWordsPos words


--Function which strikes out all found words
strikeOutWords grid positions = [ if (elem (Position2D x y) positions) then '-' else grid !! y !! x  |
    y <- [0..(length grid - 1)],
    x <- [0..(length (head grid) - 1)]
    ]
   
   
getHiddenWord grid = filter (\x -> x /= '-') grid


solve grid words = getHiddenWord ( strikeOutWords grid (concat ( getAllWordsPos ( concat (findAllSubstr words grid ) ) ) ) )
