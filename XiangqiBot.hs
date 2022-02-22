-- module (NICHT ÄNDERN!)
module XiangqiBot
  ( getMove,
    listMoves,
    getTestMoves,
    getTestBoard,
    Board
  )
where

-- More modules may be imported

import Control.Exception (tryJust)
import Control.Monad.List
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Char (digitToInt, isDigit, isUpper, toLower)
import Data.List (elemIndex)
import Data.Maybe (isJust, isNothing)
import Distribution.Compat.Lens (_1)
import System.IO
import System.Random
import Test.HUnit (Counts)
import Test.HUnit.Base
import Test.HUnit.Text
import Util

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove a = getRandElem moves
  where
    moves = buildMoves fen player
    player = getPlayer a
    fen = getPureFen a

listMoves :: String -> String
listMoves a = filter (not . (`elem` "\"")) str
  where
    str = show (buildMoves fen player)
    player = getPlayer a
    fen = getPureFen a

type Position = Maybe Figure

type Board = [[Position]]

data Figure = Figure Color FigureType deriving (Show, Eq)

data FigureType = Advisor | General | Horse | Cannon | Rook | Elephant | Soldier deriving (Show, Eq)

data Color = Red | Black deriving (Show, Eq)

getRandElem :: [String] -> String
getRandElem a = a !! rand
  where
    l = length a
    (rand, _) = randomR (0, l -1) generator

generator :: StdGen
generator = mkStdGen 40

slice :: Int -> Int -> [Position] -> [Position]
slice a b l = take b $ drop a l

setup :: String -> String
setup s = head (splitOn ' ' (concatMap uppackEmpty s))
  where
    uppackEmpty c
      | isDigit c = replicate (digitToInt c) '1'
      | c == '/' = "\n"
      | otherwise = [c]

initBoard :: String -> Maybe Board
initBoard = (mapM . mapM) readPosition . lines

showBoard :: Board -> String
showBoard = unlines . (map . map) showPosition

showPosition :: Position -> Char
showPosition = maybe ' ' showFigure

readPosition :: Char -> Maybe Position
readPosition '1' = Just Nothing
readPosition c = fmap Just (readFigure c)

showFigure :: Figure -> Char
showFigure (Figure Red Advisor) = 'A'
showFigure (Figure Red Horse) = 'H'
showFigure (Figure Red Rook) = 'R'
showFigure (Figure Red General) = 'G'
showFigure (Figure Red Cannon) = 'C'
showFigure (Figure Red Elephant) = 'E'
showFigure (Figure Red Soldier) = 'S'
showFigure (Figure Black Advisor) = 'a'
showFigure (Figure Black Horse) = 'h'
showFigure (Figure Black Rook) = 'r'
showFigure (Figure Black General) = 'g'
showFigure (Figure Black Cannon) = 'c'
showFigure (Figure Black Elephant) = 'e'
showFigure (Figure Black Soldier) = 's'

typeList :: [(Char, FigureType)]
typeList =
  [ ('r', Rook),
    ('a', Advisor),
    ('h', Horse),
    ('c', Cannon),
    ('e', Elephant),
    ('s', Soldier),
    ('g', General)
  ]

readFigure :: Char -> Maybe Figure
-- readFigure 'A' = Just(Figure Red Advisor)
-- readFigure 'H' = Just(Figure Red Horse)
-- readFigure 'R' = Just(Figure Red Rook)
-- readFigure 'G' = Just(Figure Red General)
-- readFigure 'C' = Just(Figure Red Cannon)
-- readFigure 'E' = Just(Figure Red Elephant)
-- readFigure 'S' = Just(Figure Red Soldier)
-- readFigure 'a' = Just(Figure Black Advisor)
-- readFigure 'h' = Just(Figure Black Horse)
-- readFigure 'r' = Just(Figure Black Rook)
-- readFigure 'g' = Just(Figure Black General)
-- readFigure 'c' = Just(Figure Black Cannon)
-- readFigure 'e' = Just(Figure Black Elephant)
-- readFigure 's' = Just(Figure Black Soldier)
-- readFigure _ = Nothing
readFigure c = fmap make lookUp
  where
    color = if isUpper c then Red else Black
    lookUp = lookup (toLower c) typeList
    make = Figure color

isRedFigure :: Maybe Figure -> Bool
isRedFigure Nothing = True
isRedFigure (Just (Figure c1 _)) = c1 == Red

getPlayer :: String -> Char
getPlayer s
  | isRedPlay s = 'r'
  | otherwise = 'b'

getPureFen :: String -> String
getPureFen s = head (splitOn ' ' s)

isRedPlay :: String -> Bool
isRedPlay s
  | last (splitOn ' ' s) == "r" = True
  | otherwise = False

isEnemy :: Maybe Figure -> Maybe Figure -> Bool
isEnemy Nothing Nothing = True
isEnemy _ Nothing = True
isEnemy Nothing _ = True
isEnemy (Just f1) (Just f2) = c1 /= c2
  where
    (Figure c1 _) = f1
    (Figure c2 _) = f2

side :: Maybe Figure -> Maybe Color
side Nothing = Nothing
side (Just (Figure Red _)) = Just Red
side (Just (Figure Black _)) = Just Black

getByIndex :: (Int, Int) -> Maybe Board -> Position
getByIndex (i, j) Nothing = Nothing
getByIndex (i, j) (Just b)
  | not (isOnBoard (i, j)) = Nothing
  | otherwise = b !! i !! j

--  used only for Generals (dupplication !)
getIndex :: Position -> Maybe Board -> Maybe (Int, Int)
getIndex Nothing Nothing = Nothing
getIndex _ Nothing = Nothing
getIndex Nothing _ = Nothing
getIndex (Just f) (Just x)
  | null l = Nothing
  | otherwise = Just (head l)
  where
    l = filter (\(i, j) -> Just f == getByIndex (i, j) (Just x)) positions
    positions = [(i, j) | i <- [0 .. 9], j <- [0 .. 8]]

count :: Eq a => Maybe a -> [Maybe a] -> Int
count x = length . filter (x ==)

isOnBoard :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> Bool
isOnBoard (a, b) = a >= 0 && a <= 9 && b >= 0 && b <= 8

getRow :: Int -> Maybe Board -> [Position]
getRow _ Nothing = []
getRow i (Just board) = fmap fig positions
  where
    fig (i, j) = getByIndex (i, j) (Just board)
    positions = [(i, j) | j <- [0 .. 8]]

getCol :: Int -> Maybe Board -> [Position]
getCol _ Nothing = []
getCol j (Just board) = fmap fig positions
  where
    fig (i, j) = getByIndex (i, j) (Just board)
    positions = [(i, j) | i <- [0 .. 9]]

replaceAt :: (Int, Int) -> Maybe Figure -> Maybe Board -> Maybe Board
replaceAt _ _ Nothing = Nothing
replaceAt (r, c) Nothing (Just m) = Just (take r m ++ [take c (m !! r) ++ [Nothing] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m)
replaceAt (r, c) (Just x) (Just m) =
  Just (take r m ++ [take c (m !! r) ++ [Just x] ++ drop (c + 1) (m !! r)] ++ drop (r + 1) m)

makeMove :: (Int, Int) -> (Int, Int) -> Maybe Board -> Maybe Board
makeMove _ _ Nothing = Nothing
makeMove (r, c) (t, d) (Just board) = replaceAt (t, d) dist clone
  where
    dist = getByIndex (r, c) (Just board)
    clone = replaceAt (r, c) Nothing (Just board)

makeMoveS :: String -> Maybe Board -> Maybe Board
makeMoveS _ Nothing = Nothing
makeMoveS [] (Just board) = Just board
makeMoveS move (Just board) = makeMove (r, c) (t, d) (Just board)
  where
    (r, c) = extractSource move
    (t, d) = extractDist move

getMovesByIndex :: Maybe (Int, Int) -> Maybe Board -> [String]
getMovesByIndex _ Nothing = []
getMovesByIndex Nothing _ = []
getMovesByIndex (Just (i, j)) (Just board)
  | not (isOnBoard (i, j)) = []
  | otherwise = case getByIndex (i, j) (Just board) of
    Nothing -> []
    Just (Figure _ Advisor) -> getAdvisorMoves (i, j) (Just board)
    Just (Figure _ General) -> getGeneralMoves (i, j) (Just board)
    Just (Figure _ Rook) -> getRookMoves (i, j) (Just board)
    Just (Figure _ Soldier) -> getSoldierMoves (i, j) (Just board)
    Just (Figure _ Cannon) -> getCannonMoves (i, j) (Just board)
    Just (Figure _ Elephant) -> getElephantMoves (i, j) (Just board)
    Just (Figure _ Horse) -> getHorseMoves (i, j) (Just board)

buildMoves :: String -> Char -> [String]
buildMoves a c = fitlerNonThreatenMoves (concatMap move filteredPositions) c board
  where
    board = initBoard (setup a)
    positions = [(i, j) | i <- [0 .. 9], j <- [0 .. 8]]
    filteredPositions = filter check positions
    check (i, j)
      | c == 'r' = isRedFigure (getByIndex (i, j) board)
      | otherwise = not (isRedFigure (getByIndex (i, j) board))
    move (i, j) = getMovesByIndex (Just (i, j)) board

---  (i, j) i <- [0..9] [0..8]
buildMove :: (Int, Int) -> (Int, Int) -> String
buildMove (a, b) (c, d)
  | not (isOnBoard (a, b) && isOnBoard (c, d)) = []
  | otherwise = s1 ++ "-" ++ s2
  where
    al = "abcdefghi"
    s1 = (al !! b) : show (9 - a)
    s2 = (al !! d) : show (9 - c)

extractDist :: String -> (Int, Int)
extractDist [] = (-1, -1)
extractDist s = (9 - c, d)
  where
    dist = last (splitOn '-' s)
    al = "abcdefghi"
    index = elemIndex (head dist) al
    a = case index of
      Just index -> index
      Nothing -> -1
    (c, d) = (digitToInt (last dist), a)

extractSource :: String -> (Int, Int)
extractSource [] = (-1, -1)
extractSource s = (9 - c, d)
  where
    dist = head (splitOn '-' s)
    al = "abcdefghi"
    index = elemIndex (head dist) al
    a = case index of
      Just index -> index
      Nothing -> -1
    (c, d) = (digitToInt (last dist), a)

isAnyNegativ :: (Ord a, Num a) => a -> a -> a -> a -> Bool
isAnyNegativ v k t u = any (< 0) [v, k, t, u]

isDeathLook :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe Board -> Bool
isDeathLook Nothing Nothing Nothing = False
isDeathLook _ _ Nothing = False
isDeathLook _ Nothing _ = False
isDeathLook Nothing _ _ = False
isDeathLook (Just (a, b)) (Just (c, d)) (Just board) = d /= b && isGeneralLookEachOther -- other Figure move into range
  where
    figR = getIndex (Just (Figure Red General)) (Just board) -- max i
    figB = getIndex (Just (Figure Black General)) (Just board) -- min i
    Just (k, t)
      | isJust figR = figR
      | otherwise = Just (-1, -1)
    Just (v, u)
      | isJust figB = figB
      | otherwise = Just (-1, -1)
    positions
      | isAnyNegativ u v k t = []
      | u == t && u == d = [(i, d) | i <- [v + 1 .. k -1]] -- fig -> fig general range
      | u == t && u == b = [(i, b) | i <- [v + 1 .. k -1]] --  fig <- fig  generals range
      | u /= t && ((a, b) == (v, u) || (k, t) == (a, b)) = [(i, d) | i <- [v + 1 .. k -1]] --  genral->  general range
      | otherwise = []
    filterd = filter (\(i, j) -> i < 9 && j <= 9) positions
    isGeneralLookEachOther = positions /= [] && not (any (uncurry found) filterd)
    found i j
      | (i, j) == (c, d) = True
      | (i, j) == (a, b) = False
      | otherwise = isJust (getByIndex (i, j) (Just board))

isFigureThreatenBy :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe Board -> Bool
isFigureThreatenBy _ _ Nothing = False
isFigureThreatenBy _ Nothing _ = False
isFigureThreatenBy Nothing _ _ = False
isFigureThreatenBy (Just (a, b)) (Just (c, d)) (Just board)
  | not (isOnBoard (a, b) && isOnBoard (c, d)) = False
  | isEnem && hasMoveIntersection (a, b) (c, d) = True
  | otherwise = False
  where
    isEnem = isEnemy (getByIndex (a, b) (Just board)) (getByIndex (c, d) (Just board))
    move (a, b) (c, d) = buildMove (c, d) (a, b)
    enemiesMoves (c, d) = getMovesByIndex (Just (c, d)) (Just board)
    hasMoveIntersection (a, b) (c, d) = move (a, b) (c, d) `elem` enemiesMoves (c, d)

getIndexTeamGeneral :: Maybe Color -> Maybe Board -> Maybe (Int, Int)
getIndexTeamGeneral Nothing Nothing = Nothing
getIndexTeamGeneral _ Nothing = Nothing
getIndexTeamGeneral Nothing _ = Nothing
getIndexTeamGeneral (Just c) (Just board) = getIndex (Just (Figure c General)) (Just board)

isTeamGeneralThreaten :: Maybe Color -> Maybe Board -> Bool
isTeamGeneralThreaten _ Nothing = False
isTeamGeneralThreaten Nothing _ = False
isTeamGeneralThreaten (Just c) (Just board) = isThreaten
  where
    isThreaten = length (filter (\(i, j) -> not (check (i, j))) positions) /= length positions
    positions = filter filterEniems [(i, j) | i <- [0 .. 9], j <- [0 .. 8]]
    index = getIndexTeamGeneral (Just c) (Just board)
    figure (i, j) = getByIndex (i, j) (Just board)
    filterEniems (i, j) =
      let l = figure (i, j)
       in case l of
            Nothing -> False
            Just l -> side (Just l) /= Just c
    check (i, j)
      | not (isOnBoard (i, j)) = False
      | isJust index = isFigureThreatenBy index (Just (i, j)) (Just board)
      | otherwise = False

fitlerNonThreatenMoves :: [String] -> Char -> Maybe Board -> [String]
fitlerNonThreatenMoves [] _ _ = []
fitlerNonThreatenMoves s _ Nothing = s
fitlerNonThreatenMoves s c (Just board)
  | c == 'r' || c == 'b' = moves
  | otherwise = s
  where
    side
      | 'r' == c = Red
      | otherwise = Black
    moves = filter (not . check) s
    check t = isTeamGeneralThreaten (Just side) (makeMoveS t (Just board))

getCannonMoves :: (Int, Int) -> Maybe Board -> [String]
getCannonMoves _ Nothing = []
getCannonMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions = [(i, b) | i <- [0 .. 9]] ++ [(a, j) | j <- [0 .. 8]]
    subRange i j
      | j == b && i /= a && i - a > 0 = [(i, j) | i <- [a + 1 .. i]] -- forward
      | j == b && i /= a && i - a < 0 = [(i, j) | i <- [i .. a]] -- backword
      | j /= b && i == a && j - b > 0 = [(i, j) | j <- [b + 1 .. j]] -- right
      | j /= b && i == a && j - b < 0 = [(i, j) | j <- [j .. b]] -- left
      | otherwise = []
    checkFigure (i, j)
      | let f = getByIndex (i, j) (Just board) in isJust f = False
      | otherwise = True
    lenghtCheck i j = length (takeWhile checkFigure (subRange i j))
    acceptance i j
      | j == b && i /= a && i - a > 0 = i - a
      | j == b && i /= a && i - a < 0 = a - i
      | j /= b && i == a && j - b > 0 = j - b
      | j /= b && i == a && j - b < 0 = b - j
      | otherwise = 0
    sliceAt i j
      | j == b && i /= a && i - a > 0 = slice a i (getCol j (Just board))
      | j == b && i /= a && i - a < 0 = slice i a (getCol j (Just board))
      | j /= b && i == a && j - b > 0 = slice b j (getRow i (Just board))
      | j /= b && i == a && j - b < 0 = slice j b (getRow i (Just board))
      | otherwise = []
    hasNoFigureInPath i j = let l = lenghtCheck i j in l == acceptance i j
    isAttack i j
      | j == b && i /= a =
        let f = getByIndex (i, j) (Just board)
            c = count Nothing (sliceAt i j)
         in (c == abs (i - a) -2) && isJust f
      | j /= b && i == a =
        let f = getByIndex (i, j) (Just board)
            c = count Nothing (sliceAt i j)
         in (c == abs (i - b) -2) && isJust f
      | otherwise = False
    check (i, j) =
      isEnemy figure (getByIndex (i, j) (Just board))
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))
        && (hasNoFigureInPath i j || isAttack i j)

getSoldierMoves :: (Int, Int) -> Maybe Board -> [String]
getSoldierMoves _ Nothing = []
getSoldierMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions = filter (\(i, j) -> i < 9 && j <= 9) [(a + 1, b), (a -1, b), (a, b + 1), (a, b -1)]
    isSideMove i j = abs (j - b) == 1
    isForwardDirection i j
      | isRedFigure figure = a - i == 1 && a > 0
      | otherwise = i - a == 1 && a < 9
    isHorizantalAllowed i j
      | isRedFigure figure = a <= 4
      | otherwise = a >= 5
    isInMovementLimitation i j
      | isHorizantalAllowed i j = isForwardDirection i j || isSideMove i j
      | otherwise = isForwardDirection i j && b == j
    check (i, j) =
      isEnemy figure (getByIndex (i, j) (Just board))
        && isInMovementLimitation i j
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

getGeneralMoves :: (Int, Int) -> Maybe Board -> [String]
getGeneralMoves _ Nothing = []
getGeneralMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions
      | isRedFigure figure = [(i, j) | i <- [7 .. 9], j <- [3 .. 5]]
      | otherwise = [(i, j) | i <- [0 .. 2], j <- [3 .. 5]]
    isInMovementLimitation i j = (abs (j - b) == 1 && i == a) || (j == b && abs (i - a) == 1)
    clone (i, j) = makeMove (a, b) (i, j) (Just board)
    check (i, j)
      | not (isOnBoard (i, j)) = False
      | otherwise =
        isEnemy figure (getByIndex (i, j) (Just board))
          && isInMovementLimitation i j
          && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

getAdvisorMoves :: (Int, Int) -> Maybe Board -> [String]
getAdvisorMoves _ Nothing = []
getAdvisorMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions
      | isRedFigure figure = [(7, 3), (9, 3), (8, 4), (7, 5), (9, 5)]
      | otherwise = [(1, 4), (0, 3), (2, 3), (2, 5), (0, 5)]
    isInMovementLimitation i j = abs (j - b) == 1 && abs (i - a) == 1
    clone (i, j) = makeMove (a, b) (i, j) (Just board)
    check (i, j) =
      isEnemy figure (getByIndex (i, j) (Just board))
        && isInMovementLimitation i j
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

getRookMoves :: (Int, Int) -> Maybe Board -> [String]
getRookMoves _ Nothing = []
getRookMoves (a, b) (Just board) = map move filtered
  where
    figure = getByIndex (a, b) (Just board)
    filtered = filter check positions
    move pos = buildMove (a, b) pos
    positions = [(i, b) | i <- [0 .. 9]] ++ [(a, j) | j <- [0 .. 8]]
    subRange i j
      | j == b && i /= a && i - a > 0 = [(i, j) | i <- [a + 1 .. i]] -- forward
      | j == b && i /= a && i - a < 0 = [(i, j) | i <- [i .. a]] -- backword
      | j /= b && i == a && j - b > 0 = [(i, j) | j <- [b + 1 .. j]] -- right
      | j /= b && i == a && j - b < 0 = [(i, j) | j <- [j .. b]] -- left
      | otherwise = []
    checkFigure (i, j)
      | let f = getByIndex (i, j) (Just board) in isJust f = False
      | otherwise = True
    lenghtCheck i j = length (takeWhile checkFigure (subRange i j))
    acceptance i j
      | j == b && i /= a && i - a > 0 = i - a
      | j == b && i /= a && i - a < 0 = a - i
      | j /= b && i == a && j - b > 0 = j - b
      | j /= b && i == a && j - b < 0 = b - j
      | otherwise = 0
    sliceAt i j
      | j == b && i /= a && i - a > 0 = slice a i (getCol j (Just board))
      | j == b && i /= a && i - a < 0 = slice i a (getCol j (Just board))
      | j /= b && i == a && j - b > 0 = slice b j (getRow i (Just board))
      | j /= b && i == a && j - b < 0 = slice j b (getRow i (Just board))
      | otherwise = []
    hasNoFigureInPath i j = let l = lenghtCheck i j in l == acceptance i j
    isAttack i j
      | j == b && i /= a =
        let f = getByIndex (i, j) (Just board)
            c = count Nothing (sliceAt i j)
         in (c >= abs (i - a) -1 && c <= 2) && isEnemy figure f && isJust f
      | j /= b && i == a =
        let f = getByIndex (i, j) (Just board)
            c = count Nothing (sliceAt i j)
         in (c >= abs (j - b) -1 && c <= 2) && isEnemy figure f && isJust f
      | otherwise = False
    check (i, j) =
      (hasNoFigureInPath i j || isAttack i j)
        && isEnemy figure (getByIndex (i, j) (Just board))
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

getElephantMoves :: (Int, Int) -> Maybe Board -> [String]
getElephantMoves _ Nothing = []
getElephantMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions = [(a + 2, b + 2), (a -2, b -2), (a -2, b + 2), (a + 2, b -2)]
    isBehindeRiverLimitation i j
      | isRedFigure figure = i >= 5 && i <= 9
      | otherwise = i <= 4 && i >= 0
    isPathFree (i,j) |  i> a && j >b = isNothing (getByIndex(i-1,j-1) (Just board))
                    |  i< a && j > b = isNothing (getByIndex(i+1,j-1) (Just board))
                    |  i> a && j < b = isNothing (getByIndex(i-1,j+1) (Just board))
                    |  i< a && j < b = isNothing (getByIndex(i+1,j+1) (Just board))
                    | otherwise  = False
    check (i, j) =
      isEnemy figure (getByIndex (i, j) (Just board))
        && isBehindeRiverLimitation i j &&  isPathFree (i,j)
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

getHorseMoves :: (Int, Int) -> Maybe Board -> [String]
getHorseMoves _ Nothing = []
getHorseMoves (a, b) (Just board) = map move (filter check positions)
  where
    figure = getByIndex (a, b) (Just board)
    move pos = buildMove (a, b) pos
    positions = [(i, j) | i <- [0 .. 9], j <- [0 .. 8]]
    stepCol i j
      | j - b > 0 = 1
      | otherwise = -1
    stepRow i j
      | i - a > 0 = 1
      | otherwise = -1
    isInMovementLimitation i j
      | abs (j - b) == 1 && abs (i - a) == 2 = isNothing (getByIndex (a + stepRow i j, b) (Just board))
      | abs (j - b) == 2 && abs (i - a) == 1 = isNothing (getByIndex (a, b + stepCol i j) (Just board))
      | otherwise = False
    check (i, j) =
      isEnemy figure (getByIndex (i, j) (Just board))
        && isInMovementLimitation i j
        && not (isDeathLook (Just (a, b)) (Just (i, j)) (Just board))

-----------------------------
-------------Tests-----------
-----------------------------
getTestBoard :: String -> Maybe Board
getTestBoard a = initBoard (setup a)

getTestMoves :: (Int, Int) -> String -> [String]
getTestMoves _ [] = []
getTestMoves (a, b) s
  | isNothing fig = []
  | otherwise = fitlerNonThreatenMoves moves c board
  where
    board = getTestBoard s
    fig = getByIndex (a, b) board
    c
      | side fig == Just Red = 'r'
      | otherwise = 'b'
    moves = getMovesByIndex (Just (a, b)) board

setupTest :: Test
setupTest =
  TestCase
    ( do
        assertEqual "Too Much Spaces to Inline" "1111g1111\n1111c1111\n11111s111\n111111111\n111111111\n111111111\n1S1111111\n1111111C1\n111111111\n1111G1111" (setup "4g4/4c4/5s3/9/9/9/1S7/7C1/9/4G4")
        assertEqual "Less Spaces To Inine" "1111ga111\n11e1a1111\n11111s111\n1111c1111\n111111111\n111111111\n1S1111111\n1111111C1\n111111111\n111AG11R1" (setup "4ga3/2e1a4/5s3/4c4/9/9/1S7/7C1/9/3AG2R1")
    )


getByIndexTest :: Test
getByIndexTest =
  TestCase
    ( do
        assertEqual "I am Black Advisor :)" (Just (Figure Black Advisor)) (getByIndex (0, 5) (getTestBoard "4ga3/2e6/3a5/4c2s1/9/4SsC2/1S3RS2/9/4R4/3AG4"))
        assertEqual "I am Red Advisor :)" (Just (Figure Red Advisor)) (getByIndex (8, 4) (getTestBoard "4ga3/2e6/3a5/4c2s1/9/4SsC2/1S3RS2/9/4A4/4GR3"))
    )

isFigureThreatenByTest :: Test
isFigureThreatenByTest =
  TestCase
    ( do
        assertEqual "Balck Soldier is Not Threaten By Red Soldier" False (isFigureThreatenBy (Just (4, 5)) (Just (5, 5)) (getTestBoard "4g4/4a4/9/9/5S3/5s3/9/9/4R4/3AG4"))
        assertEqual "Red Soldier is Threaten By Black Horse" True (isFigureThreatenBy (Just (5, 5)) (Just (3, 4)) (getTestBoard "4g4/4a4/9/4h4/5s3/5S3/9/9/4R4/3AG4"))
        assertEqual "Black General is Not Threaten By Black Advisor" False (isFigureThreatenBy (Just (0, 4)) (Just (1, 4)) (getTestBoard "4g4/4a4/9/4h4/5s3/5S3/9/9/4R4/3AG4"))
        assertEqual "Red Rook is Not Threaten By Red General" False (isFigureThreatenBy (Just (8, 4)) (Just (9, 4)) (getTestBoard "4g4/4a4/9/4h4/5s3/5S3/9/9/4R4/3AG4"))
        assertEqual "Black General is Threaten By Red Cannon (One Empty Between Them)" True (isFigureThreatenBy (Just (0, 4)) (Just (7, 4)) (getTestBoard "4g4/9/4a4/3h5/5s3/5S3/9/4C4/4R4/3AG4"))
    )

isTeamGeneralThreatenTest :: Test
isTeamGeneralThreatenTest =
  TestCase
    ( do
        assertEqual "No Threat On Black General" False (isTeamGeneralThreaten (Just Black) (getTestBoard "3ag4/4e4/9/2c1R4/1e7/9/9/1C1Sr2s1/4A1h2/3AG4"))
        assertEqual "Red General is Threaten by Black Horse" True (isTeamGeneralThreaten (Just Red) (getTestBoard "3ag4/4e4/9/2c1R4/1e7/9/9/1C1Sr2s1/4A1h2/3AG4"))
        assertEqual "Black General is Threaten by Red Cannon Long Range" True (isTeamGeneralThreaten (Just Black) (getTestBoard "3ag4/9/2R1e4/2c6/1e7/4C4/6h2/3Sr2s1/4A4/3AG4"))
        assertEqual "Black General Protected with Friends is Threaten by Red Cannon Long Range" True (isTeamGeneralThreaten (Just Black) (getTestBoard "1R1ageS2/4e4/9/2c6/9/4C4/6h2/4r2s1/4A4/3AG4"))
    )

deathLookTest :: Test
deathLookTest =
  TestCase
    ( do
        assertEqual "Red General move to Open Spot With General" True (isDeathLook (Just (8, 4)) (Just (7, 5)) (getTestBoard "4ga3/2e6/3a5/2c4s1/9/2S2sC2/1S3RS2/9/4A4/4GR3"))
        assertEqual "Red General move to Open Spot With Rook" False (isDeathLook (Just (8, 4)) (Just (7, 5)) (getTestBoard "3gra3/2e6/3a5/2c4s1/9/2S3C2/1S3RS2/3A1s3/9/3G1R3"))
        assertEqual "Move Black Rook From Open Range" True (isDeathLook (Just (1, 4)) (Just (1, 3)) (getTestBoard "4ga3/2e1r4/3a5/2c4s1/9/2S3C2/1S3RS2/3A1s3/9/4GR3"))
        assertEqual "Move Soldier Forward In Between Generals" False (isDeathLook (Just (4, 4)) (Just (5, 4)) (getTestBoard "4ga3/2e2r3/3a5/2c4S1/4s4/6C2/1S3RS2/3A1s3/9/4GR3"))
    )

tests :: Test
tests =
  TestList
    [ TestLabel "SETUP STRING FOR BOARD NOT WORK!" setupTest,
      TestLabel "GET FIGURE BY INDEX NOT WORK!" getByIndexTest,
      TestLabel "IS THREATEN BY IS NOT WORKING!" isFigureThreatenByTest,
      TestLabel "IS GENERAL THREATEN BY SOMEONE NOT WORKING!" isTeamGeneralThreatenTest
    ]

main :: IO (Counts, Int)
main = runTestText (putTextToHandle stdout False) tests