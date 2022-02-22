module XiangqiFormat where

import Control.Monad
import qualified Data.Char as Char
import Data.List
import System.IO
import Test.HUnit
import Util
import XiangqiBot

isRow, isCol :: Char -> Bool
isRow = between '0' '9' -- row
isCol = between 'a' 'i' -- column

between low high x = low <= x && x <= high

isValidMove :: String -> Bool
isValidMove [srcCol, srcRow, '-', dstCol, dstRow] =
  isCol srcCol && isRow srcRow && isCol dstCol && isRow dstRow
isValidMove _ = False

isValidListOf :: (String -> Bool) -> String -> Bool
isValidListOf validItem ('[' : str) =
  (last str == ']') && all validItem (Util.splitOn ',' $ init str)

assertFormat :: String -> (String -> Bool) -> Assertion
assertFormat actual check =
  unless (check actual) (assertFailure msg)
  where
    msg = "Wrong format! Looks like: \"" ++ actual ++ "\""

hasFigureUniqueMoves :: (Int, Int) -> String -> Bool
hasFigureUniqueMoves (a, b) s = not (any (\x -> count x moves > 1) moves)
  where
    moves = getTestMoves (a, b) s
    count x = length . filter (== x)

areTotalMovesUnique :: String -> Bool
areTotalMovesUnique s = not (any (\x -> count x moves > 1) moves)
  where
    total = listMoves s
    pure = drop 1 (take (length total -1) total)
    moves = splitOn ',' pure
    count x = length . filter (== x)

--------------------------------------------------------------------------



soldierMovesTest :: Test
soldierMovesTest =
  TestCase
    ( do
        assertEqual "Black Soldier Move Between Generals!" ["e4-e3"] (getTestMoves (5, 4) "4ga3/2e6/3a5/3c3s1/9/4s1C2/1S3RS2/9/9/3AGR3")
        assertEqual "Soldier Move Forward before River" ["f7-f6"] (getTestMoves (2, 5) "4ga3/2e1a4/5s3/4c2s1/9/6S2/1S2S4/7C1/4R4/3AG2R1")
        assertEqual "Red Soldier Moving After River" ["e5-e6", "e5-f5", "e5-d5"] (getTestMoves (4, 4) "4ga3/2e1a4/4cs3/7s1/4S4/4S4/1S7/7C1/4R4/3AG2R1")
        assertEqual "Black Soldier Moving After River" ["f4-f3", "f4-g4", "f4-e4"] (getTestMoves (5, 5) "4ga3/2e1a4/9/4c2s1/6S2/5s3/1S2S4/7C1/4R4/3AG2R1")
        assertEqual "Black Soldier Traped With Enemies After River" ["f4-f3", "f4-g4", "f4-e4"] (getTestMoves (5, 5) "4ga3/2e1a4/9/4c2s1/6S2/4SsC2/1S3R3/9/4R4/3AG4")
        assertEqual "Red Soldier meet Friends Before River" [] (getTestMoves (6, 6) "4ga3/2e1a4/9/4c2s1/9/4SsC2/1S3RS2/9/4R4/3AG4")
    )

advisorMovesTest :: Test
advisorMovesTest =
  TestCase
    ( do
        assertEqual "Red Advisor Move With Blocked Enemy!" ["e1-d2", "e1-d0", "e1-f2"] (getTestMoves (8, 4) "4ga3/2e6/3a5/2c4s1/9/2S3C2/1S2sRS2/9/4A4/4GR3")
        assertEqual "Red Advisor Do not Move Death Look!" [] (getTestMoves (8, 4) "4ga3/2e6/3a5/2c4s1/9/2S2sC2/1S3RS2/9/4A4/4GR3")
        assertEqual "Black Advisor Move Middle of Palace" ["f9-e8"] (getTestMoves (0, 5) "4ga3/2e6/3a5/4c2s1/9/4SsC2/1S3RS2/9/4R4/3AG4")
        assertEqual "Black Advisor In Center Of Palace" ["d7-e8"] (getTestMoves (2, 3) "4ga3/2e6/3a5/4c2s1/9/4SsC2/1S3RS2/9/4R4/3AG4")
        assertEqual "Red Advisor Get Out Of Center-> Threat General" [] (getTestMoves (8, 4) "4ga3/2e6/3a5/4c2s1/9/4SsC2/1S3RS2/9/4A4/4GR3")
        assertEqual "Red Advisor Do not Move Death Look!" [] (getTestMoves (8, 4) "4ga3/2e6/3a5/2c4s1/9/2S2sC2/1S3RS2/9/4A4/4GR3")
    )

generalsMovesTest :: Test
generalsMovesTest =
  TestCase
    ( do
        assertEqual "Red General Move one Step & Fake General, Other One Got Threat!" ["d0-e0"] (getTestMoves (9, 3) "3gra3/2e6/3a5/3c3s1/9/3S1sC2/1S3RS2/3A5/9/3G1R3")
        assertEqual "Red General has Death Look only on e0!" ["d0-d1"] (getTestMoves (9, 3) "4ga3/2e6/3a5/2c4s1/9/2S3C2/1S3RS2/3A1s3/9/3G1R3")
    )

getCannonMovesTest :: Test
getCannonMovesTest =
  TestCase
    ( do
        assertEqual "Red Cannon Moves With one Jump Attack" ["e2-e9", "e2-e5", "e2-e4", "e2-e3", "e2-a2", "e2-b2", "e2-c2", "e2-d2", "e2-f2", "e2-g2", "e2-h2", "e2-i2"] (getTestMoves (7, 4) "4g4/9/9/3ha4/5s3/5S3/9/4C4/4R4/3AG4")
        assertEqual "Black Cannon Moves With Friends" ["b6-b5", "b6-b4", "b6-b3", "b6-b2", "b6-b1", "b6-b0", "b6-a6", "b6-c6"] (getTestMoves (3, 1) "4g4/4r4/1e2a4/1c1h5/5s3/5S3/9/4C4/4R4/3AG4")
        assertEqual "Red Cannon At Left Of Bord" ["b2-b8", "b2-b5", "b2-b4", "b2-b3", "b2-b1", "b2-b0", "b2-a2"] (getTestMoves (7, 1) "4g4/1e7/4a4/1ech5/5s3/9/9/1CS6/4R4/3AG4")
        assertEqual "Red Cannon move With Lang Range Attack Enemy Between" ["b2-b8", "b2-b3", "b2-b1", "b2-b0", "b2-a2"] (getTestMoves (7, 1) "4g4/1e7/4a4/2ch5/5s3/1e7/9/1CS6/4R4/3AG4")
        assertEqual "Red Cannon move With Lang Range Attack friend Between" ["b2-b8", "b2-b3", "b2-b1", "b2-b0", "b2-a2", "b2-c2", "b2-d2", "b2-h2"] (getTestMoves (7, 1) "4g4/1e7/4a4/2ch5/9/1e7/9/1C2S2s1/4R4/3AG4")
    )

getElephantMovesTest :: Test
getElephantMovesTest =
  TestCase
    ( do
        assertEqual "Black Elephant Moves diagnal" ["g7-i5", "g7-i9", "g7-e5"] (getTestMoves (2, 6) "4g4/9/4a1e2/2ch5/1e3s3/9/9/1CS6/4R4/3AG4")
        assertEqual "Red Elephant Moves" ["g0-e2", "g0-i2"] (getTestMoves (9, 6) "4g4/9/4a1e2/2ch5/1e3s3/9/9/1CS6/4R4/3AG1E2")
        assertEqual "Red Elephant Jump from Trap" ["e5-c7", "e5-g7"] (getTestMoves (4, 4) "3ag4/1e7/4h4/1hc1R1S2/3SeS3/2C1R4/5H3/4r2s1/9/3AG4")
        assertEqual "Red Elephant Right Rigure" ["g7-i9", "g7-e5"] (getTestMoves (2, 6) "4g4/9/6e2/2ch3a1/1e7/9/9/1C2S2s1/4R4/3AG4")
    )

getHorseMovesTest :: Test
getHorseMovesTest =
  TestCase
    ( do
        assertEqual "Black Horse can not jump form Trap" [] (getTestMoves (4, 4) "3ag4/1e7/9/2c1R2S1/1e1ShS3/4R4/9/1C2r2s1/9/3AG4")
        assertEqual "Black Horse can move only throgh left" ["e5-c4"] (getTestMoves (4, 4) "4g4/1e2a4/9/2c1R2S1/1e2hS3/3SR4/9/1C2r2s1/9/3AG4")
        assertEqual "Black Horse attacks to escape" ["e5-g6", "e5-c4", "e5-g4"] (getTestMoves (4, 4) "4g4/1e2a4/9/2c1R1S2/1e2h4/2CSR1S2/9/4r2s1/9/3AG4")
        assertEqual "Red Horse moves to Protect General (General is Threaten) " ["f3-e1"] (getTestMoves (6, 5) "4g4/1e2a4/9/2c1R1S2/1e2h4/2CSR1S2/5H3/4r2s1/9/3AG4")
        assertEqual "Red Horse moves  (General is Not Threaten) " ["f3-e5", "f3-g5", "f3-h4", "f3-d2", "f3-h2", "f3-g1"] (getTestMoves (6, 5) "4g4/1e2a4/9/2c1R1S2/1e2h4/2CSR1S2/5H3/4r2s1/4A4/4G4")
        assertEqual "Black Horse moves to Protect (General is Threaten) " ["d6-e8", "d6-e4"] (getTestMoves (3, 3) "4g4/9/9/3ha4/5s3/5S3/9/4C4/4R4/3AG4")
    )

getRookMovesTest :: Test
getRookMovesTest =
  TestCase
    ( do
        assertEqual "Red Rook Moves With Friends" ["e1-e2", "e1-a1", "e1-b1", "e1-c1", "e1-d1", "e1-f1"] (getTestMoves (8, 4) "4g4/9/4a1e2/2ch5/1e3s3/9/4R4/1CS6/4R1C2/3AG1E2")
        assertEqual "Black Rook Moves and Enemy in Front " ["e4-e6", "e4-e5", "e4-e3", "e4-e2", "e4-c4", "e4-d4", "e4-f4", "e4-g4", "e4-h4", "e4-i4"] (getTestMoves (5, 4) "4g4/1e7/4a4/2ch5/9/1e2r4/9/1C2S2s1/4R4/3AG4")
        assertEqual "Black Rook Moves On Trap " ["b1-b2", "b1-b0", "b1-a1", "b1-c1", "b1-d1", "b1-e1"] (getTestMoves (8, 1) "4g4/1e7/4a4/2ch5/9/1e7/9/1C2S2s1/1r2R4/3AG4")
        assertEqual "Red Rook Moves On Long Range Trap " ["e6-e9", "e6-e8", "e6-e7", "e6-e5", "e6-c6", "e6-d6", "e6-f6", "e6-g6", "e6-h6", "e6-i6"] (getTestMoves (3, 4) "3ag4/1e7/9/2c1R4/4h4/1e7/9/1C2r2s1/4S4/3AG4")
        assertEqual "Red Rook Moves On Long Range Trap With friend on Right " ["e6-e9", "e6-e8", "e6-e7", "e6-e5", "e6-c6", "e6-d6", "e6-f6", "e6-g6"] (getTestMoves (3, 4) "3ag4/1e7/9/2c1R2S1/4h4/1e7/9/1C1Sr2s1/4A4/4G4")

        assertEqual "Red Rook Do not Move Because Theathen The General " ["e1-e2"] (getTestMoves (8, 4) "4g4/9/4a1e2/2ch5/1e3sR2/9/9/1CS1r4/4R1C2/3AG1E2")
    )

uniqueMovesTest =
  TestCase
    ( do
        assertEqual "Red Cannon has Unique Moves" True (hasFigureUniqueMoves (7, 4) "4g4/9/9/3ha4/5s3/5S3/9/4C4/4R4/3AG4")
        assertEqual "Black Elephant has Unique Moves" True (hasFigureUniqueMoves (2, 4) "4g4/4a4/4e4/3h5/5s3/5S3/9/4C4/4R4/3AG4")
        assertEqual "Total Available Red Moves are Unique" True (areTotalMovesUnique "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r")
        assertEqual "Total Available Black Moves are Unique" True (areTotalMovesUnique "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR b")
    )

testFormat =
  TestList
    [ TestLabel "MOVE FORMAT WRONG!" (TestCase (assertFormat (XiangqiBot.getMove "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r") isValidMove)),
      TestLabel "LIST FORMAT WRONG!" (TestCase (assertFormat (XiangqiBot.listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r") $ isValidListOf isValidMove)),
      TestLabel "SOLDIERS MOVES ARE WRONG!" soldierMovesTest,
      TestLabel "ADVISORS MOVES ARE WRONG!" advisorMovesTest,
      TestLabel "GENERALS MOVES ARE WRONG!" generalsMovesTest,
      TestLabel "CANNON MOVES ARE WRONG!" getCannonMovesTest,
      TestLabel "ELEPHANT MOVES ARE WRONG!" getElephantMovesTest,
      TestLabel "HORSE MOVES ARE WRONG!" getHorseMovesTest,
      TestLabel "ROOK MOVES ARE WRONG!" getRookMovesTest,
      TestLabel "UNIQUE MOVES ARE WRONG!" uniqueMovesTest
    ]

main :: IO (Counts, Int)
main = runTestText (putTextToHandle stdout False) testFormat