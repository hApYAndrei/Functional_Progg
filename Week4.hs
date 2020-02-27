import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

--1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y =  (x+y, x-y)

--2
grade :: StudentMark -> Char
grade (s1,mk)
    | mk >= 70      = 'A'
    | mk >= 60      = 'B'
    | mk >= 50      = 'C'
    | mk >= 40      = 'D'
    | otherwise     = 'F'

--3

capMark :: StudentMark -> StudentMark
capMark (st,mk)
    | mk >= 40      = (st,40)
    | otherwise     = (st,mk)

--4
firstNumbers :: Int -> [Int]
firstNumbers x = [x, x - 1 .. 1]

--5
firstSquares :: Int -> [Int]
firstSquares x = [ (x - n)^2 | n <- [x-1,x-2..0]]

--6
capitalise :: String -> String
capitalise x = [ toUpper i | i <- x ]

--7
onlyDigits :: String -> String
onlyDigits digit = [ i | i <- digit, isDigit i]

--8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [ capMark(st,mk) | (st,mk) <- stmks ]

--9
--gradeStudents :: [StudentMark] -> [(String,Char)]
--gradeStudents grades = [ grade(st,mk) | (st,mk) <- grades ]
