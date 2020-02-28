--1
absolute :: Int -> Int
absolute x
  | x > 0 = x
  | otherwise = -x

--2
sign :: Int -> Int
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

--3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | (x == y) && (x == z) && (y == z) = 3
  | (x /= y) && (x /= z) && (y /= z) = 0
  | otherwise = 2

--4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagx + diagy + diagz
                          where
                          diagx = x * sqrt 2
                          diagy = y * sqrt 2
                          diagz = z * sqrt 2

--5
taxiFare :: Int -> Float
taxiFare x = if x > 10 then addkm else first10
            where
          first10 = fare + fromIntegral(x) * 0.50
          fare = 2.20
          addkm = fare + fromIntegral(y) * 0.30 + fromIntegral(x - y) * 0.50
          y = x - 10

--6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
 | x > average = 1
 | y > average = 1
 | z > average = 1
  where average = div (x + y + z) 3

--7
validDate :: Int -> Int -> Bool
validDate day month = if ((month < 0 || month > 12) || (day < 0 || day > 31) ||
                      (month == 2 && (day > 28 || day < 0))) then False else True

--8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
 | (mod year 4 == 0) && (month == 2) = 29
 | month == 2 = 28
 | (month == 4) || (month == 6) || (month == 9) || (month == 11) = 30
 | otherwise = 31

-- ****************Written exercises (ALWAYS UNDERLINE YOUR NEXT STEP)
-- 1.i. sumThree 3 5 7
-- ↝ 3 + 5 + 7 						def(definition) of sumThree
-- ↝ 15							arithmetic
-- ii.sumThree 8 (1 + 3) 2
-- ↝ sumThree 8 4 2					arithmetic
-- ↝ 8 + 4 + 2						def of sumThree
-- ↝ 14							arithmetic
-- 2.i.threeDifferent 1 4 2
-- ↝ 1 /= 4 && 1 /= 2 && 2 /= 4				def of threeDifferent
-- ↝ True && 1 /= 2 && 2 /= 4				def of /=
-- ↝ True && True && 2 /= 4				def of /=
-- ↝ True && True && True				def of /=
-- ↝ True							def of &&
-- ii.threeDifferent 1 7 7
-- ↝  1 /= 7 && 1 /= 7 && 7 /= 7				def of threeDifferent
-- ↝  True && 1 /= 7 && 7 /= 7				def of /=
-- ↝  True && True && 7 /= 7				def of /=
-- ↝  True && True && False				def of /=
-- ↝  False						def of &&
-- 3.i.howManyEqual 3 5 2
-- ?? 3 == 5 && 3 == 2 && 5 == 2			first guard
-- ?? ↝ False && 3 == 2 && 5 == 2			def of ==
-- ?? ↝ False && False && 5 == 2			def of ==
-- ?? ↝ False && False && False			def of ==
-- ?? ↝ False						def of &&
-- ?? 3 /= 5 && 3 /= 2 && 5 /= 2				second guard
-- ?? ↝ True && 3 /= 2 && 5 /= 2			def of /=
-- ?? ↝ True && True && 5 /= 2				def of /=
-- ?? ↝ True && True && True				def of /=
-- ?? ↝ True						def of &&
-- ?? otherwise						third guard
-- ?? ↝ 2
-- ↝ 0
-- ii.howManyEqual 5 2 5
-- ?? 5 == 2 && 5 == 5 && 2 == 5			first guard
-- ?? ↝ False && 5 == 5 && 2 == 5			def of ==
-- ?? ↝ False && True && 2 == 5			def of ==
-- ?? ↝ False && True && False				def of ==
-- ?? ↝ False						def of &&
-- ?? 5 /= 2 && 5 /= 5 && 2 /= 5 			second guard
-- ?? ↝ True && 5 /= 5 && 2 /= 5			def of /=
-- ?? ↝ True && False && 2 /= 5			def of /=
-- ?? ↝ True && False && True				def of /=
-- ?? ↝ False						def of &&
-- ?? otherwise						third guard
-- ?? ↝ 2
-- ↝ 2
