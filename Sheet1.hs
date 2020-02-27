--1
timesTen :: Int -> Int
timesTen x = 10 * x

--2
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

--3
areaOfCircle :: Float -> Float
areaOfCircle x = pi * x ^ 2

--4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder x  y = pi * x ^ 2 * y

--5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 x2 y1 y2 = sqrt((y1 - y2)^2 + (x1 - x2)^2)

--6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

--7
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

--8
isEven :: Int -> Bool
isEven x = mod x 2 == 0

--9
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

--10
absolute :: Int -> Int
absolute x = if x > 0 then x else -x
