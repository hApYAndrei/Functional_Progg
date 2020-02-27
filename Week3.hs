-- We don't import '||' from the prelude, so that we can
-- define our own version

--import Prelude hiding ((||))
--import Prelude hiding ((&&))


-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

--infixr 2  ||

-- A naive re-implementation of the Prelude operator ||
--(||) :: Bool -> Bool -> Bool
--True || True    = True
--False || True   = True
--True || False   = True
--False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int
fact n
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m


--infixr 3 &&

--(&&) :: Bool -> Bool -> Bool
--True && True    = True
--False && True   = True
--True && False   = True
--False && False  = False


--(&&) :: Bool -> Bool -> Bool
--False && False   = False
--_ && _           = True


--(&&) :: Bool -> Bool -> Bool
--True && _     =  True
--False && a    = a

--2
exOr :: Bool -> Bool -> Bool
exOr False False = False
exOr True True = False
exOr _ _ = True

--3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse False z w  = w
ifThenElse _ z w = z

--4
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth 1 = 31
daysInMonth _ = 31

--5
sumNumbers :: Int -> Int
sumNumbers n
 | n == 0 = 0
 | n > 0   = n + sumNumbers(n - 1)

--6
--sumSquares :: Int -> Int
--sumSquares n
