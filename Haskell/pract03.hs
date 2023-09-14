
import Prelude hiding ((&&)) 
 

infixr 3 &&

-- A naive re-implementation of the Prelude operator &&
--(||) :: Bool -> Bool -> Bool
--True && True    = True
--False && True   = False
--True && False   = False
--False || False  = False

-- An alternative re-implementation
(&&) :: Bool -> Bool -> Bool
True && True     = True
_ && _           = False

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False      =  False
--True || a    = a

-------------------------------------------------------------------------------------------------------

-- exOr :: Bool -> Bool -> Bool
-- exOr False a  = a
-- exOr False a  = a
-- exOr False a  = a
-- exOr False a  = a

--True && False = True

--------------------------------------------------------------------------------------------------------

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
--ifThenElse False _ b = b
ifThenElse _ _ b = b

--True = int1
--False = int2



--------------------------------------------------------------------------------------------------------

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 8 = 30
daysInMonth 10 = 30
daysInMonth _ = 31

--1 || 3 || 5 || 7 || 9 || 10 || 12 = 31
--2 = 28
--4 || 6 || 8 || 10 = 30

--------------------------------------------------------------------------------------------------------

-- RECURSION----------------------------------------------------------------
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n-1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = (n ^ 2) + sumSquares (n - 1)

power :: Int -> Int -> Int
power _ 0 = 1
power 0 _ = 0
power x y = x * power x (y - 1)
--    | x > 0 && y > 0 = 
--    | x == 0 || y == 0 = 1


sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | x == y = 0
    | x > y = error "First number cant"
    |otherwise = x + sumFromTo (x + 1) y
    -- | x < y || x ==  y = x + sumFromTo (x + 1) y
    -- | x > y = 0
sumFromTo x y = if x > y then 0
sumFromTo x y = x + sumFromTo (x + 1) y

    
