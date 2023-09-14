import Data.Char
type StudentMark = (Int)

sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (sum,sub)
    where
    sum = x + y
    sub = x - y


grade :: StudentMark -> Char
grade mark
    |mark >= 70 = 'A'
    |mark >= 60 = 'B'
    |mark >= 50 = 'C'
    |mark >= 40 = 'D'
    |otherwise  = 'F'

capMark :: StudentMark -> StudentMark
capMark mark
    |mark > 40 = 40
    |otherwise = mark

firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

firstSquares :: Int -> [Int]
firstSquares n = [ i^2 | i <- nlist ]
    where
    nlist = [1 .. n]
    
-- type toUpper = (String)
-- map toUpper :: String -> String
-- type String = [Char]

capitalise :: String -> String
capitalise str = map toUpper str

onlyDigits :: String -> String
onlyDigits string = [ x | x <- string, not (x `elem` letters) ]
    where 
    letters = ['a' .. 'z']


-- capMarks :: [StudentMark] -> [StudentMark]
-- capMarks student mark = [ mark | (student,mark) <- capMark mark ]

-- gradeStudents :: [StudentMark] -> [(String,Char)]
-- gradeStudents student mark = [student | (student,mark) <- grade mark]

duplicate :: String -> Int -> String
--duplicate string times = concat (replicate times string) --CHEATED
duplicate _ 0 = " "
duplicate word n = word ++ duplicate word (n-1)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = if n > 1 then null [ x | x <- list, n `mod` x == 0] else False
    where
    list = [2 .. n-1]

