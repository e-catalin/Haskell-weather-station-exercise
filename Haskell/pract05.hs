headPlusOne :: [Int] -> Int
headPlusOne []     = 0
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead []     = []
duplicateHead (x:xs) = x : (x:xs)

rotate :: [a] -> [a]
rotate [] = []
rotate [a] = [a]
-- rotate list = case list of
--   x:y:xs -> y:x:xs
rotate (x:y:xs) =(y:x:xs)

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs
-- andAll (x:xs) = 

countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems n (x:xs) =
    if n == x then
        1 + countElems n xs
    else
        countElems n xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs)
    |n == x = removeAll n xs
    |otherwise = x:(removeAll n xs)

-- listMarks :: String -> [StudentMark] -> [Int]

 
sorted :: [Int] -> Bool
sorted [] = True
sorted (x:y:xs)
    |x < y = sorted xs
    |otherwise = False

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    |x == y = prefix xs ys
    |otherwise = False


