absolute :: Int -> Int
absolute x
    |x >= 0         = x
    |otherwise          = -x

sign :: Int -> Int
sign x
    |x > 0          = 1
    |x == 0         = 0
    |x < 0          = -1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    |x == y && x == z           = 3
    |x /= y && x /= z           = 0
    |otherwise                  = 2

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths l1 l2 l3 = diagonal l1 + diagonal l2 + diagonal l3
    where 
    diagonal x = x * sqrt (2)

taxiFare :: Int -> Float
taxiFare distance
    | distance <= 10 = 2.2 + 0.5 * fromIntegral(distance)
    | distance > 10 = 7.2 + 0.3 * fromIntegral(distance)

-- taxiFare = 2.20 + rest

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    |x > average && y > average || x > average && z > average || y > average && z > average = 2
    |x > average || y > average || z > average = 1
    |otherwise = 0
    where
    average = (x + y + z) `div` 3
       

validDate :: Int -> Int -> Bool
validDate day month = validDay && validMonth
    where 
    validDay = day >= 1 && day <= 31 && month `elem` month31Days || day >= 1 && day <= 30 && month `elem` month30Days || day >= 1 && day <= 28 && month == 2
    --validDay = day >= 1 && day <= 30 && month `elem` month30Days
    --validDay = day >= 1 && day <= 28 && month == 2
    validMonth = month >= 1 && month <= 12
    month31Days = [1,3,5,7,9,10,12]
    month30Days = [4,6,8,10]
    --month28Days = [2]



daysInMonth :: Int -> Int -> Int
daysInMonth month year
    |year `mod` 4 == 0 && month == 2 = 29 
    |year `mod` 4 /= 0 && month == 2 = 28
    |month `elem` month31Days  = 31
    |month `elem` month30Days = 30
    where
    month31Days = [1,3,5,7,9,10,12]
    month30Days = [4,6,8,10]
    

--Written Exercises

-- sumThree x y z = x + y + z                                                           
-- sumThree 3 5 7                                                                        definition
-- 3 + 5 + 7                                                                             arithmetic 
-- 12                                                                                    arithmetic

-- sumThree x y z = x + y + z                                                                                       
-- sumThree 8 (1 + 3) 2                                                                  definition                                
-- 8 + (1 + 3) + 2                                                                       arithmetic
-- 8 + 4 + 2                                                                             arithmetic
-- 14                                                                                    arithmetic

-- threeDifferent x y z = (x /= y) && (x /= z) && (y /= z)                              
-- threeDifferent 1 4 2                                                                 definition 
--                                                                                      
--                                                                                      
--                                                                                      
--                                                                                      
--                                                                                      
--                                                                                      