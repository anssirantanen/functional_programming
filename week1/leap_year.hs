leapYear:: Int -> Bool
leapYear y 
    | y `mod` 4 == 0 = 
        case y `mod` 100  == 0 of
            True -> y `mod` 400 == 0
            False -> True
    | otherwise = False
