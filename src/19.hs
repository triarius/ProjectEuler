data Date = Date Int Int Int
    deriving Show

nextDate :: Date -> Date
nextDate (Date 31 12 year) = Date 1 1 (year + 1)
nextDate (Date 31 month year) = Date 1 (month + 1) year
nextDate (Date 30 month year) = if thrityDayMonth month then Date 1 (month + 1) year
                                                        else Date 31 month year
nextDate (Date 29 2 year) = Date 1 3 year
nextDate (Date 28 2 year) = if isLeapYear year then Date 29 2 year
                                               else Date 1 3 year
nextDate (Date day 2 year) = Date (day + 1) 2 year
nextDate (Date day month year) = Date (day + 1) month year

isLeapYear :: Int -> Bool
isLeapYear year = year `rem` 400 == 0 || year `rem` 100 /= 0 && year `rem` 4 == 0

thrityDayMonth :: Int -> Bool
thrityDayMonth 4 = True
thrityDayMonth 6 = True
thrityDayMonth 9 = True
thrityDayMonth 11 = True
thrityDayMonth _ = False

twentiethCentury :: [Date]
twentiethCentury = twentiethCentury' [Date 1 1 1901]
    where
        twentiethCentury' :: [Date] -> [Date]
        twentiethCentury' [] = []
        twentiethCentury' dates@(Date 31 12 2000 : _) = dates
        twentiethCentury' dates@(date:_) = twentiethCentury' (nextDate date : dates)

sundays :: [Date] -> [Date]
sundays = sundays' 0
    where
        sundays' :: Int -> [Date] -> [Date]
        sundays' _ [] = []
        sundays' 5 (x:xs) = x : sundays' 6 xs
        sundays' n (_:xs) = sundays' ((n + 1) `rem` 7) xs

answer :: Int
answer = length $ filter (\(Date day _ _) -> day == 1) $ sundays twentiethCentury

main :: IO ()
main = print answer
