module PE19 where


numSundayAtFirstDayOfMonth :: Int -> Int -> Int
numSundayAtFirstDayOfMonth start end 
    | start > end || start < 1900 = error "end must be larger than start. Further, start must be >= 1900."
    | otherwise   = (sum . countTheDaysInRange) [start..end]

countTheDaysInRange :: [Int] -> Int
countTheDaysInRange (y:ys) =
    
    where dOfWeekOfThe1stDays :: Int -> Int -> [Int]
          dOfWeekOfThe1stDays dayOfWeek year = scanr dOfWeekNextMonth dayOfWeek (daysInEachMonth year)
          dOfWeekOfThe1stDays 

isLeap :: Int -> Bool
isLeap year = year `mod` 400 ==0 || and [year `mod` 4 ==0, year `mod` 100 /= 0]

dOfWeekNextMonth :: Int -> Int -> Int
dOfWeekNextMonth dayOfWeek daysInAMonth = (dayOfWeek + daysInAMonth) `mod` 7

daysInEachMonth :: Int -> [Int]
daysInEachMonth year
    | isLeap year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    | otherwise   = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]






