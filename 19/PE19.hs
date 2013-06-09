module PE19 where


numSundayAtFirstDayOfMonth :: Int -> Int -> Int
numSundayAtFirstDayOfMonth start end 
    | start > end || start < 1900 = error "end must be larger than start. Further, start must be >= 1900."
    | otherwise                   = (length . (filter (==0)) . (scanl (\x y -> (x + y) `mod` 7) firstDay ) . init) daysInMonths
    where  daysInMonths = (concat . (map daysInEachMonth)) [start..end]
           firstDay = dayOfWeekOnJan1st start

daysInEachMonth :: Int -> [Int]
daysInEachMonth year
    | isLeap year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    | otherwise   = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where isLeap :: Int -> Bool
          isLeap year = year `mod` 400 ==0 || and [year `mod` 4 ==0, year `mod` 100 /= 0]

dayOfWeekOnJan1st :: Int -> Int
dayOfWeekOnJan1st year = (1 + (sum . map (diffMaker)) [1901..year]) `mod` 7
                  where diffMaker = sum . (map (`mod` 7)) . daysInEachMonth