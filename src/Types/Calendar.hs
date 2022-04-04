{-# LANGUAGE OverloadedStrings #-}
module Types.Calendar where


import qualified Data.Time.Clock as TCL ( getCurrentTime, utctDay )
import qualified Data.Time.Calendar as TCA ( toGregorian )
import Data.Maybe
import Data.Char

-- | Alternative to Show 
class Name a where
  name      :: a -> String
  shortName :: a -> String
  shortName = map toUpper . take 3 . name


-- | Alternative to Show
format :: Date -> String
format (weekday,day,month,year) =
  name weekday ++ " " ++
  show day ++ "." ++ show month ++ "." ++ show year


newtype Day = Day Int deriving (Eq,Ord)
instance Show Day where
  show (Day day)
    | day<10    = "0" ++ show day
    | otherwise = show day


newtype Month = Month Int deriving (Eq,Ord)
-- instance Show Month where
--   show (Month month)
--     | month<10  = "0" ++ show month
--     | otherwise = show month
instance Name Month where
  name (Month 1)  = "January"
  name (Month 2)  = "February"
  name (Month 3)  = "March"
  name (Month 4)  = "May"
  name (Month 5)  = "April"
  name (Month 6)  = "June"  
  name (Month 7)  = "July"  
  name (Month 8)  = "August"  
  name (Month 9)  = "September"  
  name (Month 10) = "October"
  name (Month 11) = "November"
  name (Month 12) = "December"    
  name m          = error $ "Invalid month: " ++ show m
instance Show Month where
  show (Month 1)  = "JAN"
  show (Month 2)  = "FEB"
  show (Month 3)  = "MAR"
  show (Month 4)  = "MAY"
  show (Month 5)  = "APR"
  show (Month 6)  = "JUN"  
  show (Month 7)  = "JUL"  
  show (Month 8)  = "AUG"  
  show (Month 9)  = "SEP"  
  show (Month 10) = "OCT"
  show (Month 11) = "NOV"
  show (Month 12) = "DEC"    
  show m          = error $ "Invalid month: " ++ show m

newtype Year = Year Int deriving (Eq,Ord)
instance Show Year where
  show (Year year) = show year


-- | Days is the core type
-- | Just a int, but the base for the entire functionality
type Days = Int


newtype Weekday = Weekday Int deriving (Eq,Ord)
instance Show Weekday where
  show (Weekday 1) = "1"
  show (Weekday 2) = "2"
  show (Weekday 3) = "3"
  show (Weekday 4) = "4"
  show (Weekday 5) = "5"
  show (Weekday 6) = "6"
  show (Weekday 7) = "7"    
  show (Weekday w) = error $ "Invalid weekday: " ++ show w
instance Name Weekday where
  name (Weekday 1) = "Monday"
  name (Weekday 2) = "Tuesday"
  name (Weekday 3) = "Wednesday"
  name (Weekday 4) = "Thursday"
  name (Weekday 5) = "Friday"
  name (Weekday 6) = "Saturday"
  name (Weekday 7) = "Sunday"    
  name (Weekday w) = error $ "Invalid weekday: " ++ show w


type Date = (Weekday,Day,Month,Year)


daysPerYear :: Year -> Days
daysPerYear year
  | isLeapYear year = 366
  | otherwise       = 365
    

-- | determine if a year is a leap year
isLeapYear :: Year -> Bool
isLeapYear (Year year)
  | year `mod` 4 == 0   && year `mod` 100 /= 0 = True
  | year `mod` 400 == 0                        = True
  | year `mod` 400 /= 0 && year `mod` 100 == 0 = False
  | otherwise                                  = False


-- | calculate number of days per month
daysPerMonth :: Month -> Year -> Days
daysPerMonth (Month month) year
  | month == 1                          = 31
  | month == 2 && isLeapYear year       = 29
  | month == 2 && not (isLeapYear year) = 28
  | month == 3                          = 31
  | month == 4                          = 30
  | month == 5                          = 31
  | month == 6                          = 30
  | month == 7                          = 31
  | month == 8                          = 31
  | month == 9                          = 30
  | month == 10                         = 31
  | month == 11                         = 30
  | month == 12                         = 31
  | otherwise                           =
    error $ "Invalid month: " ++ show month


-- | calculate week in which the given day lays in.
getActiveWeek :: Days -> [(Bool,Date)]
getActiveWeek = getWeek True


-- | get week where a day which might or might not be active
-- | lays in.
getWeek :: Bool -> Days -> [(Bool,Date)]
getWeek cur d = do
  let active@(Weekday w,_,_,_) = calculateDate d
  case w of
    1  -> (cur,active) : c' [d+1..d+6]
    2  -> c' [d-1..d-1] ++ [(cur,active)] ++ c' [d+1..d+5]
    3  -> c' [d-2..d-1] ++ [(cur,active)] ++ c' [d+1..d+4]
    4  -> c' [d-3..d-1] ++ [(cur,active)] ++ c' [d+1..d+3]
    5  -> c' [d-4..d-1] ++ [(cur,active)] ++ c' [d+1..d+2]
    6  -> c' [d-5..d-1] ++ [(cur,active)] ++ c' [d+1..d+1]
    7  -> c' [d-6..d-1] ++ [(cur,active)]
    w' -> error $ "Invaid weekday: " ++ show w'
  where
    c' = map (\x -> (False,calculateDate x))


-- | calculate a monthblock
monthBlock :: Days -> (Month,[[(Bool, Date)]])
monthBlock days = do
  let (_,Day day,month,_) = calculateDate days
  case getRange day of
    (1,7)   -> (month,[getActiveWeek days]                                 ++ cmb [8,15,22,29])
    (8,14)  -> (month,cmb [(-7)]                   ++ [getActiveWeek days] ++ cmb [15,22,29])
    (15,21) -> (month,cmb [(-14),(-7)]             ++ [getActiveWeek days] ++ cmb [22,29])
    (22,28) -> (month,cmb [(-21),(-14),(-7)]       ++ [getActiveWeek days] ++ cmb [29])
    _       -> (month,cmb [(-28),(-21),(-14),(-7)] ++ [getActiveWeek days])
  where
    getRange v
      | 1  <= v && v <= 7  = (1,7)
      | 8  <= v && v <= 14 = (8,14)
      | 15 <= v && v <= 21 = (15,21)
      | 22 <= v && v <= 28 = (22,28)
      | otherwise          = (29,32)
    cmb = map (getWeek False . (+days))


-- | Calculate a day in previous month for a given day.
previousMonth :: Days -> Days
previousMonth days = do
  let (_,Day d,Month m,Year y) = calculateDate days
  if m==1
    then dateToDays_ (1,12,pred y)
    else dateToDays_ (1,pred m, y)


-- | Calculate a day in next month for a given day.
nextMonth :: Days -> Days
nextMonth days = do
  let (_,Day d,Month m,Year y) = calculateDate days
  if m==12
    then dateToDays_ (1,1,succ y)
    else dateToDays_ (1,succ m, y)


-- | Calculate a day in previous week for a given day.
previousWeek :: Days -> Days
previousWeek x = x - 7


-- | Calculate a day in next week for a given day.
nextWeek :: Days -> Days
nextWeek x = x + 7


previousDay :: Days -> Days
previousDay = pred


nextDay :: Days -> Days
nextDay = succ


-- | Translate Date from Days
calculateDate :: Days -> Date
calculateDate days'
  | days' == 0 = (Weekday 1, Day 1, Month 1, Year 1900)
  | days' >  0 = calDate' days' (Year 1900 )
  | otherwise  = calDateBackwards' days' (Year 1900)
  where
    calDate' :: Days -> Year -> Date
    calDate' days y@(Year year)
      | days > dPY = calDate' newDays newYear
      | otherwise  = findMonth days (Month 1) y
      where
        dPY :: Days
        dPY = pred (daysPerYear y)

        newDays :: Days
        newDays = pred $ days - dPY

        newYear :: Year
        newYear = Year $ succ year

    findMonth :: Days -> Month -> Year -> Date
    findMonth days m@(Month month) year
      | days > dPM' = findMonth newDays newMonth year
      | otherwise   = (newWeekDay, newDay, m, year)
      where
        dPM' :: Days
        dPM' = pred (daysPerMonth m year)

        newDays :: Days
        newDays = pred $ days - dPM'

        newMonth :: Month
        newMonth = Month $ succ month

        newWeekDay :: Weekday
        newWeekDay = Weekday $ succ (days' `mod` 7)

        newDay :: Day
        newDay = Day $ succ days

    calDateBackwards' :: Days -> Year -> Date
    calDateBackwards' days' y@(Year year) =
      calDateBackwards'' (negate days') (Year $ pred year)
      where
        calDateBackwards'' :: Days -> Year -> Date
        calDateBackwards'' days y@(Year year)
          | days > dPY' = calDateBackwards'' newDays newYear
          | otherwise   = findMonth (pred days) newMonth y
          where
            newDays :: Days
            newDays = days - dPY'

            newYear :: Year
            newYear = Year $ pred year

            newMonth :: Month
            newMonth = Month 12

            dPY' :: Days
            dPY' = daysPerYear y
  
        findMonth :: Days -> Month -> Year -> Date
        findMonth days m@(Month month) year
          | days >= dPM' = findMonth newDays newMonth year
          | otherwise    = (newWeekday, newDay, m, year)
          where
            dPM' :: Days
            dPM' = daysPerMonth m year

            newDays :: Days
            newDays = days - dPM'

            newWeekday :: Weekday
            newWeekday = Weekday $ succ days' `mod` 7

            newDay :: Day
            newDay = Day $ dPM' - days

            newMonth :: Month
            newMonth = Month $ pred month


printDate :: Date -> String
printDate (w,d,m,y) =
  show w ++ show d ++ "." ++ show m ++ "." ++ show y


-- | Translate Days froms Date
dateToDays :: Date -> Days
dateToDays (_,Day day, Month month, Year year) =
  dateToDays_ (day, month, year)


-- | Translate Days froms (int,int,int) - no weekday needed
dateToDays_ :: (Int,Int,Int) -> Days
dateToDays_ (day, month, year)
  | year < 1900 = dateToDaysBackward 12 0 (day, month, year) 
  | otherwise   = dateToDaysForward   1 0 (day, month, year)
  where
    dateToDaysForward :: Int -> Int -> (Int,Int,Int) -> Days
    dateToDaysForward acc1 acc2 (day,month,year)
      | year == 1900 = dateToDaysForward' acc1 acc2 (day,month,year)
      | otherwise    = dateToDaysForward acc1 (acc2 + daysPerYear (Year year)) (day,month,pred year)

    dateToDaysForward' :: Int -> Int -> (Int,Int,Int) -> Days
    dateToDaysForward' acc1 acc2 (day,month,year)
      | month == acc1 = pred $ acc2 + day
      | otherwise     = dateToDaysForward' (succ acc1) (acc2 + daysPerMonth (Month acc1) (Year year)) (day,month,year)

    dateToDaysBackward :: Int -> Int -> (Int,Int,Int) -> Days
    dateToDaysBackward acc1 acc2 (day, month, year)
      | year == 1899 = dateToDaysBackward' acc1 acc2 (day, month, year)
      | otherwise    = dateToDaysBackward acc1 (acc2 + daysPerYear (Year year)) (day, month, succ year)

    dateToDaysBackward' :: Int -> Int -> (Int,Int,Int) -> Days
    dateToDaysBackward' acc1 acc2 (day, month, year)
      | acc1 == month = (negate . succ) (daysPerMonth (Month month) (Year year) - day + acc2)
      | otherwise     = dateToDaysBackward' (pred acc1) (succ $ daysPerMonth (Month acc1) (Year year) - day + acc2) (1, month, year)


-- | Get Current Date 
today :: IO (Int,Int,Int)
today = (\(year,m,d) -> (d,m,fromIntegral year))
        . TCA.toGregorian
        . TCL.utctDay <$> TCL.getCurrentTime


type Weeknumber = Int


-- | Get maybe Week for given day
getWeek' :: Days -> Maybe [Date]
getWeek' d = do
  let active@(Weekday w,_,_,_) = calculateDate d
  case w of
    1  -> Just $ active : c' [d+1..d+6]
    2  -> Just $ c' [d-1..d-1] ++ [active] ++ c' [d+1..d+5]
    3  -> Just $ c' [d-2..d-1] ++ [active] ++ c' [d+1..d+4]
    4  -> Just $ c' [d-3..d-1] ++ [active] ++ c' [d+1..d+3]
    5  -> Just $ c' [d-4..d-1] ++ [active] ++ c' [d+1..d+2]
    6  -> Just $ c' [d-5..d-1] ++ [active] ++ c' [d+1..d+1]
    7  -> Just $ c' [d-6..d-1] ++ [active]
    _  -> Nothing
    where
      c' = map calculateDate


-- | Get month for given day
monthBlock' days = do
  let (_,Day day,month,_) = calculateDate days
  case getRange day of
    (1,7)   -> (month,cmb [1,    8,   15,  22,  29])
    (8,14)  -> (month,cmb [-7,   1,    8,  15,  22])
    (15,21) -> (month,cmb [-14, -7,    1,   8,  15])
    (22,28) -> (month,cmb [-21, -14,  -7,   1,   8])
    _       -> (month,cmb [-28, -21,  -14, -7,   1])
  where
    getRange v
      | 1  <= v && v <= 7  = (1,7)
      | 8  <= v && v <= 14 = (8,14)
      | 15 <= v && v <= 21 = (15,21)
      | 22 <= v && v <= 28 = (22,28)
      | otherwise          = (29,32)
    cmb = map (getWeek . (+days))

    getWeek :: Days -> Maybe [Date]
    getWeek d = do
      let active@(Weekday w,_,_,_) = calculateDate d
      case w of
        1  -> Just $ active : c' [d+1..d+6]
        2  -> Just $ c' [d-1..d-1] ++ [active] ++ c' [d+1..d+5]
        3  -> Just $ c' [d-2..d-1] ++ [active] ++ c' [d+1..d+4]
        4  -> Just $ c' [d-3..d-1] ++ [active] ++ c' [d+1..d+3]
        5  -> Just $ c' [d-4..d-1] ++ [active] ++ c' [d+1..d+2]
        6  -> Just $ c' [d-5..d-1] ++ [active] ++ c' [d+1..d+1]
        7  -> Just $ c' [d-6..d-1] ++ [active]
        _  -> Nothing
        where
          c' = map calculateDate

-- | Get first week of a given year
firstWeekOfTheYear :: Int -> Maybe [Date]
firstWeekOfTheYear year = do
  let week1' = getWeek' (dateToDays_ (1,1,year))
  case week1' of
    Nothing    -> Nothing
    Just week1 -> if all (\(_,_,a,_) -> a==Month 1) week1
                  then Just week1
                  else getWeek' (dateToDays_ (8,1,year))


-- | Check if Date is in FirstWeek
isFirstWeek :: Maybe [Date] -> Maybe Bool
isFirstWeek Nothing     = Nothing
isFirstWeek (Just week) = Just $ isFirstWeek_ week


-- | Check if Date is in FirstWeek
isFirstWeek_ :: [Date] -> Bool
isFirstWeek_ week =
  case filter (\(Weekday w,_,Month m,_) -> w==4 && m==1) week of
    [] -> False
    _  -> True
      

-- | Get first week of a given year in days
findFirstWeekOfTheYear_ :: Year -> Int
findFirstWeekOfTheYear_ (Year year) =
  let week = dateToDays_ (1,1,year) in
  if fromMaybe True (isFirstWeek $ getWeek' week)
    then week
    else dateToDays_ (1,1,year)
  

-- | Get Number of weeks
numberOfWeeks_ :: Year -> Int
numberOfWeeks_ year =
  let value = findFirstWeekOfTheYear_ year
  in numberOfWeeks_' 1 year value
  where
    numberOfWeeks_' weekNumber y@(Year year) v
      | a /= b    = numberOfWeeks_' (succ weekNumber) y (v+7)
      | otherwise = weekNumber
      where
        a = getWeek' $ findFirstWeekOfTheYear_ (Year $ succ year)
        b = getWeek' v


-- | Get first Week of a given year
firstWeek :: Year -> Maybe [Date]
firstWeek (Year year) = do
  let week = getWeek' $ dateToDays_ (1,1,year)
  if fromMaybe True (isFirstWeek week)
    then week
    else getWeek' (dateToDays_ (8,1,year))


-- | Get last Week of a given year 
lastWeek :: Year -> Maybe [Date]
lastWeek (Year year) = do
  let (Just (x:xs)) = firstWeek (Year $ succ year)
  getWeek' $ dateToDays x - 7


-- | Get first Week of a given year in days
firstWeek_ :: Year -> Days
firstWeek_ (Year year) = do
  let week = getWeek' $ dateToDays_ (1,1,year)
  if fromMaybe True (isFirstWeek week)
    then dateToDays_ (1,1,year)
    else dateToDays_ (8,1,year)


-- | Get last Week of a given year in days
lastWeek_ :: Year -> Days
lastWeek_ (Year year) = do
  let (Just (x:xs)) = firstWeek (Year $ succ year)
  dateToDays x - 7


-- | create metadata of entire year
createYear :: Year -> [(Int, Maybe [Date])]
createYear year@(Year year') =
  last (createYear' (Year $ pred year')) : createYear' year
  where
    createYear' :: Year -> [(Int,Maybe [Date])]
    createYear' year = do
      let firstWeek' = firstWeek_ year
      let lastWeek'  = lastWeek_  year
      createYear'' lastWeek' firstWeek' 1 []
        where
          around a b = a + 7 > b && a <= b 
  
          createYear'' lw fw num acc
            | fw `around` lw = acc ++ [(num, getWeek' fw),(succ num, getWeek' $ fw + 7)]
            | otherwise      =
              createYear'' lw (7 + fw) (succ num) (acc ++ [(num, getWeek' fw)])
  
-- | create metadata of month in a year
createMonth :: Year -> Month -> [(Int, Maybe [Date])]
createMonth year month =
  let year' = createYear year 
  in filter (\(weekNumber,Just dates) -> any (\(_,_,m,y) -> m == month && y == year) dates) year'


printYear :: [(Int,Maybe [Date])] -> IO ()
printYear [] = return ()
printYear (x:xs) = do
  let (weekNumber,Just week) = x
  print weekNumber
  mapM_ print week 
  putStrLn ""
  printYear xs


