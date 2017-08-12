module TimeTracker
    (
    ) where
import Data.String

data Year = Year Integer [Month] deriving (Show)
data Month = Month MonthName [Week]
data Week = Week WeekNumber [Day]
data Day = Day Date DayOfWeek WorkingHours
data DayOfWeek = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag deriving (Show)
data MonthName = Januar | Februar | März | April | Mai | Juni | Juli | August | September | Oktober | Novermber | Dezember  deriving (Show, Eq)
data WorkingHours = WorkingHours [TimeSegment] deriving (Show)
data TimeSegment = TimeSegment Start End Description | Pause Start End deriving (Show)
type Start = Integer
type End = Integer
type Description = String
type WeekNumber = Integer
type Name = String
type Date = Integer

instance Show Month where
  show (Month monthName weeks) = "\n" ++ show monthName ++ " " ++ show weeks

instance Show Week where
  show (Week weekNumber days) = "\n\t"++ show weekNumber ++ ". Woche " ++ show days

instance Show Day where
  show (Day date dayOfWeek workingHours) = "\n\t\t" ++ (show date) ++ ". Tag ("++ (show dayOfWeek) ++ ") \t" ++ (show workingHours)

monthTable :: [MonthName]
monthTable = [März, April, Mai, Juni, Juli, August, September, Oktober, Novermber, Dezember, Januar, Februar]

getMonthName :: Integer -> MonthName
getMonthName nth = monthTable !! ((fromInteger nth) - 1)

getMonthNumber :: MonthName -> Integer
getMonthNumber name = (indexOf name monthTable) + 1

getYearForMonth :: Integer -> MonthName -> Integer
getYearForMonth year monthName = case monthName of
  Januar -> year - 1
  Februar -> year - 1
  otherwise -> year

generateWeek :: Integer -> MonthName -> Integer -> [Integer] -> Week
generateWeek year month weekNumer days = Week weekNumer (map (generateDay year month) days)

generateDay :: Integer -> MonthName -> Integer -> Day
generateDay year month date = Day date (weekday date month year) (WorkingHours [])

getWeekDay :: Integer -> DayOfWeek
getWeekDay number = case number of
  0 -> Sonntag
  1 -> Montag
  2 -> Dienstag
  3 -> Mittwoch
  4 -> Donnerstag
  5 -> Freitag
  6 -> Samstag

generateMonth :: Integer -> MonthName -> Integer -> Month
generateMonth year name lastDay = Month name (generateWeeks year name 1 [1..lastDay])
--
generateWeeks :: Integer -> MonthName -> Integer -> [Integer] -> [Week]
generateWeeks _ _ _ [] = []
generateWeeks year monthName week rest = [ generateWeek year monthName week (take 7 rest) ] ++ generateWeeks year monthName (week + 1) (drop 7 rest)

weekday :: Integer -> MonthName -> Integer -> DayOfWeek
weekday d month year = getWeekDay (mod z 7)
  where
    z = (d + (floor ((2.6 * (fromInteger m)) - 0.2)) + y + (div y 4) + (div c 4) - (2 * c))
    c' = getYearForMonth year (getMonthName m)
    y =  (read . (drop 2) . show $ c') :: Integer
    c =  (read . (take 2) . show $ c') :: Integer
    m = getMonthNumber month

indexOf :: (Eq a) => a -> [a] -> Integer
indexOf e (x:xs) = indexOf' e (x:xs) 0 where
  indexOf' _ [] _ = -1
  indexOf' e (x:xs) index = case e == x of
    True -> index
    False -> indexOf' e xs (index + 1)

year2017 = [(Januar, 31), (Februar, 28), (März, 31), (April, 30), (Mai, 31), (Juni, 30), (Juli, 31), (August, 31), (September, 30), (Oktober, 31), (Novermber, 30), (Dezember, 31)]

generateYear :: Integer -> [(MonthName, Integer)] -> Year
generateYear year monthList = Year year (foldr (generateYear' year) [] monthList)
  where
    generateYear' year (f, s) a = [(generateMonth year f s)] ++ a

kalender2017 = generateYear 2017 year2017

-- addHours :: Year -> TimeSegment -> Year
