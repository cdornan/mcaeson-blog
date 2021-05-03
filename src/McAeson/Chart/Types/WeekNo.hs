{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE OverloadedStrings              #-}

module McAeson.Chart.Types.WeekNo
  ( WeekNo(..)
  , yearWeek
  , getCurrentWeek
  , weekStart
  , dayWeek
  , dayWeek'
  , weekDay
  ) where

import           Data.Char
import           Data.Maybe
import           Data.Possibly
import           Data.Text(Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Fmt
import           Text.Enum.Text


data WeekNo =
  WeekNo
    { _wn_year :: Integer
    , _wn_week :: Int
    }
  deriving (Show)

instance Bounded WeekNo where
  minBound = WeekNo 2021 16
  maxBound = WeekNo 2022 1

instance Enum WeekNo where
  toEnum   = to_week_no
  fromEnum = from_week_no

instance Buildable WeekNo where
  build (WeekNo yr wn) = ""+|yr|+"-W"+|padLeftF 2 '0' wn|+""

instance TextParsable WeekNo where
  parseText txt = case T.splitOn "-" txt of
    [y_t,ww_t]
      | Just ('w',w_t) <- T.uncons $ T.map toLower ww_t
      , Right yr <- parse_year y_t
      , Right wn <- parseText  w_t
      , Just _  <- fromWeekDateValid yr wn 1 ->
        Right $ WeekNo yr wn
    _ -> Left $ "bad year syntax: "+|txt|+""

yearWeek :: Integer -> Int -> Maybe WeekNo
yearWeek y w = dayWeek <$> fromWeekDateValid y w 1

getCurrentWeek :: IO WeekNo
getCurrentWeek = dayWeek . utctDay <$> getCurrentTime

dayWeek :: Day -> WeekNo
dayWeek = fst . dayWeek'

dayWeek' :: Day -> (WeekNo,Int)
dayWeek' dy = (WeekNo y w,d)
  where
    (y,w,d) = toWeekDate dy

weekStart :: WeekNo -> Day
weekStart = flip weekDay 1

weekDay :: WeekNo -> Int -> Day
weekDay (WeekNo y w) d = fromMaybe oops $ fromWeekDateValid y w d
  where
    oops = error $ "weekDay: invalid week: " ++ show (y,w,d)


from_week_no :: WeekNo -> Int
from_week_no wn = to_int $ weekStart wn `diffDays` day0 `div` 7

to_week_no :: Int -> WeekNo
to_week_no i = dayWeek $ addDays (from_int i*7) day0


day0 :: Day
day0 = fromWeekDate yr wn 1
  where
    WeekNo yr wn = minBound



parse_year :: Text -> Possibly Integer
parse_year = fmap from_int . parseText

to_int :: Integer -> Int
to_int = fromIntegral

from_int :: Int -> Integer
from_int = fromIntegral
