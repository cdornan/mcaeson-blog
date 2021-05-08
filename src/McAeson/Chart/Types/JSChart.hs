{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types.JSChart where

import           Data.Default
import           Data.String
import           Data.Text(Text)
import           McAeson.Chart.Types.Basic


class Default c => HasJSChart c where
  generate :: c -> JSChart -> Html

data JSChart =
  JSChart
    { _jsc_id     :: ID       -- ^ unique DOM id to use for JS chart container
    , _jsc_height :: Height   -- ^ height of JS chart container
    , _jsc_width  :: Width    -- ^ width  of JS chart container
    , _jsc_header :: Html     -- ^ Html preceding the graph
    , _jsc_footer :: Html     -- ^ Html following the graph
    , _jsc_xaxis  :: XAxis    -- ^ x axis parameters, including labels
    , _jsc_yaxis  :: YAxis    -- ^ y axis parameters
    , _jsc_lines  :: [Line]   -- ^ the line-by-lind date (lengths to match length of x axis labels)
    }
  deriving (Show)

data XAxis =
  XAxis
    { _xaxis_title  :: Text   -- ^ title for x axis
    , _xaxis_labels :: [Text] -- ^ list of labels, to match number of data points in each Line
    }
  deriving (Show)

data YAxis =
  YAxis
    { _yaxis_title :: Text    -- ^ vertically oriented title for Y axis
    }
  deriving (Show)

data Line =
  Line
    { _line_label :: Text     -- ^ label for the line
    , _line_data  :: [Datum]  -- ^ the data (to match numbet of x-axis labels)
    }
  deriving (Show)

-- | just for testing
instance Default JSChart where
  def =
    JSChart
      { _jsc_id     = "chart0"
      , _jsc_height = "1000px"
      , _jsc_width  = "1000px"
      , _jsc_header = "<h2>test chart0</h2>"
      , _jsc_footer = ""
      , _jsc_xaxis  = XAxis "x thingies" [fromString [c] | c<-['a'..'z']]
      , _jsc_yaxis  = YAxis "y thingies"
      , _jsc_lines  = [l1,l2,l3]
      }
    where
      l1, l2, l3 :: Line

      l1 = Line "line 1" $ map f1 is

      l2 = Line "line 2" $ map f2 is

      l3 = Line "line 3" $ map f3 is

      f1, f2, f3 :: Int -> Datum

      f1 i = if i `mod` 10 == 0 then NoDatum else Datum $ fromIntegral i

      f2 i = Datum $ fromIntegral $ 2*i

      f3 i = Datum $ fromIntegral $ 3*i

      is :: [Int]
      is = [1..26]
