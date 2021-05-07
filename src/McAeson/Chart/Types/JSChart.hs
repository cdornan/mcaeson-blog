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

import           Data.Text(Text)
import           McAeson.Chart.Types.Basic


class HasJSChart c where
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
