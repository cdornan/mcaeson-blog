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
import           Text.Blaze.Html


class HasJSChart c where
  generate :: c -> JSChart -> Html
  generate = undefined



data JSChart =
  JSChart
    { _jsc_header :: Html
    , _jsc_footer :: Html
    , _jsc_xaxis  :: XAxis
    , _jsc_yaxis  :: YAxis
    , _jsc_lines  :: [Line]
    }
--  deriving (Show)

data XAxis =
  XAxis
    { _xaxis_title  :: Text
    , _xaxis_labels :: [Text]
    }
  deriving (Show)

data YAxis =
  YAxis
    { _yaxis_title     :: Text
    }

data Line =
  Line
    { _line_label :: Text
    , _line_data  :: [Datum]
    }
