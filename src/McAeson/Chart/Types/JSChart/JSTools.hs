{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types.JSChart.JSTools
  ( jsArray
  , d3Data
  ) where

import           Data.Text(Text)
import           Fmt
import           McAeson.Chart.Types.JSChart
import           McAeson.Chart.Types.JSChart.Template
import           Text.Enum.Text
import           Text.Heredoc


jsArray :: [Text] -> Builder
jsArray = build . show

d3Data :: [Line] -> Builder
d3Data lns =
    (<>"          ];\n") $ mconcat $ zipWith d3d lns $ '[' : repeat ','
  where
    d3d :: Line -> Char -> Builder
    d3d Line{..} c = build $ subst sigma line_t
      where
        sigma :: LineParam -> Text
        sigma = fmt . \case
          LP_start_char -> build c
          LP_data       -> listF _line_data
          LP_label      -> build _line_label

data LineParam
  = LP_start_char
  | LP_data
  | LP_label
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText LineParam

line_t :: Template LineParam
line_t = Template [here|
          <<start-char>> { values      : <<data>>
            , key         : "<<label>>""
            , strokeWidth : 3.5
            }
|]
