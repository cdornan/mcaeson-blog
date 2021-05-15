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

import qualified Data.List                            as L
import           Data.Maybe
import           Data.Text(Text)
import           Fmt
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.JSChart
import           McAeson.Chart.Types.JSChart.Template
import           Text.Enum.Text
import           Text.Heredoc


jsArray :: [Text] -> Builder
jsArray = build . show

d3Data :: Unit -> Maybe Unit -> [Line] -> Builder
d3Data u mbu lns = (<>"          ];\n") $ mconcat $ zipWith d3d lns $ '[' : repeat ','
  where
    d3d :: Line -> Char -> Builder
    d3d Line{..} c = build $ subst phi tpl
      where
        tpl = case us of
          [_] -> line_sgl_t
          _   -> line_mul_t

        phi :: LineParam -> Text
        phi = fmt . \case
            LP_start_char -> build   c
            LP_data       -> listF $ zipWith mk_point [0..] _line_data
            LP_label      -> build   _line_label
            LP_yaxis      -> build   y_axis_no

        -- yaxis number (1 or 2) for this line (see @us@, below)
        y_axis_no :: Int
        y_axis_no = maybe oops (1+) $ L.elemIndex l_u us -- NB 1 or 2
          where
            oops = error $ "Datum Unit "+|l_u|+" outside the graph's universe: "+||us||+""

        -- unit of the current line
        l_u :: Unit
        l_u = case L.nub $ catMaybes $ map f _line_data of
            []   -> u      -- no points, return (arbitrarily) units for axis 1
            [u_] -> u_
            us'  -> error $ "d3Data: Line with multiple units: "+||us'||+"" -- TODO: test
          where
            f :: Datum -> Maybe Unit
            f NoDatum      = Nothing
            f (Datum _ u_) = Just u_

    -- list enumerating units of yAxis1 and yAxis2 (if relevent), respectively
    us :: [Unit]
    us = u : maybe [] (:[]) mbu

    mk_point :: Int -> Datum -> Builder
    mk_point i d = "{x:"+|i|+",y:"+|d|+"}"


data LineParam
  = LP_start_char
  | LP_data
  | LP_label
  | LP_yaxis
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText LineParam

line_sgl_t :: Template LineParam
line_sgl_t = Template [here|
          <<start-char>> { values      : <<data>>
            , key         : "<<label>>"
            , strokeWidth : 3.5
            }
|]

line_mul_t :: Template LineParam
line_mul_t = Template [here|
          <<start-char>> { values      : <<data>>
            , key         : "<<label>>"
            , type        : "line"
            , yaxis       : <<yaxis>>
            , strokeWidth : 3.5
            }
|]
