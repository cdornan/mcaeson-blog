{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types.JSChart.NVD3(NVD3(..)) where

import           Data.Default
import           Data.Text(Text)
import           Fmt
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.JSChart
import           McAeson.Chart.Types.JSChart.JSTools
import           McAeson.Chart.Types.JSChart.Template
import           Text.Heredoc
import           Text.Enum.Text


data NVD3 = NVD3
  deriving (Show)

instance Default NVD3 where
  def = NVD3

instance HasJSChart NVD3 where
  generate nvd3 = gen_html . mk_sigma nvd3

mk_sigma :: NVD3 -> JSChart -> Param -> Text
mk_sigma _ JSChart{..} p = case _jsc_yaxis2 of
  Nothing ->
    fmt $ case p of
      P_header        -> build _jsc_header
      P_footer        -> build _jsc_footer
      P_id            -> build _jsc_id
      P_height        -> build _jsc_height
      P_width         -> build _jsc_width
      P_labels        -> jsArray $ _xaxis_labels                _jsc_xaxis
      P_x_axis_title  -> build   $ _xaxis_title                 _jsc_xaxis
      P_y_axis_title  -> build   $ _yaxis_title                 _jsc_yaxis
      P_min_y         -> build   $ min_y $ concatMap _line_data _jsc_lines
      P_max_y         -> build   $ max_y $ concatMap _line_data _jsc_lines
      P_data          -> d3Data                                 _jsc_lines
  Just _ -> undefined -- multigraph

min_y, max_y :: [Datum] -> Double
min_y ds0 = case [ d | Datum d<-ds0 ] of
  [] -> 0
  ds -> min 0 $ minimum ds
max_y dtms = case [ d | Datum d<-dtms ] of
  [] -> 0
  ds -> maximum ds

gen_html :: (Param->Text) -> Html
gen_html = Html . flip subst template

data Param
  = P_header
  | P_footer
  | P_id
  | P_height
  | P_width
  | P_labels
  | P_x_axis_title
  | P_y_axis_title
  | P_min_y
  | P_max_y
  | P_data
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText Param

template :: Template Param
template = Template [here|
<<header>>
<div id="<<id>>" style="height:<<height>>;width=<<width>>;"></div>

<script>

    nv.addGraph(function() {
        var chart = nv.models.lineChart()
                      .useInteractiveGuideline(true)
                      .yDomain([<<min-y>>,<<max-y>>]);
        var data;

        var x_format = function(num) {
            var labels = <<labels>>;

            return labels[num];
        };

        var y_format = function(d) {
          if (d==null) {
            return "";
          } else {
            return d3.format(',.2f')(d);
          }
        };

        chart.xAxis
            .axisLabel("<<x-axis-title>>")
            .tickFormat(x_format)
        ;

        chart.yAxis
            .axisLabel("<<y-axis-title>>")
            .tickFormat(y_format)
        ;

        data =
          <<data>>
        d3.select('#<<id>>').append('svg')
            .datum(data)
            .call(chart)
        ;

        nv.utils.windowResize(chart.update);

        return chart;
    });

</script>
<<footer>>
|]
