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

instance HasJSChart NVD3 where
  generate nvd3 = gen_html . mk_sigma nvd3

mk_sigma :: NVD3 -> JSChart -> Param -> Text
mk_sigma _ JSChart{..} p = fmt $ case p of
    P_header        -> build _jsc_header
    P_footer        -> build _jsc_header
    P_id            -> build _jsc_id
    P_height        -> build _jsc_height
    P_width         -> build _jsc_width
    P_labels        -> jsArray $ _xaxis_labels _jsc_xaxis
    P_x_axis_title  -> build   $ _xaxis_title _jsc_xaxis
    P_y_axis_title  -> build   $ _yaxis_title _jsc_yaxis
    P_data          -> d3Data _jsc_lines

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
        var chart;
        var data;

        chart = nv.models.lineChart()
        ;

        var x_format = function(num) {
            var labels = <<labels>>;

            return labels[num];
        };

        chart.xAxis
            .axisLabel("<<x-axis-title>>")
            .tickFormat(x_format);
        ;

        chart.yAxis
            .axisLabel("<<y-axis-title>>")
        ;

        data =
          <<data>>
          ;

        d3.select('#chart3').append('svg')
            .datum(data)
            .call(chart);

        nv.utils.windowResize(chart.update);

        return chart;
    });

</script>
<<footer>>
|]
