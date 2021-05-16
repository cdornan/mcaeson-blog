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
  generate nvd3 jsc = gen_html (mk_phi nvd3 jsc) tpl
    where
      tpl = maybe template_single (const template_multi) $ _jsc_yaxis2 jsc

mk_phi :: NVD3 -> JSChart -> Param -> Text
mk_phi _ JSChart{..} p = fmt $ case p of
    P_header        -> build                            _jsc_header
    P_footer        -> build                            _jsc_footer
    P_id            -> build                            _jsc_id
    P_height        -> build                            _jsc_height
    P_width         -> build                            _jsc_width
    P_labels        -> jsArray $ _xaxis_labels          _jsc_xaxis
    P_x_axis_title  -> build   $ _xaxis_title           _jsc_xaxis
    P_y_axis_title  -> build   $ _yaxis_title           _jsc_yaxis
    P_y_axis2_title -> maybe "" (build . _yaxis_title)  _jsc_yaxis2
    P_min_y         -> build   $ min_y (Just u)         _jsc_lines
    P_max_y         -> build   $ max_y (Just u)         _jsc_lines
    P_min_y2        -> build   $ min_y mb_u             _jsc_lines
    P_max_y2        -> build   $ max_y mb_u             _jsc_lines
    P_data          -> d3Data u mb_u                    _jsc_lines
  where
    u :: Unit
    u = _yaxis_unit _jsc_yaxis

    mb_u :: Maybe Unit
    mb_u = _yaxis_unit <$> _jsc_yaxis2

min_y, max_y :: Maybe Unit -> [Line] -> Double
min_y mb_u = maybe (const 0) calc mb_u
  where
    calc :: Unit -> [Line] -> Double
    calc u lns = case [ d | Datum d u' _<-concatMap _line_data lns, u==u' ] of
      [] -> 0
      ds -> min 0 $ minimum ds
max_y mb_u = maybe (const 0) calc mb_u
  where
    calc :: Unit -> [Line] -> Double
    calc u lns = case [ d | Datum d u' _<-concatMap _line_data lns, u==u' ] of
      [] -> 0
      ds -> maximum ds

gen_html :: (Param->Text) -> Template Param -> Html
gen_html phi = Html . subst phi

data Param
  = P_header
  | P_footer
  | P_id
  | P_height
  | P_width
  | P_labels
  | P_x_axis_title
  | P_y_axis_title
  | P_y_axis2_title
  | P_min_y
  | P_max_y
  | P_min_y2
  | P_max_y2
  | P_data
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText Param

template_single :: Template Param
template_single = Template [here|
<<header>>
<div id="<<id>>" style="height:<<height>>;"></div>

<script>

    nv.addGraph(function() {
        var chart = nv.models.lineChart()
                      .useInteractiveGuideline(true)
                      .margin({right: 45})
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
            return d3.format(',.3f')(d);
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

template_multi :: Template Param
template_multi = Template [here|
<<header>>
<div id="<<id>>" style="height:<<height>>;"></div>

<script>

    nv.addGraph(function() {
        var chart = nv.models.multiChart()
                      .useInteractiveGuideline(true)
                      .margin({right: 90})
                      .yDomain1([<<min-y>>,<<max-y>>])
                      .yDomain2([<<min-y2>>,<<max-y2>>]);
        var data;

        var x_format = function(num) {
            var labels = <<labels>>;

            return labels[num];
        };

        var y_format = function(d) {
          if (d==null) {
            return "";
          } else {
            return d3.format(',.3f')(d);
          }
        };

        chart.xAxis
            .axisLabel("<<x-axis-title>>")
            .tickFormat(x_format)
        ;

        chart.yAxis1
            .axisLabel("<<y-axis-title>>")
            .tickFormat(y_format)
        ;

        chart.yAxis2
            .axisLabel("<<y-axis2-title>>")
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
