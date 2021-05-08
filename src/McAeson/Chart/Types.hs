{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types
  ( postReport
  , extract
  , module McAeson.Chart.Types.Basic
  , module McAeson.Chart.Types.Chart
  -- testing
  , test
  ) where

import           Data.Default
-- import           Data.Text(Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Data.Time
import           Fmt
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.Chart
import           McAeson.Chart.Types.JSChart
import           McAeson.Chart.Types.JSChart.NVD3
import           McAeson.Chart.Types.WeekNo
import           McAeson.Query
import           System.Directory
-- import           System.FilePath


----------------------------------------------------------------------------------------------------
-- postReport
----------------------------------------------------------------------------------------------------

postReport :: [Chart] -> IO ()
postReport cs = do
    ut <- getCurrentTime
    write_report ut =<< mapM extract cs

extract :: Chart -> IO ChartData
extract c = mk <$> mapM gen_series _c_series
  where
    gen_series :: LabeledQuery -> IO [Datum]
    gen_series LabeledQuery{..} = mapM (gen_datum _lq_query) _c_values

    gen_datum :: QueryDescriptor -> LabeledQuery -> IO Datum
    gen_datum qd0 lq = do
        print q
        calcOutput OP_min <$> queryBenchmarksWith QS_explicit root q
      where
        q = mkQuery $ qd0 <> _lq_query lq <> _lq_query _c_universe

    Chart{..} = c

    mk :: [[Datum]] -> ChartData
    mk dz =
      ChartData
        { _cd_chart = c
        , _cd_data  = dz
        }


----------------------------------------------------------------------------------------------------
-- write_report
----------------------------------------------------------------------------------------------------

write_report :: UTCTime -> [ChartData] -> IO ()
write_report ut cds = write_report' ut $ mconcat $ zipWith mk [0..] cds
  where
    mk :: Int -> ChartData -> Html
    mk i = generate nvd3 . mk_js_chart i

    nvd3 :: NVD3
    nvd3 = def

write_report' :: UTCTime -> Html -> IO ()
write_report' ut h = wr =<< post_filepath ut
  where
    wr :: FilePath -> IO ()
    wr fp = T.writeFile fp txt

    txt = mta <> getHtml h

    mta = T.unlines
      [ "---"
      , "title: Dashboard for ("+|wk|+")"
      , "subtl: "+|dy|+""
      , "issue: 2"
      , "---"
      ,""
      ]

    wk = dayWeek dy
    dy = utctDay ut

post_filepath :: UTCTime -> IO FilePath
post_filepath ut = loop 'a'
  where
    loop :: Char -> IO FilePath
    loop c = do
        yup <- doesFileExist fp
        case yup of
          True  -> loop $ succ c
          False -> return fp
      where
        fp :: FilePath
        fp = "posts/"+|dt|+"-data-"+|c|+".mchtml"

    dt :: String
    dt = show $ utctDay ut


----------------------------------------------------------------------------------------------------
-- mk_js_chart
----------------------------------------------------------------------------------------------------

mk_js_chart :: Int -> ChartData -> JSChart
mk_js_chart i ChartData{..} =
  JSChart
    { _jsc_id     = ID $ "mcchart"+|i|+""
    , _jsc_height = "1000px"
    , _jsc_width  = "1000px"
    , _jsc_header = header
    , _jsc_footer = ""
    , _jsc_xaxis  = xaxis
    , _jsc_yaxis  = yaxis
    , _jsc_lines  = lnes
    }
  where
    header = Html $ fmt $ unlines_b
      [ "<h1>"+|_c_heading|+"</h1>"
      , "<p>"+|_c_blurb|+"</p>"
      ]

    xaxis =
      XAxis
        { _xaxis_title  = _c_x_title
        , _xaxis_labels = map _lq_label _c_values
        }

    yaxis =
      YAxis
        { _yaxis_title = _c_y_title
        }

    lnes :: [Line]
    lnes = zipWith mk _c_series _cd_data
      where
        mk :: LabeledQuery -> [Datum] -> Line
        mk LabeledQuery{..} ds =
            Line
              { _line_label = _lq_label
              , _line_data  = ds
              }

    Chart{..} = _cd_chart


----------------------------------------------------------------------------------------------------
-- test
----------------------------------------------------------------------------------------------------

test :: IO ()
test = postReport [test_chart]

test_chart :: Chart
test_chart =
  Chart
    { _c_heading  = "test chart"
    , _c_blurb    = "functions and algoritms on giga/dt"
    , _c_y_title  = "GiB/s"
    , _c_x_title  = "function"
    , _c_universe = universe "giga/dt" $ me_query (==ME_dt) ++ if_query (==IF_giga)
    , _c_series   = al_query every
    , _c_values   = fn_query every
    }


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

unlines_b :: [Builder] -> Builder
unlines_b = unlinesF
