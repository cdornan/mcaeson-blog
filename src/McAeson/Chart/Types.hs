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
import qualified Data.Set                     as Set
import           Data.Text(Text)
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


----------------------------------------------------------------------------------------------------
-- postReport
----------------------------------------------------------------------------------------------------

postReport :: [Chart] -> IO ()
postReport cs = do
    ut <- getCurrentTime
    write_report ut =<< mapM extract cs

extract :: Chart -> IO ChartData
extract c = mk <$> mapM gen_series (_lqs_queries _c_series)
  where
    gen_series :: LabeledQuery -> IO [Datum]
    gen_series LabeledQuery{..} = mapM (gen_datum _lq_label _lq_query) (_lqs_queries _c_values)

    gen_datum :: Text -> QueryDescriptor -> LabeledQuery -> IO Datum
    gen_datum slab qd0 lq = do
        d <- calcOutput OP_e_best <$> queryBenchmarksWith QS_explicit root q
        fmtLn $
          ""   +|(padRightF 20 ' ' slab)          |+
          " "  +|(padRightF 20 ' ' $ _lq_label lq)|+
          " : "+|(padRightF 20 ' ' d)             |+
          " "  +|q                                |+
          ""
        return d
      where
        q = mkQuery $ qd0 <> _lq_query lq <> universe (_uni_queries _c_universe)

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
mk_js_chart i ChartData{..} = case _uni_units $ _c_universe _cd_chart of
    []     -> error "mk_js_chart: no units"
    [u]    -> mk u Nothing
    [u,u'] -> mk u $ Just u'
    _   -> undefined -- multigraph
  where
    mk :: Unit -> Maybe Unit -> JSChart
    mk u mb_u2 =
      JSChart
        { _jsc_id     = ID $ "mcchart"+|i|+""
        , _jsc_height = "1000px"
        , _jsc_width  = "1000px"
        , _jsc_header = header
        , _jsc_footer = ""
        , _jsc_xaxis  = xaxis
        , _jsc_yaxis  = yaxis u
        , _jsc_yaxis2 = yaxis <$> mb_u2
        , _jsc_lines  = lnes
        }
      where
        header = Html $ fmt $ unlines_b
          [ "<h1>"+|_c_heading|+"</h1>"
          , "<p>" +|_c_blurb  |+"</p>"
          ]

        xaxis =
          XAxis
            { _xaxis_title  = _lqs_label _c_values
            , _xaxis_labels = map _lq_label $ _lqs_queries _c_values
            }

        lnes :: [Line]
        lnes = zipWith mk_ln (_lqs_queries _c_series) _cd_data
          where
            mk_ln :: LabeledQuery -> [Datum] -> Line
            mk_ln LabeledQuery{..} ds =
                Line
                  { _line_label   = _lq_label
                  , _line_yaxis2  = False
                  , _line_data    = ds
                  }

        Chart{..} = _cd_chart

    yaxis :: Unit -> YAxis
    yaxis u =
      YAxis
        { _yaxis_title = fmt $ build u
        }


----------------------------------------------------------------------------------------------------
-- test
----------------------------------------------------------------------------------------------------

test :: IO ()
test = postReport [test_chart,test_multi_chart]

test_chart :: Chart
test_chart =
  Chart
    { _c_heading  = "test chart"
    , _c_blurb    = "functions and algoritms on giga/dt (best)"
    , _c_universe = giga_dt_GiBps_universe
    , _c_series   = al_query every
    , _c_values   = fn_query every
    }

giga_dt_GiBps_universe :: Universe
giga_dt_GiBps_universe =
    Universe
      { _uni_units   = [U_GiBps]
      , _uni_queries = LabeledQueries "giga/dt" (Set.fromList [OP_e_best]) $ concat
          [ _lqs_queries $ me_query (==ME_dt)
          , _lqs_queries $ if_query (==IF_giga)
          ]
      }

test_multi_chart :: Chart
test_multi_chart =
  Chart
    { _c_heading  = "test multi chart"
    , _c_blurb    = "algorithm time and space usage on sc/giga/dt (best)"
    , _c_universe = sc_giga_dt_GiBps_universe
    , _c_series   = op_query best_time_and_space
    , _c_values   = al_query every
    }

best_time_and_space :: Output -> Bool
best_time_and_space op = case op of
  -- external times
  OP_e_best           -> True
  OP_e_worst          -> False
  OP_e_mean           -> False
  -- internal times
  OP_i_best           -> True
  OP_i_worst          -> False
  OP_i_mean           -> False
  -- memory: best
  OP_e_best_max_res   -> True
  OP_e_best_reclaims  -> False
  OP_e_best_ctx_sws   -> False
  OP_i_best_allocated -> True
  OP_i_best_copied    -> True
  OP_i_best_num_gcs   -> False
  -- memory: worst
  OP_e_wrst_max_res   -> False
  OP_e_wrst_reclaims  -> False
  OP_e_wrst_ctx_sws   -> False
  OP_i_wrst_allocated -> False
  OP_i_wrst_copied    -> False
  OP_i_wrst_num_gcs   -> False
  -- memory: mean
  OP_e_mean_max_res   -> False
  OP_e_mean_reclaims  -> False
  OP_e_mean_ctx_sws   -> False
  OP_i_mean_allocated -> False
  OP_i_mean_copied    -> False
  OP_i_mean_num_gcs   -> False

sc_giga_dt_GiBps_universe :: Universe
sc_giga_dt_GiBps_universe =
    Universe
      { _uni_units   = [U_GiBps,U_GiB]
      , _uni_queries = LabeledQueries "sc/giga/dt" (Set.fromList []) $ concat
          [ _lqs_queries $ me_query (==ME_dt)
          , _lqs_queries $ if_query (==IF_giga)
          ]
      }


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

unlines_b :: [Builder] -> Builder
unlines_b = unlinesF
