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
-- import           System.FilePath


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
        d <- calcOutput OP_best <$> queryBenchmarksWith QS_explicit root q
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
    []  -> error "mk_js_chart: no units"
    [u] ->
      JSChart
        { _jsc_id     = ID $ "mcchart"+|i|+""
        , _jsc_height = "1000px"
        , _jsc_width  = "1000px"
        , _jsc_header = header
        , _jsc_footer = ""
        , _jsc_xaxis  = xaxis
        , _jsc_yaxis  = yaxis
        , _jsc_yaxis2 = Nothing
        , _jsc_lines  = lnes
        }
      where
        header = Html $ fmt $ unlines_b
          [ "<h1>"+|_c_heading|+"</h1>"
          , "<p>"+|_c_blurb|+"</p>"
          ]
        xaxis =
          XAxis
            { _xaxis_title  = _lqs_label _c_values
            , _xaxis_labels = map _lq_label $ _lqs_queries _c_values
            }

        yaxis =
          YAxis
            { _yaxis_title = fmt $ build u
            }

        lnes :: [Line]
        lnes = zipWith mk (_lqs_queries _c_series) _cd_data
          where
            mk :: LabeledQuery -> [Datum] -> Line
            mk LabeledQuery{..} ds =
                Line
                  { _line_label   = _lq_label
                  , _line_yaxis2  = False
                  , _line_data    = ds
                  }

        Chart{..} = _cd_chart
    _ -> undefined -- multigraph


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
    , _c_universe =
        Universe
          { _uni_units   = [U_GiBps]
          , _uni_queries = LabeledQueries "giga/dt" (Set.fromList [OP_best]) $ concat
              [ _lqs_queries $ me_query (==ME_dt)
              , _lqs_queries $ if_query (==IF_giga)
              ]
          }
    , _c_series   = al_query every
    , _c_values   = fn_query every
    }


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

unlines_b :: [Builder] -> Builder
unlines_b = unlinesF
