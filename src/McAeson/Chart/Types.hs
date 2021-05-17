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
  , dump_test
  ) where

import           Data.Default
import           Data.List                          as L
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set                           as Set
import           Data.Text(Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Data.Time
import           Fmt
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.Chart
import           McAeson.Chart.Types.JSChart
import           McAeson.Chart.Types.JSChart.NVD3
import           McAeson.Bench.MDTable
import           McAeson.Chart.Types.WeekNo
import           McAeson.Query
import           System.Directory
import           Text.Enum.Text


----------------------------------------------------------------------------------------------------
-- postReport
----------------------------------------------------------------------------------------------------

postReport :: [Chart] -> IO ()
postReport cs = do
    ut <- getCurrentTime
    write_report ut =<< mapM extract cs

extract :: Chart -> IO ChartData
extract c = ChartData c <$> mapM gen_series (_lqs_queries _c_series)
  where
    gen_series :: LabeledQuery -> IO Series
    gen_series LabeledQuery{..} = do
      ds <- mapM (gen_datum _lq_label _lq_query) (_lqs_queries _c_values)
      case L.nub $ catMaybes $ map datumUnit ds of
        []  -> return $ Series U_none ds
        [u] -> return $ Series u      ds
        us  -> fail $ "series "+|_lq_label|+" with no/conflicting units: "+|unwordsF us|+""

    gen_datum :: Text -> QueryDescriptor -> LabeledQuery -> IO Datum
    gen_datum slab qd_s lq = do
        -- dmp "universe" qd_u -- TODO
        -- dmp "series  " qd_s
        -- dmp "values  " qd_v
        op <- case Set.toList $ getOutput qd of
          []   -> fail $ "extract: no outputs: "+|qd|+""
          [op] -> return op
          ops  -> fail $ "extract: multiple outputs: "+|unwordsF ops|+""
        d <- calcOutput op <$> queryBenchmarksWith QS_explicit root q
        fmtLn $
          ""   +|(padRightF 20 ' '   slab        )|+
          " "  +|(padRightF 20 ' ' $ _lq_label lq)|+
          " : "+|(padRightF 20 ' '   d           )|+
          " "  +|                    q            |+
          ""
        return d
      where
        q :: Query
        q  = mkQuery qd

        qd, qd_v, qd_u :: QueryDescriptor
        qd   = qd_u <> qd_s <> qd_v
        qd_u = universe $ _uni_queries _c_universe
        qd_v = _lq_query lq

        -- TODO
        -- dmp :: Builder -> QueryDescriptor -> IO ()
        -- dmp lab qd_ = fmt $ unlinesF
        --   [ "------------"
        --   , lab
        --   , "------------"
        --   , build qd_
        --   , "------------"
        --   , ""
        --   ]

    Chart{..} = c


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
      , ""
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
    _      -> error "graphs with more than three unit types not supported (yet)"
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
            mk_ln :: LabeledQuery -> Series -> Line
            mk_ln LabeledQuery{..} sz =
                Line
                  { _line_label   = _lq_label
                  , _line_data    = _s_data sz
                  }

        Chart{..} = _cd_chart

    yaxis :: Unit -> YAxis
    yaxis u =
      YAxis
        { _yaxis_title = fmt $ build u
        , _yaxis_unit  = u
        }


----------------------------------------------------------------------------------------------------
-- test
----------------------------------------------------------------------------------------------------

enable_multi :: Bool
enable_multi = True

test :: IO ()
test = postReport $ concat
  [ [ test_chart ]
  , [ test_multi_chart | enable_multi ]
  ]

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
      , _uni_queries = LabeledQueries "giga/dt" (Set.fromList [OP_e_best_time]) $ concat
          [ _lqs_queries $ me_query (==ME_dt)
          , _lqs_queries $ if_query (==IF_giga)
          , _lqs_queries $ op_query (==OP_e_best_time)
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
  OP_e_best_time      -> True
  OP_e_wrst_time      -> False
  OP_e_mean_time      -> False
  -- internal times
  OP_i_best_time      -> True
  OP_i_wrst_time      -> False
  OP_i_mean_time      -> False
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
      , _uni_queries = LabeledQueries "sc/giga/dt" (Set.fromList []) $ concatMap _lqs_queries
          [ me_query (==ME_dt)
          , fn_query (==FN_string_count)
          , if_query (==IF_giga)
          , gs_query $ const True
          ]
      }


----------------------------------------------------------------------------------------------------
-- dump_table
----------------------------------------------------------------------------------------------------

dump_test :: IO ()
dump_test = dump_table test_multi_chart

dump_table :: Chart -> IO ()
dump_table c = fmt . dump_table' =<< extract c

dump_table' :: ChartData -> Builder
dump_table' ChartData{..} = unlinesF
    [ "# "+|_c_heading|+""
    , ""
    , build $ getMarkdown _c_blurb
    , ""
    , bld_universe
    , ""
    ] <> markdownTable'' jf build gen [minBound..maxBound] rows
  where
    jf :: Col -> CJ
    jf col = case col of
      COL_series      -> LJust
      COL_category    -> LJust
      COL_value       -> RJust
      COL_unit        -> LJust
      COL_pop         -> RJust
      COL_output      -> LJust
      COL_query       -> LJust

    rows :: [Row]
    rows =
      [ Row srs slq vlq d
        | (srs,slq) <- zip _cd_data (_lqs_queries _c_series)
        , let Series{..} = srs
        , (d,vlq) <- zip _s_data (_lqs_queries _c_values)
        ]

    gen :: Col -> Row -> Builder
    gen col = case col of
      COL_series      -> bld_slb
      COL_category    -> bld_vlb
      COL_value       -> bld_val
      COL_unit        -> bld_vlu
      COL_pop         -> bld_pop
      COL_output      -> bld_out
      COL_query       -> bld_qry

    bld_universe :: Builder
    bld_universe = unlinesF
        [ "universe : "+|_lqs_label _uni_queries|+" ["+|unwordsF _uni_units|+"]" :: Builder
        , "  series : "+|_lqs_label _c_series|+""
        , "  values : "+|_lqs_label _c_values|+""
        ]
      where
        Universe{..} = _c_universe

    bld_slb, bld_vlb, bld_val, bld_vlu, bld_pop, bld_out, bld_qry :: Row -> Builder

    bld_slb Row{..} = ""+|_lq_label _row_series_lq|+" ("+|_s_unit _row_series|+")"
    bld_vlb Row{..} = ""+|_lq_label _row_value_lq|+""
    bld_val Row{..} = build _row_data
    bld_vlu Row{..} = case _row_data of
      NoDatum     -> "-"
      Datum _ u _ -> build u
    bld_pop Row{..} = case _row_data of
      NoDatum     -> "-"
      Datum _ _ p -> build p
    bld_out Row{..} = build_s $ getOutput $ mk_qd Row{..}
    bld_qry Row{..} = build   $ mkQuery   $ mk_qd Row{..}

    mk_qd :: Row -> QueryDescriptor
    mk_qd Row{..} = uni_qd<>srs_qd<>val_qd
      where
        srs_qd, val_qd :: QueryDescriptor
        srs_qd = _lq_query _row_series_lq
        val_qd = _lq_query _row_value_lq

    uni_qd :: QueryDescriptor
    uni_qd = mconcat $ map _lq_query $ _lqs_queries $ _uni_queries _c_universe

    Chart{..} = _cd_chart

    build_s :: Buildable a => Set a -> Builder
    build_s = unwordsF . Set.toList

data Row =
  Row
    { _row_series    :: Series
    , _row_series_lq :: LabeledQuery
    , _row_value_lq  :: LabeledQuery
    , _row_data      :: Datum
    }
  deriving (Show)

data Col
  = COL_series
  | COL_category
  | COL_value
  | COL_unit
  | COL_pop
  | COL_output
  | COL_query
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText Col


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

unlines_b :: [Builder] -> Builder
unlines_b = unlinesF
