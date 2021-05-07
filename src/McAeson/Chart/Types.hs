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

import           Data.Time
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.Chart
import           McAeson.Query


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
    gen_datum qd0 LabeledQuery{..} = calcOutput OP_min <$> queryBenchmarksWith True root q
      where
        q = mkQuery $ qd0 <> _lq_query

    Chart{..} = c

    mk :: [[Datum]] -> ChartData
    mk dz =
      ChartData
        { _cd_chart = c
        , _cd_data  = dz
        }


----------------------------------------------------------------------------------------------------
-- render_table, render_chart, write_report
----------------------------------------------------------------------------------------------------

write_report :: UTCTime -> [ChartData] -> IO ()
write_report = undefined


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
    , _c_universe = universe "giga/dt" $ me_query (==ME_dt) ++ if_query (==IF_giga)
    , _c_series   = al_query every
    , _c_values   = fn_query every
    }
