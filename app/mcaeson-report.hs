{-# LANGUAGE OverloadedStrings #-}

module Main(main,view_q) where

-- import           Control.Monad
import           Data.Text(Text)
-- import qualified Data.Text      as T
import           Data.Time
import qualified Data.Vector                as V
import           Fmt
import           McAeson.Bench.Renderable
import           McAeson.Bench.Types
import           McAeson.Chart.Types
import           McAeson.Query
-- import           McAeson.Query.Types
-- import           McAeson.Installation.Persistence
-- import           McAeson.Installation.Types


-- type Markdown = Text
type Label    = Text

data Report =
  Report
    { _report_title      :: Text
    , _report_decription :: Markdown
    , _report_queries    :: [(Label,Query)]
    }
  deriving (Show)

reports :: [Report]
reports = [tyro_nt]

tyro_nt :: Report
tyro_nt =
  Report
    { _report_title      = "Tyro on NT"
    , _report_decription = "Peak tyro performance on a large multi-core machine."
    , _report_queries    =
        [ (,) "nt:query" "installation:cdornan/dt-8.10.4 ts-ty-gi quiet"
        ]
    }

-- root :: Root
-- root = Root "/Volumes/mcaeson/data"

-- test :: IsQuery a => a -> IO ()
-- test =
--

view_q :: Query -> IO ()
view_q q = do
    bm_v   <- queryBenchmarksWith True root q
    mk_rbm <- renderable LM_inter
    V.mapM_ (out mk_rbm) bm_v
  where
    out :: (Benchmark->RenderableBenchmark) -> Benchmark -> IO ()
    out mk = fmt . build . mk



gen_report :: Report -> Markdown
gen_report = undefined Report

aggregate_reports :: UTCTime -> [Markdown] -> Markdown
aggregate_reports = undefined

wr_report :: FilePath -> Markdown -> IO ()
wr_report = undefined

report_path :: UTCTime -> FilePath
report_path = undefined

main :: IO ()
main = do
  ut <- getCurrentTime
  wr_report (report_path ut) $ aggregate_reports ut $ map gen_report reports
