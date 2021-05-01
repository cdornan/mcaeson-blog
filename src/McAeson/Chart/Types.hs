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
{-# LANGUAGE StandaloneDeriving        #-}

module McAeson.Chart.Types where

import qualified Control.Lens                 as LENS
-- import           Control.Monad
import qualified Data.List                    as L
import           Data.Maybe
import qualified Data.Set                     as Set
import           Data.Text(Text)
-- import qualified Data.Text      as T
import           Data.Time
import           Data.Vector(Vector)
import qualified Data.Vector                  as V
import           Fmt
-- import           McAeson.Bench.Renderable
import           McAeson.Bench.Types
import           McAeson.Query
import           McAeson.Query.Types
-- import           McAeson.Installation.Persistence
import           McAeson.Installation.Types
-- import           McAeson.Types
-- import           Text.Show.Functions()


import           GHC.Generics
import           Text.Enum.Text


root :: Root
root = Root "/Volumes/mcaeson/data"


type Markdown   = Text
type Heading    = Text
type Javascript = Text
type Html       = Text

type Report = [Chart]

data Chart =
  Chart
    { _c_heading :: Heading
    , _c_blurb   :: Markdown
    , _c_series  :: [LabeledQuery]
    , _c_values  :: [LabeledQuery]
    }
  deriving (Show)

data ChartData =
  ChartData
    { _cd_chart :: Chart
    , _cd_data  :: [[Datum]]
    }
  deriving (Show)

data LabeledQuery =
  LabeledQuery
    { _lq_label :: Text
    , _lq_query :: QueryDescriptor
    }
  deriving (Show)

data Datum
  = NoDatum
  | Datum Double
  deriving stock (Show)

data ChartPacket =
  ChartPacket
    { _cp_id :: Text
    , _cp_hg :: Heading
    , _cp_by :: Markdown
    , _cp_js :: Javascript
    }
  deriving (Show)


postReport :: Report -> IO ()
postReport cs = do
    ut  <- getCurrentTime
    cps <- mapM (gen ut) cs
    write_report ut cps
  where
    gen :: UTCTime -> Chart -> IO ChartPacket
    gen ut c = render_chart ut <$> extract c

extract :: Chart -> IO ChartData
extract c = mk <$> mapM gen_series _c_series
  where
    gen_series :: LabeledQuery -> IO [Datum]
    gen_series LabeledQuery{..} = mapM (gen_datum _lq_query) _c_values

    gen_datum :: QueryDescriptor -> LabeledQuery -> IO Datum
    gen_datum qd0 LabeledQuery{..} = calc_output OP_min <$> queryBenchmarksWith True root q
      where
        q = mkQuery $ qd0 <> _lq_query

    Chart{..} = c

    mk :: [[Datum]] -> ChartData
    mk dz =
      ChartData
        { _cd_chart = c
        , _cd_data  = dz
        }


-- renderTable :: ChartData -> Markdown
-- renderTable =

render_chart :: UTCTime -> ChartData -> ChartPacket
render_chart = undefined

write_report :: UTCTime -> [ChartPacket] -> IO ()
write_report = undefined



newtype QueryDescriptor =
  QueryDescriptor
    { getQueryDescriptor :: QueryDescriptorL -> Maybe QueryMethods
    }
  deriving (Show)

instance Monoid QueryDescriptor where
  mempty = QueryDescriptor $ const Nothing

instance Semigroup QueryDescriptor where
  (<>) x y = QueryDescriptor f
    where
      f qdl = case isJust mb of
          True  -> mb
          False -> getQueryDescriptor y qdl
        where
          mb = getQueryDescriptor x qdl

instance IsQuery QueryDescriptor where
  mkQuery (QueryDescriptor f) = mconcat
      [ case qm of
          QueryMethods x -> mkQuery x
        | qdl <- [minBound..maxBound]
        , Just qm <- [f qdl]
        ]



test :: IO ()
test = postReport [test_chart]

test_chart :: Chart
test_chart =
  Chart
    { _c_heading = "test chart"
    , _c_blurb   = "functions and algoritms on giga/dt"
    , _c_series  = al_query every
    , _c_values  = fn_query every
    }



data QueryDescriptorL
  = QD_installation
  | QD_function
  | QD_algorithm
  | QD_input
  | QD_compiler
  | QD_os
  | QD_partitions
  | QD_workers
  | QD_week
  | QD_output
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText QueryDescriptorL

class IsQuery a where
  mkQuery :: a -> Query

class IsBrief a where
  brief :: a -> Text

class (Bounded a,IsBrief a,Buildable a,Enum a,IsQuery a,Show a) => HasQueryMethods a

data QueryMethods = forall a . HasQueryMethods a =>
  QueryMethods
    { _qm_value :: a
    }

deriving instance Show QueryMethods


fn_query :: (Function->Bool) -> [LabeledQuery]
fn_query = gen_labelel_queries QD_function

data Function
  = FN_string_count
  | FN_tsearch
  | FN_simple_json
  | FN_aeson_value
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Function

instance IsBrief Function where
  brief = fmt . build . fn_to_tag

instance IsQuery Function where
  mkQuery = tag_to_query . fn_to_tag

fn_to_tag :: Function -> Tag
fn_to_tag fn = case fn of
    FN_string_count -> TG_sc
    FN_tsearch      -> TG_ts
    FN_simple_json  -> TG_sj
    FN_aeson_value  -> TG_av


al_query :: (Algorithm->Bool) -> [LabeledQuery]
al_query = gen_labelel_queries QD_algorithm

data Algorithm
  = AL_tyro
  | AL_pheres
  | AL_medea
  | AL_aeson
  | AL_jsonsimd
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Algorithm

instance IsBrief Algorithm where
  brief = fmt . build . al_to_tag

instance IsQuery Algorithm where
  mkQuery = tag_to_query . al_to_tag

al_to_tag :: Algorithm -> Tag
al_to_tag = \case
    AL_tyro     -> TG_ty
    AL_pheres   -> TG_ph
    AL_medea    -> TG_ma
    AL_aeson    -> TG_ae
    AL_jsonsimd -> TG_js



data InputFile
  = IF_giga
  | IF_large
  | IF_little
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText InputFile

instance IsBrief InputFile where
  brief = fmt . build . if_to_tag

instance IsQuery InputFile where
  mkQuery = tag_to_query . if_to_tag

if_to_tag :: InputFile -> Tag
if_to_tag = \case
    IF_giga   -> TG_gi
    IF_large  -> TG_lg
    IF_little -> TG_lt

if_query :: (InputFile->Bool) -> [LabeledQuery]
if_query = gen_labelel_queries QD_input



data Output
  = OP_min
  | OP_max
  | OP_mean
  | OP_internal_min
  | OP_internal_max
  | OP_internal_mean
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Output

instance IsBrief Output where
  brief op = case op of
    OP_min            -> "mi"
    OP_max            -> "mx"
    OP_mean           -> "me"
    OP_internal_min   -> "imi"
    OP_internal_max   -> "imx"
    OP_internal_mean  -> "ime"

instance IsQuery Output where
  mkQuery = const mempty

calc_output :: Output -> Vector Benchmark -> Datum
calc_output op = case op of
    OP_min           -> calc_op bm_external_seconds min_1
    OP_max           -> calc_op bm_external_seconds max_1
    OP_mean          -> calc_op bm_external_seconds mean_1
    OP_internal_min  -> calc_op bm_internal_seconds min_1
    OP_internal_max  -> calc_op bm_internal_seconds max_1
    OP_internal_mean -> calc_op bm_internal_seconds mean_1


calc_op :: (Benchmark->Double) -> (Double->[Double]->Double) -> Vector Benchmark -> Datum
calc_op ext agg v0 = case V.uncons v0 of
    Nothing        -> NoDatum
    Just (bm,bm_v) -> Datum $ agg (ext bm) $ V.toList $ V.map ext bm_v

bm_external_seconds :: Benchmark -> Double
bm_external_seconds = (/1000) . fromIntegral . getMilisecondsReal . _bm_real

bm_internal_seconds :: Benchmark -> Double
bm_internal_seconds = getSeconds . _bm_time

min_1 :: Double -> [Double] -> Double
min_1 d ds = L.minimum $ d:ds

max_1 :: Double -> [Double] -> Double
max_1 d ds = L.maximum $ d:ds

mean_1 :: Double -> [Double] -> Double
mean_1 d ds = sum (d:ds) / 1 + L.genericLength ds


tag_to_query :: Tag -> Query
tag_to_query tg = LENS.set q_tags (Set.fromList [tg]) mempty

gen_queries :: HasQueryMethods a => QueryDescriptorL -> (a->Bool) -> [QueryDescriptor]
gen_queries qdl = map _lq_query . gen_labelel_queries qdl

gen_labelel_queries :: HasQueryMethods a => QueryDescriptorL -> (a->Bool) -> [LabeledQuery]
gen_labelel_queries qdl p =
    [ LabeledQuery (fmt_t x) $ QueryDescriptor $ \case
          qdl_ | qdl_==qdl -> Just $ QueryMethods x
               | otherwise -> Nothing
      | x <- [minBound..maxBound]
      , p x
      ]

every :: a -> Bool
every = const True

-- fmt_t :: Buildable a => a -> Text
-- fmt_t = fmt . build
