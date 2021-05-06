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
  ( root
  , Chart(..)
  , ChartData(..)
  , LabeledQuery(..)
  , ChartPacket(..)
  , postReport
  , extract
  , QueryDescriptor(..)
  , QueryDescriptorL(..)
  , IsQuery(..)
  , HasQueryMethods
  , IsLabelledQuery(..)
  , IsBrief(..)
  , QueryMethods(..)
  , module McAeson.Chart.Types.Basic
  -- testing
  , test
  ) where

import qualified Control.Lens                 as LENS
import qualified Data.List                    as L
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set                     as Set
import           Data.Text(Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Vector(Vector)
import qualified Data.Vector                  as V
import           Fmt
import           McAeson.Bench.Types
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.WeekNo
import           McAeson.Query
import           McAeson.Installation.Types


import           GHC.Generics
import           Text.Enum.Text


root :: Root
root = Root "/Volumes/mcaeson/data"

data Chart =
  Chart
    { _c_heading  :: Text
    , _c_blurb    :: Markdown
    , _c_universe :: [LabeledQuery]
    , _c_series   :: [LabeledQuery]
    , _c_values   :: [LabeledQuery]
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

data ChartPacket =
  ChartPacket
    { _cp_id :: Text
    , _cp_hg :: Text
    , _cp_by :: Markdown
    , _cp_js :: Javascript
    }
  deriving (Show)


----------------------------------------------------------------------------------------------------
-- postReport
----------------------------------------------------------------------------------------------------

postReport :: [Chart] -> IO ()
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


----------------------------------------------------------------------------------------------------
-- render_table, render_chart, write_report
----------------------------------------------------------------------------------------------------

-- render_table :: ChartData -> Markdown
-- render_table =

render_chart :: UTCTime -> ChartData -> ChartPacket
render_chart = undefined

write_report :: UTCTime -> [ChartPacket] -> IO ()
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
    , _c_universe = me_query (==ME_dt) <> if_query (==IF_giga)
    , _c_series   = al_query every
    , _c_values   = fn_query every
    }


----------------------------------------------------------------------------------------------------
-- QueryDescriptor
----------------------------------------------------------------------------------------------------

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

data QueryDescriptorL
  = QD_installation
  | QD_machine
  | QD_function
  | QD_algorithm
  | QD_input
  | QD_version
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

class HasQueryMethods a => IsLabelledQuery a where
  mkLabeledQueries :: (a->Bool) -> [LabeledQuery]

class IsBrief a where
  brief :: a -> Text

class (Bounded a,IsBrief a,Buildable a,Enum a,IsQuery a,Show a) => HasQueryMethods a

data QueryMethods = forall a . HasQueryMethods a =>
  QueryMethods
    { _qm_value :: a
    }

deriving instance Show QueryMethods


----------------------------------------------------------------------------------------------------
-- Instltn
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Instltn where mkLabeledQueries = il_query

data Instltn
  = IL_dt_8'10'4
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Instltn

instance EnumText Instltn where
  configEnumText _ = version_etc

instance IsBrief Instltn where
  brief = \case
    IL_dt_8'10'4 -> "dt8"

instance IsQuery Instltn where
  mkQuery il = LENS.set q_installations iq mempty
    where
      iq :: [InstallationPrefix]
      iq = [InstallationPrefix $ fmt_t il]

il_query :: (Instltn->Bool) -> [LabeledQuery]
il_query = gen_labeled_queries QD_installation


----------------------------------------------------------------------------------------------------
-- Machine
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Machine where mkLabeledQueries = me_query

data Machine
  = ME_dt
  | ME_ford
  | ME_heartofgold
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Machine

instance IsBrief Machine where
  brief = \case
    ME_dt          -> "dt"
    ME_ford        -> "ford"
    ME_heartofgold -> "hog"

instance IsQuery Machine where
  mkQuery me = LENS.set q_installations iq mempty
    where
      iq :: [InstallationPrefix]
      iq = [InstallationPrefix $ fmt_t me]

me_query :: (Machine->Bool) -> [LabeledQuery]
me_query = gen_labeled_queries QD_machine


----------------------------------------------------------------------------------------------------
-- Function
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Function where mkLabeledQueries = fn_query

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

fn_query :: (Function->Bool) -> [LabeledQuery]
fn_query = gen_labeled_queries QD_function


----------------------------------------------------------------------------------------------------
-- Algorithm
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Algorithm where mkLabeledQueries = al_query

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

al_query :: (Algorithm->Bool) -> [LabeledQuery]
al_query = gen_labeled_queries QD_algorithm


----------------------------------------------------------------------------------------------------
-- InputFile
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery InputFile where mkLabeledQueries = if_query

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
if_query = gen_labeled_queries QD_input


----------------------------------------------------------------------------------------------------
-- McVersion
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery McVersion where mkLabeledQueries = mv_query

mv_query :: (McVersion->Bool) -> [LabeledQuery]
mv_query = gen_labeled_queries QD_version

data McVersion
  = MV_0'0'0      -- == 0.0.0.*
  | MV_0'0'0'1    -- == 0.0.0.1
  | MV_0'0'1      -- == 0.0.1.*
  | MV_0'0'1'0    -- == 0.0.1.0
  | MV_0'1        -- == 0.1.*
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText McVersion

instance EnumText McVersion where
  configEnumText _ = version_etc

instance IsBrief McVersion where
  brief = T.filter (/='.') . fmt . build

instance IsQuery McVersion where
  mkQuery il = LENS.set q_versions mv mempty
    where
      mv :: [VersionPrefix]
      mv = [either error id $ parseText $ fmt_t il]


----------------------------------------------------------------------------------------------------
-- Compiler
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Compiler where mkLabeledQueries = cr_query

data Compiler
  = CR_8'6'5
  | CR_8'8'1
  | CR_8'8'2
  | CR_8'8'3
  | CR_8'8'4
  | CR_8'10'1
  | CR_8'10'2
  | CR_8'10'3
  | CR_8'10'4
  | CR_9'0'1
  | CR_9'2'1
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Compiler

instance EnumText Compiler where
  configEnumText _ = version_etc

instance IsBrief Compiler where
  brief = T.filter (/='.') . fmt . build

instance IsQuery Compiler where
  mkQuery il = LENS.set q_compilers gv mempty
    where
      gv :: [GHCVersionPrefix]
      gv = [either error id $ parseText $ fmt_t il]

cr_query :: (Compiler->Bool) -> [LabeledQuery]
cr_query = gen_labeled_queries QD_compiler


----------------------------------------------------------------------------------------------------
-- OS
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery OS where mkLabeledQueries = os_query

data OS
  = OS_Darwin_19'6'0
  | OS_Darwin_20'3'0
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText OS

instance EnumText OS where
  configEnumText _ = version_etc

instance IsBrief OS where
  brief = \case
    OS_Darwin_19'6'0 -> "10.15" -- .7 (Big Sur)  (dt)
    OS_Darwin_20'3'0 -> "11.2"  -- .3 (Cataline) (ford)

instance IsQuery OS where
  mkQuery os = LENS.set q_os gv mempty
    where
      gv :: [OSVersionPrefix]
      gv = [either error id $ parseText $ fmt_t os]

os_query :: (OS->Bool) -> [LabeledQuery]
os_query = gen_labeled_queries QD_os


----------------------------------------------------------------------------------------------------
-- Partitions
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Partitions where mkLabeledQueries = ps_query

newtype Partitions = Partitions { getPartitions :: PartNo }
  deriving stock (Show)
  deriving newtype (Buildable,Enum,Eq,Ord,Num)
  deriving anyclass (HasQueryMethods)

instance Bounded Partitions where
  minBound = 1
  maxBound = 1000

instance IsBrief Partitions where
  brief = fmt . build

instance IsQuery Partitions where
  mkQuery (Partitions ps) = LENS.set q_partitions st mempty
    where
      st :: Set PartNo
      st = Set.fromList [ps]

ps_query :: (Partitions->Bool) -> [LabeledQuery]
ps_query = gen_labeled_queries QD_partitions


----------------------------------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Workers where mkLabeledQueries = ws_query

newtype Workers = Workers { getWorkers :: WorkerID }
  deriving stock (Show)
  deriving newtype (Buildable,Enum,Eq,Ord,Num)
  deriving anyclass (HasQueryMethods)

instance Bounded Workers where
  minBound = 1
  maxBound = 1000

instance IsBrief Workers where
  brief = fmt . build

instance IsQuery Workers where
  mkQuery (Workers ws) = LENS.set q_workers st mempty
    where
      st :: Set WorkerID
      st = Set.fromList [ws]

ws_query :: (Workers->Bool) -> [LabeledQuery]
ws_query = gen_labeled_queries QD_workers


----------------------------------------------------------------------------------------------------
-- Week
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery WeekNo where mkLabeledQueries = wn_query

instance HasQueryMethods WeekNo

instance IsBrief WeekNo where
  brief = fmt . build

instance IsQuery WeekNo where
  mkQuery wn = LENS.set q_sessions (Just sr) mempty
    where
      sr :: SessionRange
      sr = SessionRange (Just $ ins_sq start) (Just $ ins_sq end)

      start, end :: UTCTime
      start =                 ins_u $ weekStart        wn
      end   = addUTCTime nd $ ins_u $ weekStart $ succ wn

      ins_sq :: UTCTime -> SessionQuery
      ins_sq ut = SessionQuery (Session ut) Nothing

      ins_u :: Day -> UTCTime
      ins_u = flip UTCTime 0

      nd :: NominalDiffTime
      nd = negate $ toEnum 1

wn_query :: (WeekNo->Bool) -> [LabeledQuery]
wn_query = gen_labeled_queries QD_week


----------------------------------------------------------------------------------------------------
-- Output
----------------------------------------------------------------------------------------------------

instance IsLabelledQuery Output where mkLabeledQueries = op_query

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

op_query :: (Output->Bool) -> [LabeledQuery]
op_query = gen_labeled_queries QD_output


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

tag_to_query :: Tag -> Query
tag_to_query tg = LENS.set q_tags (Set.fromList [tg]) mempty

gen_labeled_queries :: HasQueryMethods a => QueryDescriptorL -> (a->Bool) -> [LabeledQuery]
gen_labeled_queries qdl p =
    [ LabeledQuery (fmt_t x) $ QueryDescriptor $ \case
          qdl_ | qdl_==qdl -> Just $ QueryMethods x
               | otherwise -> Nothing
      | x <- [minBound..maxBound]
      , p x
      ]

version_etc :: EnumTextConfig
version_etc = defaultEnumTextConfig
    { _etc_char_prep = ecp
    }
  where
    ecp :: Char -> Char
    ecp = \case
      '_'  -> '-'
      '\'' -> '.'
      c    -> c

every :: a -> Bool
every = const True

-- fmt_t :: Buildable a => a -> Text
-- fmt_t = fmt . build
