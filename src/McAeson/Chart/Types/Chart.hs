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

module McAeson.Chart.Types.Chart where

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
import           GHC.Generics
import           McAeson.Bench.JobTags
import           McAeson.Bench.Types
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.WeekNo
import           McAeson.Query
import           McAeson.Installation.Types
import           Text.Enum.Text


root :: Root
root = Root defaultSharedQueryRoot

data Chart =
  Chart
    { _c_heading  :: Text
    , _c_blurb    :: Markdown
    , _c_universe :: Universe
    , _c_series   :: LabeledQueries
    , _c_values   :: LabeledQueries
    }
  deriving (Show)

data Universe =
  Universe
    { _uni_units   :: [Unit]
    , _uni_queries :: LabeledQueries
    }
  deriving (Show)

data ChartData =
  ChartData
    { _cd_chart :: Chart
    , _cd_data  :: [Series]
    }
  deriving (Show)

data Series =
  Series
    { _s_unit :: Unit
    , _s_data :: [Datum]
    }
  deriving (Show)

data LabeledQueries =
  LabeledQueries
    { _lqs_label   :: Text
    , _lqs_output  :: Set Output
    , _lqs_queries :: [LabeledQuery]
    }
  deriving (Show)

data LabeledQuery =
  LabeledQuery
    { _lq_label :: Text
    , _lq_query :: QueryDescriptor
    }
  deriving (Show)


----------------------------------------------------------------------------------------------------
-- QueryDescriptor
----------------------------------------------------------------------------------------------------

data QueryDescriptor =
  QueryDescriptor
    { getQueryDescriptor :: QueryDescriptorL -> Maybe QueryMethods
    , getOutput          :: Set Output
    }
  deriving (Show)

instance Monoid QueryDescriptor where
  mempty =
    QueryDescriptor
      { getQueryDescriptor = const Nothing
      , getOutput          = Set.empty
      }

instance Semigroup QueryDescriptor where
  (<>) x y =
    QueryDescriptor
      { getQueryDescriptor = f
      , getOutput          = getOutput x <> getOutput y
      }
    where
      f qdl = case isJust mb of
          True  -> mb
          False -> getQueryDescriptor y qdl
        where
          mb = getQueryDescriptor x qdl

instance IsQuery QueryDescriptor where
  mkQuery (QueryDescriptor f _) = mconcat
      [ case qm of
          QueryMethods x -> mkQuery x
        | qdl <- [minBound..maxBound]
        , Just qm <- [f qdl]
        ]

instance Buildable QueryDescriptor where
  build QueryDescriptor{..} = unlinesF $ map b_qd [minBound..maxBound] ++ [b_op]
    where
      b_qd :: QueryDescriptorL -> Builder
      b_qd qdl = b_ln (build qdl) $ case getQueryDescriptor qdl of
        Nothing               -> "-"
        Just (QueryMethods x) -> build x

      b_op :: Builder
      b_op = b_ln "output" $ unwordsF $ Set.toList getOutput

      b_ln :: Builder -> Builder -> Builder
      b_ln lab val = padRightF 12 ' ' lab <> " : " <> val

data QueryDescriptorL
  = QD_installation
  | QD_machine
  | QD_function
  | QD_algorithm
  | QD_input
  | QD_gc_stats
  | QD_version
  | QD_compiler
  | QD_os
  | QD_partitions
  | QD_workers
  | QD_week
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText QueryDescriptorL

class IsQuery a where
  mkQuery :: a -> Query

class HasQueryMethods a => IsLabeledQuery a where
  mkLabeledQueries :: (a->Bool) -> LabeledQueries

class Buildable a => IsBrief a where
  brief :: a -> Text
  brief = fmt . build

class (Bounded a,IsBrief a,Buildable a,Enum a,IsQuery a,Show a) => HasQueryMethods a

data QueryMethods = forall a . HasQueryMethods a =>
  QueryMethods
    { _qm_value :: a
    }

deriving instance Show QueryMethods


----------------------------------------------------------------------------------------------------
-- universe
----------------------------------------------------------------------------------------------------

universe :: LabeledQueries -> QueryDescriptor
universe lqs = mconcat $ map _lq_query $ _lqs_queries lqs


----------------------------------------------------------------------------------------------------
-- Instltn
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Instltn where mkLabeledQueries = il_query

data Instltn
  = IL_dt_8'10'4
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Instltn

instance EnumText Instltn where
  configEnumText _ = versionETC

instance IsBrief Instltn where
  brief = \case
    IL_dt_8'10'4 -> "cdornan/dt8"

instance IsQuery Instltn where
  mkQuery il = LENS.set q_installations iq mempty
    where
      iq :: [InstallationPrefix]
      iq = [InstallationPrefix $ fmt_t il]

il_query :: (Instltn->Bool) -> LabeledQueries
il_query = genEOLabeledQueries QD_installation


----------------------------------------------------------------------------------------------------
-- Machine
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Machine where mkLabeledQueries = me_query

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
      iq = [InstallationPrefix pfx]
        where
          pfx = case me of
            ME_dt           -> "cdornan/dt"
            ME_ford         -> "cdornan/ford"
            ME_heartofgold  -> "cdornan/hog"

me_query :: (Machine->Bool) -> LabeledQueries
me_query = genEOLabeledQueries QD_machine


----------------------------------------------------------------------------------------------------
-- Function
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Function where mkLabeledQueries = fn_query

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
  mkQuery = tagToQuery . fn_to_tag

fn_to_tag :: Function -> Tag
fn_to_tag fn = case fn of
    FN_string_count -> TG_sc
    FN_tsearch      -> TG_ts
    FN_simple_json  -> TG_sj
    FN_aeson_value  -> TG_av

fn_query :: (Function->Bool) -> LabeledQueries
fn_query = genEOLabeledQueries QD_function


----------------------------------------------------------------------------------------------------
-- Algorithm
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Algorithm where mkLabeledQueries = al_query

instance HasQueryMethods Algorithm

instance IsBrief Algorithm where
  brief = fmt . build . al_to_tag

instance IsQuery Algorithm where
  mkQuery = tagToQuery . al_to_tag

al_to_tag :: Algorithm -> Tag
al_to_tag = \case
    AL_tyro     -> TG_ty
    AL_pheres   -> TG_ph
    AL_medea    -> TG_ma
    AL_aeson    -> TG_ae
    AL_simdjson -> TG_js

al_query :: (Algorithm->Bool) -> LabeledQueries
al_query = genEOLabeledQueries QD_algorithm


----------------------------------------------------------------------------------------------------
-- InputFile
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery InputFile where mkLabeledQueries = if_query

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
  mkQuery = tagToQuery . if_to_tag

if_to_tag :: InputFile -> Tag
if_to_tag = \case
    IF_giga   -> TG_gi
    IF_large  -> TG_lg
    IF_little -> TG_lt

if_query :: (InputFile->Bool) -> LabeledQueries
if_query = genEOLabeledQueries QD_input


----------------------------------------------------------------------------------------------------
-- GCStats
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery GCStats where mkLabeledQueries = gs_query

data GCStats = GCS_with_gc_stats
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText GCStats

instance IsBrief GCStats where
  brief = fmt . build . const TG_rts_gc_stats

instance IsQuery GCStats where
  mkQuery = tagToQuery . const TG_rts_gc_stats

gs_query :: (GCStats->Bool) -> LabeledQueries
gs_query = genEOLabeledQueries QD_gc_stats


----------------------------------------------------------------------------------------------------
-- McVersion
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery McVersion where mkLabeledQueries = mv_query

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
  configEnumText _ = versionETC

instance IsBrief McVersion where
  brief = T.filter (/='.') . fmt . build

instance IsQuery McVersion where
  mkQuery il = LENS.set q_versions mv mempty
    where
      mv :: [VersionPrefix]
      mv = [either error id $ parseText $ fmt_t il]

mv_query :: (McVersion->Bool) -> LabeledQueries
mv_query = genEOLabeledQueries QD_version


----------------------------------------------------------------------------------------------------
-- Compiler
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Compiler where mkLabeledQueries = cr_query

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
  configEnumText _ = versionETC

instance IsBrief Compiler where
  brief = T.filter (/='.') . fmt . build

instance IsQuery Compiler where
  mkQuery il = LENS.set q_compilers gv mempty
    where
      gv :: [GHCVersionPrefix]
      gv = [either error id $ parseText $ fmt_t il]

cr_query :: (Compiler->Bool) -> LabeledQueries
cr_query = genEOLabeledQueries QD_compiler


----------------------------------------------------------------------------------------------------
-- OS
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery OS where mkLabeledQueries = os_query

data OS
  = OS_Darwin_19'6'0
  | OS_Darwin_20'3'0
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText OS

instance EnumText OS where
  configEnumText _ = versionETC

instance IsBrief OS where
  brief = \case
    OS_Darwin_19'6'0 -> "10.15" -- .7 (Big Sur)  (dt)
    OS_Darwin_20'3'0 -> "11.2"  -- .3 (Cataline) (ford)

instance IsQuery OS where
  mkQuery os = LENS.set q_os gv mempty
    where
      gv :: [OSVersionPrefix]
      gv = [either error id $ parseText $ fmt_t os]

os_query :: (OS->Bool) -> LabeledQueries
os_query = genEOLabeledQueries QD_os


----------------------------------------------------------------------------------------------------
-- Partitions
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Partitions where mkLabeledQueries = ps_query

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

ps_query :: (Partitions->Bool) -> LabeledQueries
ps_query = genEOLabeledQueries QD_partitions


----------------------------------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Workers where mkLabeledQueries = ws_query

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

ws_query :: (Workers->Bool) -> LabeledQueries
ws_query = genEOLabeledQueries QD_workers


----------------------------------------------------------------------------------------------------
-- Week
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery WeekNo where mkLabeledQueries = wn_query

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

wn_query :: (WeekNo->Bool) -> LabeledQueries
wn_query = genEOLabeledQueries QD_week


----------------------------------------------------------------------------------------------------
-- Output
----------------------------------------------------------------------------------------------------

instance IsLabeledQuery Output where mkLabeledQueries = op_query

data Output
  -- external times
  = OP_e_best_time
  | OP_e_wrst_time
  | OP_e_mean_time
  -- internal times
  | OP_i_best_time
  | OP_i_wrst_time
  | OP_i_mean_time
  -- memory: best
  | OP_e_best_max_res
  | OP_e_best_reclaims
  | OP_e_best_ctx_sws
  | OP_i_best_allocated
  | OP_i_best_copied
  | OP_i_best_num_gcs
  -- memory: worst
  | OP_e_wrst_max_res
  | OP_e_wrst_reclaims
  | OP_e_wrst_ctx_sws
  | OP_i_wrst_allocated
  | OP_i_wrst_copied
  | OP_i_wrst_num_gcs
  -- memory: mean
  | OP_e_mean_max_res
  | OP_e_mean_reclaims
  | OP_e_mean_ctx_sws
  | OP_i_mean_allocated
  | OP_i_mean_copied
  | OP_i_mean_num_gcs
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (IsBrief, EnumText, HasQueryMethods)
  deriving (Buildable, TextParsable)
    via UsingEnumText Output

instance IsQuery Output where
  mkQuery = const mempty

outputUnit :: Output -> Unit
outputUnit = fst . calc_output

calcOutput :: Output -> Vector Benchmark -> Datum
calcOutput = snd . calc_output

-- | retsricted to GiB/s for now
calc_output :: Output -> (Unit,Vector Benchmark -> Datum)
calc_output op = case op of
    -- external times
    OP_e_best_time      -> calc_op  bm_external_seconds min_1
    OP_e_wrst_time      -> calc_op  bm_external_seconds max_1
    OP_e_mean_time      -> calc_op  bm_external_seconds avg_1
    -- internal times
    OP_i_best_time      -> calc_op  bm_internal_seconds min_1
    OP_i_wrst_time      -> calc_op  bm_internal_seconds max_1
    OP_i_mean_time      -> calc_op  bm_internal_seconds avg_1
    -- memory: best
    OP_e_best_max_res   -> calc_op' gib _bm_max_res     min_1
    OP_e_best_reclaims  -> calc_op' fri _bm_reclaims    min_1
    OP_e_best_ctx_sws   -> calc_op' fri _bm_i_ctx_sws   min_1
    OP_i_best_allocated -> calc_op' gib _bm_allocated   min_1
    OP_i_best_copied    -> calc_op' gib _bm_copied      min_1
    OP_i_best_num_gcs   -> calc_op' fri _bm_num_gcs     min_1
    -- memory: worst
    OP_e_wrst_max_res   -> calc_op' gib _bm_max_res     max_1
    OP_e_wrst_reclaims  -> calc_op' fri _bm_reclaims    max_1
    OP_e_wrst_ctx_sws   -> calc_op' fri _bm_i_ctx_sws   max_1
    OP_i_wrst_allocated -> calc_op' gib _bm_allocated   max_1
    OP_i_wrst_copied    -> calc_op' gib _bm_copied      max_1
    OP_i_wrst_num_gcs   -> calc_op' fri _bm_num_gcs     max_1
    -- memory: mean
    OP_e_mean_max_res   -> calc_op' gib _bm_max_res     avg_1
    OP_e_mean_reclaims  -> calc_op' fri _bm_reclaims    avg_1
    OP_e_mean_ctx_sws   -> calc_op' fri _bm_i_ctx_sws   avg_1
    OP_i_mean_allocated -> calc_op' gib _bm_allocated   avg_1
    OP_i_mean_copied    -> calc_op' gib _bm_copied      avg_1
    OP_i_mean_num_gcs   -> calc_op' fri _bm_num_gcs     avg_1
  where
    calc_op' :: (Unit,a->Double)
             -> (Benchmark->a)
             -> (Double->[Double]->Double)
             -> (Unit,Vector Benchmark -> Datum)
    calc_op' (u,tod) prj = calc_op (u,tod . prj)

    gib :: (Unit,ByteCount -> Double)
    gib = (U_GiB,\bc -> snd fri bc / gibi)

    fri :: Integral a => (Unit,a->Double)
    fri = (U_count,fromIntegral)

    calc_op :: (Unit,Benchmark->Double)
            -> (Double->[Double]->Double)
            -> (Unit,Vector Benchmark -> Datum)
    calc_op (u,ext) agg = (u,f)
      where
        f v0 = case V.uncons v0 of
          Nothing        -> NoDatum
          Just (bm,bm_v) -> mk $ agg (ext bm) $ V.toList $ V.map ext bm_v
            where
              mk :: Double -> Datum
              mk d = Datum d u $ V.length v0

    bm_external_seconds :: (Unit,Benchmark -> Double)
    bm_external_seconds = (U_GiBps,calc)
      where
        calc :: Benchmark -> Double
        calc bm = gibps bm . (/1000) . fromIntegral . getMilisecondsReal . _bm_real $ bm

    bm_internal_seconds :: (Unit,Benchmark -> Double)
    bm_internal_seconds = (U_GiBps,calc)
      where
        calc :: Benchmark -> Double
        calc bm = gibps bm . getSeconds . _bm_time $ bm

    gibps :: Benchmark -> Double -> Double
    gibps bm secs = fromMaybe 0 (input_bytes bm) / (gibi*secs)

    min_1 :: Double -> [Double] -> Double
    min_1 d ds = L.minimum $ d:ds

    max_1 :: Double -> [Double] -> Double
    max_1 d ds = L.maximum $ d:ds

    avg_1 :: Double -> [Double] -> Double
    avg_1 d ds = sum (d:ds) / 1 + L.genericLength ds

gibi :: Double
gibi = 1024 * 1024 * 1024

input_bytes :: Benchmark -> Maybe Double
input_bytes Benchmark{..} =
  case L.nub $ catMaybes $ map tagToinputSize $ filter (getTags _bm_tags) [minBound..maxBound] of
    [sz] -> Just $ fromInteger sz
    _    -> Nothing

op_query :: (Output->Bool) -> LabeledQueries
op_query p = LabeledQueries "output" Set.empty
  [ LabeledQuery (fmt_t x) $ QueryDescriptor (const Nothing) $ Set.singleton x
    | x <- [minBound..maxBound]
    , p x
    ]


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

tagToQuery :: Tag -> Query
tagToQuery tg = LENS.set q_tags ([Set.fromList [tg]]) mempty

genEOLabeledQueries :: HasQueryMethods a => QueryDescriptorL -> (a->Bool) -> LabeledQueries
genEOLabeledQueries qdl p = LabeledQueries (fmt $ build qdl) Set.empty
    [ LabeledQuery (fmt_t x) $ flip QueryDescriptor Set.empty $ \case
          qdl_ | qdl_==qdl -> Just $ QueryMethods x
               | otherwise -> Nothing
      | x <- [minBound..maxBound]
      , p x
      ]

versionETC :: EnumTextConfig
versionETC = defaultEnumTextConfig
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
