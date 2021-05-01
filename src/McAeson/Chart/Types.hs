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

import qualified Control.Lens                 as L
-- import           Control.Monad
import           Data.Maybe
import qualified Data.Set                     as Set
import           Data.Text(Text)
-- import qualified Data.Text      as T
-- import           Data.Time
-- import qualified Data.Vector                as V
import           Fmt
-- import           McAeson.Bench.Renderable
-- import           McAeson.Bench.Types
-- import           McAeson.Query
import           McAeson.Query.Types
-- import           McAeson.Installation.Persistence
-- import           McAeson.Installation.Types
import           McAeson.Types
-- import           Text.Show.Functions()


import           GHC.Generics
import           Text.Enum.Text


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
  DataSet
    { _ds_chart :: Chart
    , _ds_data  :: [[Datum]]
    }

data LabeledQuery =
  LabeledQueries
    { _lq_label   :: Text
    , _lq_queries :: QueryDescriptor
    }
  deriving (Show)

data Datum
  = DatumNull
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



extract :: Chart -> IO ChartData
extract = undefined

renderTable :: ChartData -> Markdown
renderTable = undefined

renderChart :: ChartData -> ChartPacket
renderChart = undefined



newtype QueryDescriptor =
  QueryDescriptor
    { getQueryDescriptor :: QueryDescriptorL -> Maybe QueryMethods
    }
  deriving (Show)

instance Semigroup QueryDescriptor where
  (<>) x y = QueryDescriptor f
    where
      f qdl = case isJust mb of
          True  -> mb
          False -> getQueryDescriptor y qdl
        where
          mb = getQueryDescriptor x qdl




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

data QueryMethods = forall a . (Bounded a,IsBrief a,Buildable a,Enum a,IsQuery a,Show a) =>
  QueryMethods
    { _qm_value :: a
    }

deriving instance Show QueryMethods


data Function
  = FN_string_count
  | FN_tsearch
  | FN_simple_json
  | FN_aeson_value
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText)
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



data Algorithm
  = AL_tyro
  | AL_pheres
  | AL_medea
  | AL_aeson
  | AL_jsonsimd
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (EnumText)
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
  deriving anyclass (EnumText)
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



tag_to_query :: Tag -> Query
tag_to_query tg = L.set q_tags (Set.fromList [tg]) mempty
