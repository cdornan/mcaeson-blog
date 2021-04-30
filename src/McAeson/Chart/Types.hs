{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving        #-}

module McAeson.Chart.Types where

-- import           Control.Monad
import           Data.Maybe
import           Data.Text(Text)
-- import qualified Data.Text      as T
-- import           Data.Time
-- import qualified Data.Vector                as V
-- import           Fmt
-- import           McAeson.Bench.Renderable
-- import           McAeson.Bench.Types
-- import           McAeson.Query
import           McAeson.Query.Types
-- import           McAeson.Installation.Persistence
-- import           McAeson.Installation.Types
-- import           Text.Show.Functions()


import           Fmt
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
