{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types.Basic where

import qualified Data.ByteString.Lazy     as LBS
import           Data.String
import           Data.Text(Text)
import           Fmt
import           Text.Enum.Text


newtype Html =
  Html
    { getHtml :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString,Monoid,Semigroup)

newtype CSV =
  CSV
    { getCSV :: LBS.ByteString
    }
  deriving stock (Show)
  deriving newtype (IsString,Monoid,Semigroup)

newtype ID =
  ID
    { getID :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

newtype Height =
  Height
    { getHeight :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

newtype Width =
  Width
    { getWidth :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

newtype Markdown =
  Markdown
    { getMarkdown :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

newtype Javascript =
  Javascript
    { getJavascript :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

data Datum
  = NoDatum
  | Datum Double Unit Int
  deriving stock (Show)

instance Buildable Datum where
  build = \case
    NoDatum     -> "null"
    Datum d _ _ -> fixedF 3 d

data Unit
  = U_none
  | U_GiBps
  | U_GiB
  | U_count
  deriving stock (Bounded, Enum, Eq, Ord, Show)
  deriving anyclass (EnumText)
  deriving (Buildable, TextParsable)
    via UsingEnumText Unit

datumUnit :: Datum -> Maybe Unit
datumUnit NoDatum       = Nothing
datumUnit (Datum _ u _) = Just u
