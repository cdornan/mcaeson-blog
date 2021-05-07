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

import           Data.String
import           Data.Text(Text)
import           Fmt


newtype Html =
  Html
    { getHtml :: Text
    }
  deriving stock (Show)
  deriving newtype (Buildable,IsString)

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
  | Datum Double
  deriving stock (Show)

instance Buildable Datum where
  build = \case
    NoDatum -> "null"
    Datum d -> build $ show d
