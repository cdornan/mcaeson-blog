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


newtype Markdown =
  Markdown
    { getMarkdown :: Text
    }
  deriving stock (Show)
  deriving newtype (IsString)

newtype Javascript =
  Javascript
    { getJavascript :: Text
    }
  deriving stock (Show)
  deriving newtype (IsString)

data Datum
  = NoDatum
  | Datum Double
  deriving stock (Show)
