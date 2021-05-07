{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module McAeson.Chart.Types.JSChart.Template where

import           Data.String
import           Data.Text(Text)
import qualified Data.Text                        as T
import           Fmt
import           Text.Enum.Text


newtype Template p = Template { getTemplate :: Text }
  deriving stock (Show)
  deriving newtype (IsString)

subst :: forall p . EnumText p => (p->Text) -> Template p -> Text
subst sigma (Template t0) = foldr f t0 [minBound..maxBound::p]
  where
    f :: p -> Text -> Text
    f p = T.replace (fmt $ "<<"+|p|+">>") (sigma p)
