{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module McAeson.Chart.Types.Table(table) where

-- import qualified Control.Lens                 as LENS
import           Data.Default
-- import qualified Data.List                    as L
-- import           Data.Maybe
-- import           Data.Set(Set)
-- import qualified Data.Set                     as Set
import           Data.Text(Text)
-- import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
-- import           Data.Time
-- import           Data.Vector(Vector)
-- import qualified Data.Vector                  as V
-- import           Fmt
-- import           GHC.Generics
-- import           McAeson.Bench.JobTags
-- import           McAeson.Bench.Types
import           McAeson.Chart.Types.Basic
import           McAeson.Chart.Types.Chart
-- import           McAeson.Chart.Types.WeekNo
-- import           McAeson.Installation.Types
-- import           McAeson.Query
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.XHtml5          as BLZ
import           Text.Heredoc
-- import           Text.Enum.Text
import           Text.Pandoc
-- import           Text.Pandoc.Definition
-- import           Text.Pandoc.Error
-- import           Text.Pandoc.Writers
import           Text.Table.Tablify


table :: Chart -> IO Html
table _ = do
    fmap mk $ runIO $ do
      Pandoc m bls <- readMarkdown def test
      writeHtml5 def $ Pandoc m $ concatMap tablifyCsvLinksPure bls
  where
    mk :: Either PandocError BLZ.Html -> Html
    mk = either (error . show) (Html . TL.toStrict . renderHtml)


test :: Text
test = [here|
```{.table aligns="LCR" caption="This is the **caption**" header="yes"}
Fruit, Quantity, Price
apples, 15, 3.24
oranges, 12, 2.22
```
|]
