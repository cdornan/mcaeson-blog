{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module McAeson.Chart.Types.MDTable
  ( markdownTable
  , CJ(..)
  , markdownTable'
  , markdownTable''
  ) where

import           Fmt
import qualified Data.List            as L
import qualified Data.Text            as T
import           Text.Enum.Text


markdownTable :: EnumText i => (i->a->Builder) -> [a] -> Builder
markdownTable = markdownTable' $ const LJust

data CJ = LJust | CJust | RJust
  deriving (Eq,Ord,Show)

markdownTable' :: forall i a . EnumText i
               => (i->CJ)
               -> (i->a->Builder)
               -> [a]
               -> Builder
markdownTable' jf gen =
  markdownTable'' jf build gen [minBound..maxBound]

markdownTable'' :: (i->CJ)
                -> (i->Builder)
                -> (i->a->Builder)
                -> [i]
                -> [a]
                -> Builder
markdownTable'' jf bld_i gen is xs =
  mk_table
    (map jf    is)
    (map bld_i is)
    [ [ gen i x | i <- is ]
      | x <- xs
      ]

mk_table :: [CJ] -> [Builder] -> [[Builder]] -> Builder
mk_table cjs hds grid = unlinesF $
    [ mk_row (LJust:repeat LJust) wds hds
    , mk_row0 $ zipWith mk_dashes cjs wds
    ] ++
    [ mk_row cjs wds row
      | row <- grid
      ]
  where
    wds = map (maximum . map size) $ L.transpose $ hds : grid

mk_row :: [CJ] -> [Int] -> [Builder] -> Builder
mk_row cjs wds = mk_row0 . zipWith3 padf cjs wds
  where
    padf :: CJ -> Int -> Builder -> Builder
    padf cj w = case cj of
      LJust -> padRightF w ' '
      CJust -> padBothF  w ' '
      RJust -> padLeftF  w ' '


mk_dashes :: CJ -> Int -> Builder
mk_dashes cj w = case cj of
    LJust -> ":" <> dshs (w-1) <> ""
    CJust -> ":" <> dshs (w-2) <> ":"
    RJust -> ""  <> dshs (w-1) <> ":"
  where
    dshs n = mconcat $ replicate (max 1 n) "-"

mk_row0 :: [Builder] -> Builder
mk_row0 = mconcat . L.intersperse "|"

size :: Builder -> Int
size = T.length . fmt
