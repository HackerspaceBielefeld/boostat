module Boostat.Chart where

import Graphics.Rendering.Chart.Easy

import Data.Time (LocalTime(..))
import Data.List (transpose)

import Boostat.Types (Record(..))

genChart :: String -> [Record] -> Renderable ()
genChart t r = toRenderable $ genChart' t r

genChart' :: String -> [Record] -> Layout LocalTime Double
genChart' title raw =
  layout_title .~ (title ++ " Boost statistics")
  $ layout_y_axis . laxis_title .~ "Euro"
  $ layout_plots .~ [toPlot pFrei, plotBars pChanges]
  $ def where
    pFrei = plot_lines_title .~ "Freigegeben"
          $ plot_lines_values .~ vFrei
          $ def
    vFrei = [map (\r -> (date r, fromInteger (freigegeben r) / 100)) raw]
    pChanges = plot_bars_titles .~ ["Gesamt", "Ausgezahlt"]
             $ plot_bars_values .~ vBars
             $ def
    vBars :: [(LocalTime,[Double])]
    vBars = zip (tail $ map date raw)
      $ transpose
      $ map diff
      $ (map . map) ((/ 100) . fromInteger)
      [ map gesamt raw, map ausgezahlt raw ]
    diff :: Num a => [a] -> [a]
    diff []  = []
    diff vs  = zipWith (-) (tail vs) (init vs)


