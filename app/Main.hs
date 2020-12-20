{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State

import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Maybe
import Data.Time.Calendar
import qualified Data.Vector as V

import qualified Data.Map as M

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams

numberOfDays :: Num a => a
numberOfDays = 365

startDate :: Day
startDate = fromGregorian 2020 3 1

data Country = Country
  { _cName           :: T.Text
  , _cSuceptibles    :: Double
  , _cExposed        :: Double
  , _cInfectious     :: Double
  , _cRecovered      :: Double
  , _cFatigue        :: [Double]
  }
makeLenses ''Country

data SimulationState = SimulationState
  { _ssCountries          :: [Country]
  , _ssBaseInfectionRate  :: Double
  , _ssIncubationPeriod   :: Double
  , _ssDiseaseDuration    :: Double
  , _ssFatigueCoefficient :: Double
  , _ssTravelMatrix       :: M.Map (T.Text, T.Text) Double
  }
makeLenses ''SimulationState

main :: IO ()
main = 
  do
    estFatigue <- readTrendsData "data/eesti_trends.csv"
    finFatigue <- readTrendsData "data/soome_trends.csv"
    rusFatigue <- readTrendsData "data/venemaa_trends.csv"
    latFatigue <- readTrendsData "data/lati_trends.csv"
    wldFatigue <- readTrendsData "data/maailm_trends.csv"
    let countries =
          -- Nakatunud 1. märtsi seisuga
          [ Country
              { _cName = "Eesti"
              , _cSuceptibles = 1328976
              , _cExposed = 1
              , _cInfectious = 2
              , _cRecovered = 0
              , _cFatigue = estFatigue
              }
          , Country
              { _cName = "Läti"
              , _cSuceptibles = 2795000
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cFatigue = latFatigue
              }
          , Country
              { _cName = "Soome"
              , _cSuceptibles = 5518000
              , _cExposed = 6
              , _cInfectious = 12
              , _cRecovered = 0
              , _cFatigue = finFatigue
              }
          , Country
              { _cName = "Venemaa"
              , _cSuceptibles = 144500000
              , _cExposed = 2
              , _cInfectious = 4
              , _cRecovered = 0
              , _cFatigue = rusFatigue
              }
          , Country
              { _cName = "Maailm"
              , _cSuceptibles = 7.8e9
              , _cExposed = 84000
              , _cInfectious = 42000
              , _cRecovered = 42000
              , _cFatigue = wldFatigue
              }
          ]
    let travelMatrix = M.fromList
          -- Overnight stays
          [ (("Läti", "Eesti"),    182860)
          , (("Soome", "Eesti"),   804645)
          , (("Venemaa", "Eesti"), 260036)
          , (("Eesti", "Läti"),    210300)
          , (("Soome", "Läti"),    82500)
          , (("Venemaa", "Läti"),  297800)
          , (("Läti", "Soome"),    50000)
          , (("Eesti", "Soome"),   243000)
          , (("Venemaa", "Soome"), 814600)
          -- Total visitors
          , (("Eesti", "Venemaa"),  540062)
          , (("Läti", "Venemaa"),   365783)
          , (("Soome", "Venemaa"),  938693)
          , (("Maailm", "Eesti"),   3789955)
          , (("Maailm", "Läti"),    5370000)
          , (("Maailm", "Soome"),   22000000)
          , (("Maailm", "Venemaa"), 584000000)
          ]
    let bordersOpen = SimulationState
          { _ssTravelMatrix = travelMatrix
          , _ssDiseaseDuration   = 0.01
          , _ssIncubationPeriod  = 0.05
          , _ssBaseInfectionRate = 0.17
          , _ssFatigueCoefficient = 0.5
          , _ssCountries = countries
          }
    let countriesOpenGrid = layoutToGrid . countryGraph bordersOpen <$>
          [ "Eesti"
          , "Läti"
          , "Soome"
          , "Venemaa"
          , "Maailm"
          ]
    let opts = def
          { _fo_size = (1024, 1024)
          }
    void . renderableToFile opts "mudel.svg" . fillBackground def . gridToRenderable $ 
      aboveN countriesOpenGrid

update :: SimulationState -> SimulationState
update state = evalState ?? state $
  do
    updated <- traverse updateInternal =<< use ssCountries
    ssCountries .= updated
    internal <- use ssCountries

    travelMatrix <- use ssTravelMatrix
    let travelled :: [SimulationState -> SimulationState]
        travelled = 
          do
            src <- internal
            dest <- internal
            let travelRate1 = M.findWithDefault 0 (src ^. cName, dest ^. cName) travelMatrix
            let travelRate2 = M.findWithDefault 0 (dest ^. cName, src ^. cName) travelMatrix
            let travelRate = travelRate1 + travelRate2
            let dailyTravels = realToFrac $ travelRate / 365
            let fatigue = head $ src ^. cFatigue
            let srcPop = population src
            let fatigueCoef = state ^. ssFatigueCoefficient
            let ftg = fatigue * fatigueCoef + 1 - fatigueCoef
            let travelSuc = (src ^. cSuceptibles) / srcPop * dailyTravels * ftg
            let travelExp = (src ^. cExposed) / srcPop * dailyTravels * ftg
            let travelRec = (src ^. cRecovered) / srcPop * dailyTravels * ftg
            let updateSrc x = x & cSuceptibles -~ travelSuc
                                & cExposed     -~ travelExp
                                & cRecovered   -~ travelRec
            let updateDest x = x & cSuceptibles +~ travelSuc
                                 & cExposed     +~ travelExp
                                 & cRecovered   +~ travelRec
            let findLens n = ssCountries . traversed . filtered (view $ cName . to (==n))
            return $ \st -> st & findLens (_cName src)  %~ updateSrc
                               & findLens (_cName dest) %~ updateDest
    state <- get
    let state' = state & ssCountries . traversed . cFatigue %~ tail
    return $ Prelude.foldr ($) state' travelled

updateInternal :: (MonadState SimulationState m) => Country -> m Country
updateInternal country = 
  do
    infectionRate <- use ssBaseInfectionRate
    gamma         <- use ssIncubationPeriod
    duration      <- use ssDiseaseDuration
    fatigueCoef   <- use ssFatigueCoefficient
    let suceptibles = country ^. cSuceptibles
    let exposed     = country ^. cExposed
    let infected    = country ^. cInfectious
    let fatigue     = head $ country ^. cFatigue

    let pop  = population country
    let beta = infectionRate / (pop - 1) * (fatigue*fatigueCoef+1-fatigueCoef)

    let deltaS = -beta*suceptibles*(exposed+infected)
    let deltaE = beta*suceptibles*(exposed+infected)-gamma*exposed
    let deltaI = exposed*gamma-infected*duration
    let deltaR = infected*duration
    return $ country 
      & cSuceptibles +~ deltaS
      & cExposed     +~ deltaE
      & cInfectious  +~ deltaI
      & cRecovered   +~ deltaR

population :: Country -> Double
population country = sum
  [ country ^. cSuceptibles
  , country ^. cExposed
  , country ^. cInfectious
  , country ^. cRecovered
  ]

fillBetween title vs = liftEC $ do
  plot_fillbetween_title .= title
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle color
  plot_fillbetween_values .= vs

countryGraph :: SimulationState -> T.Text -> Layout Day Double
countryGraph init c = execEC $
  do
    let states = take numberOfDays $ iterate update init
    let countryStats = states ^.. folded . ssCountries . findCountry c
    let dat = zip3 
          [startDate..addDays numberOfDays startDate]
          (countryStats ^.. folded . cInfectious)
          (countryStats ^.. folded . cRecovered)
    layout_title .= T.unpack c
    plot $ fillBetween "Taastunud" [(d, (0, v1+v2)) | (d, v1, v2) <- dat]
    plot $ fillBetween "Nakatunud" [(d, (0, v1)) | (d, v1, _) <- dat]

findCountry :: T.Text -> Traversal' [Country] Country
findCountry c = traversed . filtered (view $ cName . to (==c))

readTrendsData :: T.Text -> IO [Double]
readTrendsData filename = 
  do
    file <- readFile $ T.unpack filename
    let csv :: Either String (V.Vector (String, Int))
        csv = CSV.decode CSV.NoHeader $ BS.fromString file
    let points :: [(Day, Int)]
        points = V.toList $ (_1 %~ read) <$> either error id csv
    let interp' :: Day -> Double
        interp' t = fromIntegral . snd . fromMaybe (last points) $ L.find (\(x,_) -> x >= t) points
    let interp t = 1 - interp' t / 100
    let interpolated = fmap interp [startDate..addDays numberOfDays startDate]
    return $ interpolated <> repeat (last interpolated)

