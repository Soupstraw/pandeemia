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

-- Simulatsiooni alguskuupäev
startDate :: Day
startDate = fromGregorian 2020 9 1

-- Simulatsiooni lõpukuupäev
endDate :: Day
endDate = fromGregorian 2021 2 1

-- Andmestruktuur riigi info talletamiseks
data Country = Country
  { _cName           :: T.Text
  , _cSuceptibles    :: Double
  , _cExposed        :: Double
  , _cInfectious     :: Double
  , _cRecovered      :: Double
  , _cFatigue        :: [Double]
  }
makeLenses ''Country

-- Defineerime simulatsiooni oleku
data SimulationState = SimulationState
  { _ssCountries          :: [Country]
  , _ssBaseInfectionRate  :: Double
  , _ssIncubationPeriod   :: Double
  , _ssDiseaseDuration    :: Double
  , _ssFatigueCoefficient :: Double
  , _ssTravelCoefficient  :: Double
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
    -- Riikide andmed
    let countries =
          -- Nakatunud 1. märtsi seisuga
          [ Country
              { _cName = "Eesti"
              , _cSuceptibles = 1326590            -- Tervete inimeste arv
              , _cExposed = 55                     -- Kokkupuutunute arv
              , _cInfectious = 219                 -- Nakkusohtlike arv
              , _cRecovered = 2396                 -- Taastunute arv
              , _cFatigue = estFatigue             -- Pandeemiaväsimus (ei kasutata hetkel)
              }
          , Country
              { _cName = "Läti"
              , _cSuceptibles = 2795000
              , _cExposed = 20
              , _cInfectious = 81
              , _cRecovered = 1404
              , _cFatigue = latFatigue
              }
          , Country
              { _cName = "Soome"
              , _cSuceptibles = 5518000
              , _cExposed = 151
              , _cInfectious = 606
              , _cRecovered = 7500
              , _cFatigue = finFatigue
              }
          , Country
              { _cName = "Venemaa"
              , _cSuceptibles = 144500000
              , _cExposed = 40000
              , _cInfectious = 167004
              , _cRecovered = 1000000
              , _cFatigue = rusFatigue
              }
          , Country
              { _cName = "Maailm"
              , _cSuceptibles = 7.8e9
              , _cExposed = 1.65e6
              , _cInfectious = 6471000
              , _cRecovered = 26500000
              , _cFatigue = wldFatigue
              }
          ]
    -- Reiside maatriks (arv näitab reiside arvu aastas)
    let travelMatrix = M.fromList
          -- Overnight stays
          [ (("Läti", "Eesti"),    182860) -- Aastas Lätist Eestisse reisinute arv
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
          , _ssDiseaseDuration   = 12    -- Haiguse kestus päevades
          , _ssIncubationPeriod  = 3     -- Haiguse peiteaeg päevades
          , _ssBaseInfectionRate = 0.090 -- Nakatamise kiirus
          , _ssFatigueCoefficient = 1
          , _ssCountries = countries
          , _ssTravelCoefficient = 1     -- Pole hetkel nii oluline, seda muudame hiljem
          }
    let countriesOpenGrid = layoutToGrid . countryGraph bordersOpen <$>
          [ "Eesti"
          , "Soome"
          , "Läti"
          , "Venemaa"
          ]
    let opts = def
          { _fo_size = (1024*2, 1024*2)
          }
    -- Joonistame graafikud ja salvestame need faili
    void . renderableToFile opts "mudel.svg" . fillBackground def . gridToRenderable $ 
      aboveN countriesOpenGrid

update :: SimulationState -> SimulationState
update state = evalState ?? state $
  do
    updated <- traverse updateInternal =<< use ssCountries
    ssCountries .= updated
    internal <- use ssCountries
    travelCoefficient <- use ssTravelCoefficient

    travelMatrix <- use ssTravelMatrix
    let travelled :: [SimulationState -> SimulationState]
        travelled = 
          do
            src <- internal
            dest <- internal
            -- Leiame reiside arvu kahe riigi peale ning liidame need (A -> B ja B -> A)
            let travelRate1 = M.findWithDefault 0 (src ^. cName, dest ^. cName) travelMatrix
            let travelRate2 = M.findWithDefault 0 (dest ^. cName, src ^. cName) travelMatrix
            let travelRate = (travelRate1 + travelRate2 * 0.8)*travelCoefficient

            -- Leiame keskmise päevaste reiside arvu
            let dailyTravels = realToFrac $ travelRate / 365
            let srcPop = population src

            -- Arvutame kui palju reisib haigestunuid terveid ja taastunuid
            let travelSuc = (src ^. cSuceptibles) / srcPop * dailyTravels
            let travelExp = (src ^. cExposed) / srcPop * dailyTravels
            let travelRec = (src ^. cRecovered) / srcPop * dailyTravels
            -- Nakatunud reisivad poole väiksema tõenäosusega
            let travelInf = (src ^. cInfectious) / srcPop * dailyTravels * 0.5

            -- Uuendame riikide populatsioone
            let updateSrc x = x & cSuceptibles -~ travelSuc
                                & cInfectious  -~ travelInf
                                & cExposed     -~ travelExp
                                & cRecovered   -~ travelRec
            let updateDest x = x & cSuceptibles +~ travelSuc
                                 & cInfectious  +~ travelInf
                                 & cExposed     +~ travelExp
                                 & cRecovered   +~ travelRec

            -- Tagastame funktsiooni, mis uuendab simulatsiooni olekut
            let findLens n = ssCountries . traversed . filtered (view $ cName . to (==n))
            return $ \st -> st & findLens (_cName src)  %~ updateSrc
                               & findLens (_cName dest) %~ updateDest
    state <- get
    let state' = state & ssCountries . traversed . cFatigue %~ tail
    -- Rakendame järjestikku kõik eelnevalt leitud oleku uuendamise funktsioonid
    return $ Prelude.foldr ($) state' travelled

updateInternal :: (MonadState SimulationState m) => Country -> m Country
updateInternal country = 
  do
    -- Leiame olekust kõik vajalikud andmed
    infectionRate <- use ssBaseInfectionRate
    mu            <- use $ ssIncubationPeriod . to (1/)
    gamma         <- use $ ssDiseaseDuration . to (1/)
    --fatigueCoef   <- use ssFatigueCoefficient
    let suceptibles = country ^. cSuceptibles
    let exposed     = country ^. cExposed
    let infected    = country ^. cInfectious
    --let fatigue     = head $ country ^. cFatigue
    
    -- Arvutame rahvaarvu ja leiame parameetri beeta
    let pop  = population country
    let beta = infectionRate / (pop - 1) 

    -- Arvuta igas olekus oleva populatsiooni muudud
    let deltaS = -beta*suceptibles*(exposed+infected)
    let deltaE = beta*suceptibles*(exposed+infected)-mu*exposed
    let deltaI = exposed*mu-infected*gamma
    let deltaR = infected*gamma

    -- Uuenda riigi tervete, kokkupuutunute ja nakkusohtlike arve
    return $ country 
      & cSuceptibles +~ deltaS
      & cExposed     +~ deltaE
      & cInfectious  +~ deltaI
      & cRecovered   +~ deltaR

-- Arvutab riigi rahvaarvu
population :: Country -> Double
population country = sum
  [ country ^. cSuceptibles
  , country ^. cExposed
  , country ^. cInfectious
  , country ^. cRecovered
  ]

-- Joonistab riigi graafikud nakatunud
countryGraph :: SimulationState -> T.Text -> Layout Day Double
countryGraph init c = execEC $
  do
      let infVals x = zip xVals $ ite x ^.. folded . cInfectious
      layout_title .= T.unpack c <> " nakatunute arv"
      layout_x_axis . laxis_override .= (axis_labels . traversed . traversed . _2 %~ takeWhile (/= '-'))
      let plt coef = plot . line ("Reisimise koefitsent " <> show coef) $ [infVals coef]
      -- Joonistame graafikud muutes reisimise koefitsenti 0.25 kaupa
      forM [0,0.25..1] plt
  where
    withTravelCoefficient x = init & ssTravelCoefficient .~ x
    numberOfDays = diffDays endDate startDate
    ite x = states ^.. folded . ssCountries . findCountry c
      where states = take (fromEnum numberOfDays) $ iterate update $ withTravelCoefficient x
    xVals = [startDate..addDays numberOfDays startDate]
    
-- Funktsioon nime järgi riigi otsimiseks ja selle muutmiseks
findCountry :: T.Text -> Traversal' [Country] Country
findCountry c = traversed . filtered (view $ cName . to (==c))

-- Google Trendsi andmete sisselugemine (ei ole mudelis kasutusel)
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
    let interpolated = fmap interp [startDate..endDate]
    return $ interpolated <> repeat (last interpolated)

