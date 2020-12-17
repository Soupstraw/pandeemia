{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad

import Data.Text
import Data.Int
import Data.Maybe

import qualified Data.Map as M

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams

data Country = Country
  { _cName           :: Text
  , _cSuceptibles    :: Int64
  , _cExposed        :: Int64
  , _cInfectious     :: Int64
  , _cRecovered      :: Int64
  , _cYearlyTourists :: Int64
  }
makeLenses ''Country

data SimulationState = SimulationState
  { _ssCountries         :: [Country]
  , _ssBaseInfectionRate :: Double
  , _ssIncubationPeriod  :: Double
  , _ssDiseaseDuration   :: Double
  , _ssTravelMatrix      :: M.Map (Text, Text) Double
  }
makeLenses ''SimulationState

main :: IO ()
main = 
  do
    let countries =
          [ Country
              { _cName = "Eesti"
              , _cSuceptibles = 1328976
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = 3789955
              }
          , Country
              { _cName = "Läti"
              , _cSuceptibles = 2795000
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = 5370000
              }
          , Country
              { _cName = "Soome"
              , _cSuceptibles = 5518000
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = 22000000
              }
          , Country
              { _cName = "Venemaa"
              , _cSuceptibles = 144500000
              , _cExposed = 100
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = 584000000
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
          , (("Eesti", "Venemaa"), 540062)
          , (("Läti", "Venemaa"),  365783)
          , (("Soome", "Venemaa"), 938693)
          ]
    let bordersOpen = SimulationState
          { _ssTravelMatrix = travelMatrix
          , _ssDiseaseDuration = 0.1
          , _ssIncubationPeriod = 0.5
          , _ssBaseInfectionRate = 0.5
          , _ssCountries = countries
          }
    let countriesOpenGrid = layoutToGrid . countryGraph bordersOpen <$>
          [ "Eesti"
          , "Läti"
          , "Soome"
          , "Venemaa"
          ]
    void . renderableToFile def "mudel.svg" . fillBackground def . gridToRenderable $ 
      aboveN countriesOpenGrid

update :: SimulationState -> SimulationState
update state = Prelude.foldr ($) updated travelled
  where
    countries = state ^. ssCountries
    internal = fmap (updateInternal state) countries
    updated = state & ssCountries .~ internal

    travelMatrix = state ^. ssTravelMatrix
    travelled :: [SimulationState -> SimulationState]
    travelled = 
      do
        src <- internal
        dest <- internal
        let travelRate = M.findWithDefault 0 (src ^. cName, dest ^. cName) travelMatrix
        let dailyTravels = realToFrac $ travelRate / 365
        let srcPop = fromIntegral $ population src
        let travelSuc = round $ fromIntegral (src ^. cSuceptibles) / srcPop * dailyTravels
        let travelExp = round $ fromIntegral (src ^. cExposed) / srcPop * dailyTravels
        let travelRec = round $ fromIntegral (src ^. cRecovered) / srcPop * dailyTravels
        let updateSrc x = x & cSuceptibles -~ travelSuc
                            & cExposed     -~ travelExp
                            & cRecovered   -~ travelRec
        let updateDest x = x & cSuceptibles +~ travelSuc
                             & cExposed     +~ travelExp
                             & cRecovered   +~ travelRec
        let findLens n = ssCountries . traversed . filtered (view $ cName . to (==n))
        return $ \st -> st & findLens (_cName src)  %~ updateSrc
                           & findLens (_cName dest) %~ updateDest
    

updateInternal :: SimulationState -> Country -> Country
updateInternal state country = country 
                                 & cSuceptibles +~ round deltaS
                                 & cExposed     +~ round deltaE
                                 & cInfectious  +~ round deltaI
                                 & cRecovered   +~ round deltaR
  where
    infectionRate    = state ^. ssBaseInfectionRate
    incubationPeriod = state ^. ssBaseInfectionRate
    duration         = state ^. ssDiseaseDuration

    suceptibles = fromIntegral $ country ^. cSuceptibles
    exposed     = fromIntegral $ country ^. cExposed
    infected    = fromIntegral $ country ^. cInfectious

    pop  = population country
    beta = infectionRate / fromIntegral (pop - 1)

    deltaS = -beta*suceptibles*(exposed+infected)
    deltaE = beta*suceptibles*(exposed+infected)-incubationPeriod*exposed
    deltaI = exposed*incubationPeriod-infected*duration
    deltaR = infected*duration

population :: Country -> Int64
population country = sum
  [ country ^. cSuceptibles
  , country ^. cExposed
  , country ^. cInfectious
  , country ^. cRecovered
  ]

countryGraph :: SimulationState -> Text -> Layout Int Double
countryGraph init c = execEC $
  do
    let states = Prelude.zip [0..365] $ iterate update init
    let getVal l st = st ^? ssCountries . findCountry c . l . to fromIntegral
    let graph l = states & traversed . _2 %~ fromJust . getVal l
    layout_title .= unpack c
    layout_x_axis . laxis_override .= (axis_ticks .~ Prelude.zip [0,10..360] (repeat 3))
    plot $ line "Terved"        [graph cSuceptibles]
    plot $ line "Kokkupuutunud" [graph cExposed]
    plot $ line "Nakatunud"     [graph cInfectious]
    plot $ line "Taastunud"     [graph cRecovered]

findCountry :: Text -> Traversal' [Country] Country
findCountry c = traversed . filtered (view $ cName . to (==c))

