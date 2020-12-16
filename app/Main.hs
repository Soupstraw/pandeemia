{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad.State

import Data.Text
import qualified Data.Map as M

data Country = Country
  { _cName           :: Text
  , _cSuceptibles    :: Int
  , _cExposed        :: Int
  , _cInfectious     :: Int
  , _cRecovered      :: Int
  , _cYearlyTourists :: Int
  }
makeLenses ''Country

data SimulationState = SimulationState
  { _ssCountries         :: [Country]
  , _ssBaseInfectionRate :: Float
  , _ssIncubationPeriod  :: Float
  , _ssDiseaseDuration   :: Float
  , _ssTravelMatrix      :: M.Map (Text, Text) Float
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
              , _cYearlyTourists = undefined
              }
          , Country
              { _cName = "Soome"
              , _cSuceptibles = 5518000
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = undefined
              }
          , Country
              { _cName = "Venemaa"
              , _cSuceptibles = 144500000
              , _cExposed = 0
              , _cInfectious = 0
              , _cRecovered = 0
              , _cYearlyTourists = undefined
              }
          ]
    let travelMatrix = M.fromList
          -- Overnight stays
          [ (("Läti", "Eesti"), 182860)
          , (("Soome", "Eesti"), 804645)
          , (("Venemaa", "Eesti"), 260036)
          , (("Eesti", "Läti"), 210300)
          , (("Soome", "Läti"), 82500)
          , (("Venemaa", "Läti"), 297800)
          , (("Läti", "Soome"), 50000)
          , (("Eesti", "Soome"), 243000)
          , (("Venemaa", "Soome"), 814600)
          -- Total visitors
          , (("Eesti", "Venemaa"), 540062)
          , (("Läti", "Venemaa"), 365783)
          , (("Soome", "Venemaa"), 938693)
          ]
    undefined

update :: State SimulationState ()
update =
  do
    countries <- use ssCountries
    internal <- traverse updateInternal countries
    travelMatrix <- use ssTravelMatrix
    let travelled = 
          do
            x <- internal
            y <- internal
            let travelRate = M.lookup (x ^. cName, y ^. cName) travelMatrix
            undefined
    undefined

updateInternal :: Country -> State SimulationState Country
updateInternal country =
  do
    infectionRate    <- use ssBaseInfectionRate
    incubationPeriod <- use ssBaseInfectionRate
    duration         <- use ssDiseaseDuration

    let suceptibles = fromIntegral $ country ^. cSuceptibles
    let exposed     = fromIntegral $ country ^. cExposed
    let infected    = fromIntegral $ country ^. cInfectious
    let recovered   = fromIntegral $ country ^. cRecovered

    let pop  = suceptibles + exposed + infected + recovered
    let beta = infectionRate / (pop - 1)

    let deltaS = -beta*suceptibles*(exposed+infected)
    let deltaE = beta*suceptibles*(exposed+infected)-incubationPeriod*exposed
    let deltaI = exposed*incubationPeriod-infected*duration
    let deltaR = infected*duration
    return $ country & cSuceptibles +~ round deltaS
                     & cExposed     +~ round deltaE
                     & cInfectious  +~ round deltaI
                     & cRecovered   +~ round deltaR

