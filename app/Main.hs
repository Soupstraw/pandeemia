{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.State

import Data.Text
import qualified Data.Map as M

data Disease = Disease
  { _dBaseInfectionRate :: Float
  , _dIncubationPeriod  :: Float
  , _dDuration          :: Float
  }
makeLenses ''Disease

data Country = Country
  { _cName         :: Text
  , _cSuceptibles  :: Int
  , _cExposed      :: Int
  , _cInfectious   :: Int
  , _cRecovered    :: Int
  }
makeLenses ''Country

data SimulationState = SimulationState
  { _ssCountries    :: [Country]
  , _ssDisease      :: Disease
  , _ssTravelMatrix :: M.Map (Text, Text) Float
  }
makeLenses ''SimulationState

main :: IO ()
main = 
  do
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
    infectionRate    <- use $ ssDisease . dBaseInfectionRate
    incubationPeriod <- use $ ssDisease . dBaseInfectionRate
    duration         <- use $ ssDisease . dDuration

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

