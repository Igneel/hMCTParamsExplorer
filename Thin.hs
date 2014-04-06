
-- Параметры пленки.

module Thin where

import Types
import ADCInteract
import Extrapolation
import Filter

sxx = undefined
sxy = undefined

sEff = undefined

rhEff = undefined

setThinParams [] = []
setThinParams ((b,us,uy):rest) = [ThinParams (MagneticField b, Magnetoresistance us,HallEffect uy)] ++ setThinParams rest

hallEffect [] = []
hallEffect ((ThinParams (MagneticField b, Magnetoresistance us, HallEffect uy)):rest) = [uy]++hallEffect rest

magnetoresistance [] = []
magnetoresistance ((ThinParams (MagneticField b, Magnetoresistance us, HallEffect uy)):rest) = [us]++ magnetoresistance rest

originalParams = splitData $ convertToVolt getData

filteredParams = filterLowBand originalParams

extrapolatedParams = extrapolate filteredParams