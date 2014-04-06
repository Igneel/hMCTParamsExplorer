
-- Взаимодействие с АЦП
module ADCInteract where

import ADCBind

initADC = "InitADC"

configurateADC channels frencuency = "configurateADC" ++ show (channels) ++ show (frencuency)

startMeasurement = "startMeasurement"

stopMeasurement = "stopMeasirement"

getData::[Int]
getData = [0,3,0,1,4,1]

splitData (x:y:z:rest) = [(x,y,z)] ++ splitData rest
splitData _ = []

convertToVolt (x:xs) = [x/1] ++ convertToVolt xs
convertToVolt _ = []





