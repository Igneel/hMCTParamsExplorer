-- А что если всё-таки попробовать написать всю эту дребень на Хаскелле?

-- Испробуем.

module Main where

import Types
import Filter
import Extrapolation
import Feat
import ADCInteract
import Thin




main = 
	do
		putStrLn "Hello World"
		putStrLn initADC
		putStrLn $configurateADC 3 400
		putStrLn startMeasurement
		putStrLn stopMeasurement
		putStrLn $convertToVolt (splitData getData)
	