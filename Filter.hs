
-- Модуль фильтрации.

module Filter where

import Types

filterLowBand::[Double]->Int->Double->[Double]
filterLowBand x lengthFilter samplingF = filter_helper 1    
    where fCut=5.5*samplingF/ realToFrac lengthFilter
          blackman i = 0.42 - 0.5 * cos((2*pi*realToFrac i) / realToFrac( lengthFilter-1)) + 0.08 * cos((4*pi*realToFrac i) /realToFrac( lengthFilter-1))
          h_id::Int->Double
          h_id i | i==0 = 1.0
                 | otherwise = sin(2.0*pi*fCut* realToFrac i )/(2.0*pi*fCut*realToFrac i)
          h::Int->[Double]
          h i = map (/normKoef) (h_temp 0)
              where h_temp i | i<=lengthFilter = [(h_id i) * (blackman i)]++ h_temp (i+1)
              	             | otherwise = []
                    normKoef = sum (h_temp 0)
          filter_helper::Int->[Double]
          filter_helper counter = [f 0.0  (m counter x) ] ++ filter_helper (counter+1)
              where 
                    m counter x | counter <=lengthFilter = take counter x
                    			| otherwise = drop (counter-lengthFilter) $ take counter x
                    f::Int->[Double]->Double
                    f accum [] = accum
                    f accum (x:xs)  =f (sum (map (x*) (h counter))+accum) xs

