
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

-- Код ниже даже отлажен и дает одинаковые с матлабом результаты.

generateCoef::Int->Double->Double->Double->[Double]
generateCoef length samplingFreq passFreq stopFreq 
 = normH
 where normH = map (\x-> x/sumH) h
       sumH = sum h
       h = zipWith (*) w h_id
       h_id = getIdealH 0 length cutW cutFreq
       w= blackmanFunc 0 length
       cutW = 2.0*pi*cutFreq
       cutFreq = (passFreq+stopFreq) / 2.0 / samplingFreq

blackmanFunc::Int->Int->[Double]
blackmanFunc i2 length 
 | i2>0 && i2<length = (0.42 - 0.5 * cos((2.0*pi*i) / fromIntegral( length)) + 0.08 * cos((4.0*pi*i)/fromIntegral( length))):blackmanFunc (i2+1) length
 | i2==0 = 0 : blackmanFunc (i2+1) length
 | otherwise = []
  where i = fromIntegral i2

getIdealH::Int->Int->Double->Double->[Double]
getIdealH i2 length cutW cutFreq
 | i2==div length 2 = 2.0*cutFreq : getIdealH (i2+1) length cutW cutFreq
 | i2<length = (2.0*cutFreq*sin(cutW*(i-(fromIntegral length)/2.0))/(cutW*(i-(fromIntegral length)/2.0))): getIdealH (i2+1) length cutW cutFreq
 | otherwise = []
  where i = fromIntegral i2

digitalFilter signal length samplingFreq passFreq stopFreq
 = convolve signal impulseResponse
 where impulseResponse = generateCoef length samplingFreq passFreq stopFreq

convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
      -- суммируем попарное произведение импульсной характеристики и всех хвостов сигнала
  in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)

{-
main = do 
	h<-getContents
	let d = map read $ words h
	let d2 = evenFeat d
	mapM (putStrLn. show) $ digitalFilter ((reverse d2) ++ d2) 900 10000 15 25
-}

blackmanNuttalFunc::Int->Int->[Double]
blackmanNuttalFunc i2 length 
 | i2>0 && i2<length = (a0 - a1 * cos((2.0*pi*i) / fromIntegral( length)) + a2 * cos((4.0*pi*i)/fromIntegral( length))- a3* cos((6.0*pi*i)/fromIntegral( length))):blackmanNuttalFunc (i2+1) length 
 | i2==0 = 0 : blackmanNuttalFunc (i2+1) length
 | otherwise = []
  where i = fromIntegral i2
        a0 = 0.3635819
        a1 = 0.4891775
        a2 = 0.1365995
        a3 = 0.0106411

-- нужно реализовать аналог отражения и т.п. как в моей проге.
-- четное усреднение
evenFeat x | even (length x) = zipWith (\x y -> (x+y)/2) (reverse(take l x)) ((drop l x))
           | otherwise = [x!!((div (length x) 2) )] ++ ( evenFeat $ (take l x) ++ (drop (l+1) x))
	where l = div (length x) 2
		      

-- Так, испробуем экстраполяцию.
getColumn x a= [x!!(b+a) | b<-[0..l]] 
  where l = length x
 -- transpose x l a i= 

curveFitting x y len powPolinom = fmatrix x len powPolinom ++ [y]
	where a = powPolinom+1
	      fmatrix x len 0 = [replicate len 1]
	      fmatrix x len a = [map (^a) x ] ++ fmatrix x len (a-1)