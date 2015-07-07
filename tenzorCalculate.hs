
sampleLength = 3.25*1E-3

sampleWidth = 1.4*1E-3

sampleThickenss = 10.03*1E-6

i = 251.0*1E-6

sxx us uy = map (calculate) (zip s r)
 where s = s_eff us
       r = rh_eff uy
       calculate (s,r) = s/(r*r*s*s+1.0)

sxy us uy = map (calculate) (zip s r)
 where s = s_eff us
       r = rh_eff uy
       calculate (s,r) = s*s*r/(r*r*s*s+1.0)

rh_eff uy = map (*(sampleThickenss/i)) uy

s_eff us = map (koef/) us
	where koef=sampleLength/sampleWidth/sampleThickenss*i

b= map (/10) b1
	where b1 = map (realToFrac) [0,2..20]

tenzor us uy = zip3 b (sxx us uy) (sxy us uy)

main = do	
	  let sxxTheor=sxxFromParams b
	  let sxyTheor=sxyFromParams b
	  let s_effTheor=s_effFromTenzor sxxTheor sxyTheor
	  let rh_effTheor=rh_effFromTenzor sxxTheor sxyTheor
	  let usTheor=usFromEffective s_effTheor
	  let uyTheor=uyFromEffective rh_effTheor
	  let tenzorReverse= tenzor usTheor uyTheor
	  putStrLn "TenzorTheor"
	  putStrLn $show $zip3 b sxxTheor sxyTheor
	  --putStrLn "sxyTheor"
	  --putStrLn $show (sxyTheor)
	  putStrLn "s_effTheor"
	  --putStrLn $show (s_effTheor)
	  putStrLn "rh_effTheor"
	  --putStrLn $show (rh_effTheor)
	  putStrLn "usTheor"
	  putStrLn $show (usTheor)
	  putStrLn "uyTheor"
	  putStrLn $show (uyTheor)
	  putStrLn "tenzorReverse"
	  putStrLn $show (tenzorReverse)
	
us= [1.3082085816563453,1.3100926434289055,1.3135871492581412,1.318418455964526,1.324441982234018,1.331293346965898,1.3385847600547487,1.3459795477882157,1.3532187335353372,1.3601232274227686,1.366583456789775]

uy= [0.0,7.602120274874487e-3,1.5415408377464796e-2,2.2798722633043004e-2,2.9589088446772414e-2,3.5728802917382244e-2,4.121890380025751e-2,4.610386062877668e-2,5.0454797779135856e-2,5.435434064448049e-2,5.7885450821310365e-2]

e = 1.60217657E-19

ne = -7.08364496849683E16

mue = -5.0

pl = 5.36246431315553E19

pt = 1.0E22

mul = 0.5

mut = 0.025

-- Эти функции мы проверили. Результаты совпадают с моделью.
sxxFromParams::[Double]->[Double]
sxxFromParams b = map (sxxHepler) b
   where sxxHepler b = sum $ map (calculate) [(ne,mue),(pl, mul), (pt,mut)]
                       where
                        	calculate (n, mu) = e*n*mu/(1.0+mu*mu*b*b)
sxyFromParams::[Double]->[Double]
sxyFromParams b = map sxyHepler b
   where sxyHepler b = sum $ map (calculate) [(ne, mue),(pl, mul), (pt, mut)]
                        where 
                        	calculate (n, mu) = e*n*mu*mu*b/(1.0+mu*mu*b*b)

s_effFromTenzor sxx sxy = map (calculate) (zip sxx sxy)
 where 
       calculate (sxx,sxy) = (sxx*sxx+sxy*sxy)/sxx

rh_effFromTenzor sxx sxy = map (calculate) (zip sxx sxy)
 where 
       calculate (sxx,sxy) = sxy/(sxx*sxx+sxy*sxy)

usFromEffective s_eff = map (sampleLength/sampleWidth/sampleThickenss*i/) s_eff

uyFromEffective rh_eff = map (*(i/sampleThickenss)) rh_eff
