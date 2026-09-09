contarMult3Hasta :: Integer -> Integer -> Integer
contarMult3Hasta d h | h == 0 = 0
                     | (d `mod` h == 0) && (h `mod` 3 == 0) = 1 + contarMult3Hasta d (h-1)  
                     | otherwise = contarMult3Hasta d (h-1)



cantDivMult3 :: Integer -> Integer
cantDivMult3 d = contarMult3Hasta d d 



masMultiplosDe3 :: Integer -> Integer -> Integer
masMultiplosDe3 d h | d == h = d        
                    | cantDivMult3 d > cantDivMult3 mejorResto = d
                    | otherwise = mejorResto
                    where mejorResto = masMultiplosDe3 (d+1) h


        

--------------------------------------------------------------------------------------------------------------------------

cantDivisores :: Integer -> Integer -> Integer -> Integer
cantDivisores n d h | h == 1 = 1
                    | n `mod` h == 0  = 1 + cantDivisores n d (h-1)
                    | otherwise = cantDivisores n d (h-1)



cuantosDiv :: Integer -> Integer
cuantosDiv n = cantDivisores n 1 n 




masDividido :: Integer -> Integer -> Integer
masDividido d h | d == h = d
                | cuantosDiv d > cuantosDiv num = d 
                | otherwise = num 
                where num = masDividido (d+1) h 


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

esPrimoAux :: Integer -> Integer -> Integer -> Bool
esPrimoAux n d h | d == h = True
                 | n `mod` d == 0 = False
                 | otherwise = esPrimoAux n (d+1) h



esPrimo :: Integer -> Bool
esPrimo n = esPrimoAux n 2 n 


cantPrimosDividen :: Integer -> Integer -> Integer 
cantPrimosDividen d h | h == 1 = 0 
                      | (d `mod` h == 0) && (esPrimo h) = 1 + cantPrimosDividen d (h-1)
                      | otherwise = cantPrimosDividen d (h-1)



cantDivisoresPrimos :: Integer -> Integer 
cantDivisoresPrimos n = cantPrimosDividen n n 
