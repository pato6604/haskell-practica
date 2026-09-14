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


---AUXILIAR----
cantPrimosDividen :: Integer -> Integer -> Integer 
cantPrimosDividen d h | h == 1 = 0 
                      | (d `mod` h == 0) && (esPrimo h) = 1 + cantPrimosDividen d (h-1)
                      | otherwise = cantPrimosDividen d (h-1)


---PRINCIPAL---
cantDivisoresPrimos :: Integer -> Integer 
cantDivisoresPrimos n = cantPrimosDividen n n 





masPrimosQueLoDividen :: Integer -> Integer -> Integer
masPrimosQueLoDividen d h | d == h = d
                          | cantDivisoresPrimos d >= cantDivisoresPrimos res = d
                          | otherwise = res
                          where res = cantDivisoresPrimos (d+1) 




-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


divisoresPropiosAux :: Integer -> Integer -> [Integer]
divisoresPropiosAux d h | d == h = []
                        | h `mod` d == 0 = d : divisoresPropiosAux (d+1) h
                        | otherwise = divisoresPropiosAux (d+1) h 



divisoresPropios :: Integer -> [Integer]
divisoresPropios n = divisoresPropiosAux 1 n 




suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs 



masAbundante :: Integer -> Integer -> Integer
masAbundante d h | d == h = d   
                 | suma(divisoresPropios d) >= mejorR = d 
                 | otherwise = mejorR
                 where mejorR = masAbundante (d+1) h




--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cantMultiplosEnRango :: Integer -> Integer -> Integer -> Integer
cantMultiplosEnRango x d h | d > h = 0
                           | d `mod` x == 0 = 1 + cantMultiplosEnRango x (d+1) h
                           | otherwise = cantMultiplosEnRango x (d+1) h 



masMultiplos :: Integer -> Integer -> Integer
masMultiplos d h | d == h = d 
                 | cantMultiplosEnRango d d h <= cantMultiplosEnRango mejorDelResto d h = mejorDelResto
                 | otherwise = d
                 where mejorDelResto = masMultiplos (d+1) h 


    
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


sumarDigitos :: Integer -> Integer 
sumarDigitos n | n < 10 = n
               | otherwise = (n `mod` 10) + sumarDigitos (n `div` 10) 


masSumaDigitos :: Integer -> Integer -> Integer
masSumaDigitos d h | d == h = d 
                   | sumarDigitos d >= sumarDigitos mejor = d 
                   | otherwise = mejor 
                   where mejor = masSumaDigitos (d+1) h 


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

esPar :: Integer -> Bool
esPar n = n `mod` 2 == 0 


divisoresParesDeNAux :: Integer -> Integer -> Integer -> [Integer]
divisoresParesDeNAux n d h | d > h = []
                           | (n `mod` d) == 0 && (esPar d) = d : divisoresParesDeNAux n (d+1) h 
                           | otherwise = divisoresParesDeNAux n (d+1) h 




divisoresPares :: Integer -> [Integer]
divisoresPares n = divisoresParesDeNAux n 1 n 



longitud :: [Integer] -> Integer 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 




masDivisoresPares :: Integer -> Integer -> Integer 
masDivisoresPares d h | d == h = longitud(divisoresPares d)
                      | longitud(divisoresPares d) <= longitud(divisoresPares resto) = resto 
                      | otherwise = d 
                      where resto = masDivisoresPares (d+1) h  





---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

factoresPrimosAux :: Integer -> Integer -> [Integer]
factoresPrimosAux n d | n == 1 = []
                      | n `mod` d == 0 = d : factoresPrimosAux (n `div` d) d    
                      | otherwise = factoresPrimosAux n (d+1)

                
factoresPrimos :: Integer -> [Integer]
factoresPrimos n = factoresPrimosAux n 2 



esAtractivo :: Integer -> Bool
esAtractivo n | n == 1 = False
              | esPrimo(longitud(factoresPrimos(n))) = True





-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


sonAmigos :: Integer -> Integer -> Bool
sonAmigos n m | (suma(divisoresPropios n) == m) && (suma(divisoresPropios m) == n) = True 
              | otherwise = False

    



esPerfecto :: Integer -> Bool 
esPerfecto n | suma(divisoresPropios n) == n = True 
             | otherwise = False



losPrimerosNPerfectosAux :: Integer -> Integer ->  [Integer]
losPrimerosNPerfectosAux n d   | n == 0 = []
                               | esPerfecto d = d : losPrimerosNPerfectosAux (n-1) (d+1)  
                               | otherwise = losPrimerosNPerfectosAux n (d+1)  



losPrimerosNPerfectos :: Integer -> [Integer]
losPrimerosNPerfectos n = losPrimerosNPerfectosAux n 1 




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


nEsimoPrimoAux :: Integer -> Integer -> Integer 
nEsimoPrimoAux n d | n == 0 = (d-1) 
                   | esPrimo d = nEsimoPrimoAux (n-1) (d+1) 
                   | otherwise = nEsimoPrimoAux n (d+1)



nEsimoPrimo :: Integer -> Integer 
nEsimoPrimo n = nEsimoPrimoAux n 2 

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





