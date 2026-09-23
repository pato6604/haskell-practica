

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

esMontaniaRusa :: [Integer] -> Bool
esMontaniaRusa [] = True
esMontaniaRusa [x] = True
esMontaniaRusa [x,y] = True
esMontaniaRusa (x:y:z:xs) | (x < y) && (y > z) = esMontaniaRusa (y:z:xs)
                          | (x > y) && (y < z) = esMontaniaRusa (y:z:xs)
                          | otherwise = False 



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

invertir :: [Integer] -> [Integer] -> [Integer]
invertir [] _ = []
invertir (x:xs) [] =  invertir xs [] ++ [x]




ultimaAparicionDeUnParQueDivideAnAux :: [Integer] -> Integer -> Integer 
ultimaAparicionDeUnParQueDivideAnAux [] _ = 0 
ultimaAparicionDeUnParQueDivideAnAux (x:xs) n | (esPar x) && (n `mod` x == 0) = x 
                                              | otherwise = ultimaAparicionDeUnParQueDivideAnAux xs n 



ultimaAparicionDeUnParQueDivideAn :: [Integer] -> Integer -> Integer 
ultimaAparicionDeUnParQueDivideAn [x] _ = x 
ultimaAparicionDeUnParQueDivideAn (x:xs) n  = ultimaAparicionDeUnParQueDivideAnAux(invertir (x:xs) []) n 


----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

listaSinonimos :: [(String,String)] -> String -> [String]
listaSinonimos [] _ = []
listaSinonimos ((palabra,sinonimo): xs) p | p == palabra = sinonimo : listaSinonimos xs p 
                                          | otherwise  = listaSinonimos xs p 


eliminarClave :: String -> [(String,String)] -> [(String,String)]
eliminarClave _ [] = []
eliminarClave palabra ((x,y):xs) | palabra == x = eliminarClave palabra xs 
                                 | otherwise = (x,y) : eliminarClave palabra xs 


sinonimos :: [(String, String)] -> [(String, [String])]
sinonimos [] = []
sinonimos ((x,y):xs) = (x, listaSinonimos ((x,y):xs) x) : sinonimos (eliminarClave x xs)


-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

listaPorTemporada :: Integer -> [(String,Integer)] -> [String]
listaPorTemporada _ [] = []
listaPorTemporada temp ((serie,t):xs) | temp == t = serie : listaPorTemporada temp xs 
                                      | otherwise = listaPorTemporada temp xs 




quitarSeriesConMismasTemp :: Integer -> [(String, Integer)] -> [(String, Integer)]
quitarSeriesConMismasTemp _ [] = []
quitarSeriesConMismasTemp temp ((s,t):xs) | temp == t = quitarSeriesConMismasTemp temp xs 
                                          | otherwise = (s,t) : quitarSeriesConMismasTemp temp xs 
 


agruparPorCantidadDeTemporadas :: [(String, Integer)] -> [(Integer, [String])]
agruparPorCantidadDeTemporadas [] = []
agruparPorCantidadDeTemporadas ((s,t): xs) = (t, listaPorTemporada t ((s,t):xs)) : agruparPorCantidadDeTemporadas(quitarSeriesConMismasTemp t xs)


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

contarProducto :: [String] -> String -> Integer
contarProducto [] _ = 0
contarProducto (x:xs) p | x == p = 1 + contarProducto xs p 
                        | otherwise = contarProducto xs p 


quitarProducto :: [String] -> String -> [String]
quitarProducto [] _ = []
quitarProducto (x:xs) prod | x == prod = quitarProducto xs prod
                           | otherwise = x : quitarProducto xs prod 


generarStock :: [String] -> [(String,Integer)]
generarStock [] = []
generarStock (x:xs) = (x, contarProducto (x:xs) x) : generarStock (quitarProducto xs x)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sumaCuadradosPares :: Integer -> Integer
sumaCuadradosPares n | n == 1 = 0
                     | n `mod` 2 == 0 = n^2 + sumaCuadradosPares (n-1)
                     | otherwise = sumaCuadradosPares (n-1)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


sumaTodos :: [Integer] -> Integer
sumaTodos [] = 0
sumaTodos (x:xs) = x + sumaTodos xs 


promedio :: [Integer] -> Float 
promedio [] = 0.0
promedio (x:xs) = fromInteger(sumaTodos (x:xs))/fromInteger(longitud (x:xs))


mejorPromedio :: [(String,[Integer])] -> String
mejorPromedio [(nombre,notas)] = nombre 
mejorPromedio ((nombre1,notas1):(nombre2,notas2):xs) | promedio notas1 > promedio notas2 = mejorPromedio ((nombre1,notas1):xs) 
                                                     | promedio notas1 < promedio notas2 =  mejorPromedio((nombre2,notas2):xs) 
                                                     | otherwise = mejorPromedio ((nombre1,notas1):xs) 





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

materiasPorRangoH :: [(String,String,Integer,Integer)] -> Integer -> Integer -> [String]
materiasPorRangoH [] _ _ = []
materiasPorRangoH ((m,d,i,f):xs) inicio fin | i <= inicio && fin <= f = m : materiasPorRangoH xs inicio fin 
                                            | otherwise = materiasPorRangoH xs inicio fin 



quitarMateriasRepe :: [(String,String,Integer,Integer)] -> String -> [(String,String,Integer,Integer)]
quitarMateriasRepe [] _ = []
quitarMateriasRepe ((m,d,i,f): xs) materia | m == materia = quitarMateriasRepe xs materia
                                           | otherwise = (m,d,i,f) : quitarMateriasRepe xs materia 





materiasComisionN :: [(String,String,Integer,Integer)] -> Integer -> Integer -> [String]
materiasComisionN [] _ _ = []
materiasComisionN ((m,d,i,f):xs) inicio fin = materiasPorRangoH ((m,d,i,f):xs) inicio fin ++ materiasComisionN(quitarMateriasRepe((m,d,i,f):xs) m) inicio fin   

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



saturarEnUmbralHastaNegativo :: [Integer] -> Integer -> [Integer]
saturarEnUmbralHastaNegativo [x] _ = [x]
saturarEnUmbralHastaNegativo (x:y:xs) u | x * y < 0 = [u]
                                        | x <= u = x : saturarEnUmbralHastaNegativo (y:xs) u 
                                        | x > u = u : saturarEnUmbralHastaNegativo (y:xs) u 



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

esImpar :: Integer -> Bool
esImpar n = n `mod` 2 /= 0 



invertir2 :: [Integer] -> [Integer] -> [Integer] 
invertir2 [] _ = []
invertir2 (x:xs) [] = invertir2 xs [] ++ [x]


imparConMayorPosicionQueDivideANAux :: [Integer] -> Integer -> Integer 
imparConMayorPosicionQueDivideANAux [x] _ = x
imparConMayorPosicionQueDivideANAux (x:xs) n | (esImpar x) && (n `mod` x == 0) = x
                                             | otherwise = imparConMayorPosicionQueDivideANAux xs n 



imparConMayorPosicionQueDivideAN :: [Integer] -> Integer -> Integer
imparConMayorPosicionQueDivideAN (x:xs) n = imparConMayorPosicionQueDivideANAux(invertir2 (x:xs) []) n 



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x > y = maximo  (x:xs)
                | otherwise = maximo (y:xs)




obtenerFila :: [[Integer]] -> Integer -> [Integer] 
obtenerFila (x:xs) i | i == 1 = x 
                     | otherwise = obtenerFila xs (i-1)






filaDelMaximoAux :: [[Integer]] -> Integer -> Integer 
filaDelMaximoAux [x] i = i 
filaDelMaximoAux (x:xs) i | maximo x > maximo(obtenerFila (xs)(maxDelResto)) = i 
                          | otherwise = i + maxDelResto
                          where maxDelResto = filaDelMaximoAux xs 1


filaDelMaximo :: [[Integer]] -> Integer 
filaDelMaximo (x:xs) = filaDelMaximoAux (x:xs) 1 



---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


iesimoElemento :: [Integer] -> Integer -> Integer 
iesimoElemento [x] _ = x
iesimoElemento (x:xs) i | i == 1 = x 
                        | otherwise = iesimoElemento xs (i-1)



iesimaColumna :: [[Integer]] -> Integer -> [Integer]
iesimaColumna [x] i = [iesimoElemento x i] 
iesimaColumna (x:xs) i = iesimoElemento x i : iesimaColumna xs i 


contarParesColumna :: [Integer] -> Integer
contarParesColumna [] = 0 
contarParesColumna (x:xs) | esPar x = 1 + contarParesColumna xs 
                          | otherwise = contarParesColumna xs 





cantidadParesColumna :: [[Integer]] -> Integer -> Integer
cantidadParesColumna (x:xs) i = contarParesColumna(iesimaColumna (x:xs) i)




------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



minimo :: [Integer] -> Integer
minimo [x] = x 
minimo (x:y:xs) | x > y = minimo (y:xs) 
                | otherwise = minimo (x:xs)



iesimaFila :: [[Integer]] -> Integer -> [Integer]
iesimaFila (x:xs) i | i == 0 = x 
                    | otherwise = iesimaFila xs (i-1)


minimoDeFila :: [[Integer]] -> Integer -> Integer 
minimoDeFila [x] _ = minimo x 
minimoDeFila (x:xs) i = minimo(iesimaFila (x:xs) i)


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maximaSumaDeTresConsecutivos :: [Integer] -> Integer
maximaSumaDeTresConsecutivos [x,y,z] = x + y + z
maximaSumaDeTresConsecutivos (x:y:z:xs) | x + y + z > sumaDelResto = x + y + z 
                                        | otherwise = sumaDelResto
                                        where sumaDelResto = maximaSumaDeTresConsecutivos (y:z:xs)




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pertenece :: Char -> [Char] -> Bool
pertenece _ [] = False 
pertenece p (x:xs) | p == x = True
                   | otherwise = pertenece p xs 



quitarRepetidos :: [Char] -> [Char] 
quitarRepetidos [] = []
quitarRepetidos (x:xs) | pertenece x xs = quitarRepetidos xs 
                       | otherwise = x : quitarRepetidos xs 



compartidosAux :: [Char] -> [Char] -> [Char]
compartidosAux [] _ = []
compartidosAux (x:xs) p2 | x /= ' ' && pertenece x p2 = x : compartidosAux xs p2 
                         | otherwise = compartidosAux xs p2 


caracteresCompartidosSinEspacio :: [Char] -> [Char] -> [Char]
caracteresCompartidosSinEspacio p1 p2 = compartidosAux (quitarRepetidos p1) p2 


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


diferenciaAux :: [Char] -> [Char] -> [Char]
diferenciaAux [] _ = []
diferenciaAux (x:xs) p2 | x /= ' ' && not (pertenece x p2) = x : diferenciaAux xs p2
                        | otherwise  = diferenciaAux xs p2 



diferenciaSinEspacios :: [Char] -> [Char] -> [Char]
diferenciaSinEspacios p1 p2  = diferenciaAux(quitarRepetidos p1) p2  


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



filaCreciente :: [Integer] -> Bool
filaCreciente [x] = True
filaCreciente (x:y:xs) | x < y = filaCreciente (y:xs)
                       | otherwise = False
                    


columnasCrecientes :: [[Integer]] -> Bool
columnasCrecientes [x] = filaCreciente x 
columnasCrecientes (fila:filas) | filaCreciente fila = columnasCrecientes filas 
                                | otherwise = False 


    




-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cuantosIgualesAux :: [Char] -> [Char] -> Integer 
cuantosIgualesAux [] _ = 0 
cuantosIgualesAux (x:xs) p | x /= ' ' && pertenece x p = 1 + cuantosIgualesAux xs p 
                           | otherwise = cuantosIgualesAux xs p


cuantosIguales :: [Char] -> [Char] -> Integer 
cuantosIguales p1 p2 = cuantosIgualesAux(quitarRepetidos p1) p2 



-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

filaAlternada :: [Integer] -> Bool
filaAlternada [x] = True 
filaAlternada (x:y:xs) | x * y < 0 = filaAlternada (y:xs)
                       | otherwise = False 




matrizAlternada :: [[Integer]] -> Bool
matrizAlternada [x] = filaAlternada x 
matrizAlternada (x:xs) | filaAlternada x = matrizAlternada xs 
                       | otherwise = False 

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------