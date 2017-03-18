{-- JUEGO DEL 4 EN RAYA REALIZADO POR 		IVÁN CARRASCO ALONSO
    DNI: 70907531A	2ºCURSO DE GIISI	LENGUAJES DE PROGRAMACIÓN
    EPSZ
--}

import System.Environment	--Importamos el conjunto de funciones para la entrada y salida
import Data.List		--Importamos Data.List para usar la funcion que convierte una matriz en su transpuesta
import Text.Printf 		--Importamos Text.Printf para usar Printf

-------CREACION DE LA MATRIZ--------------------------------

{--Crea las columnas de la matriz--}
crear_columna 0 = []
crear_columna n = [0]++crear_columna (n-1)

{--Junta varias columnas creando una matriz--}
crear_tablero m 0 = []
crear_tablero m n =[crear_columna m]++crear_tablero m (n-1)

------------------------------------------------------------

----PINTA EL TABLERO DE JUEGO-------------------------------

{--Pinta el tablero--}
pinta x m i|i==(m-1)=do printf "\t\t" 
			print (x!!(m-1))
           |otherwise=do
			printf "\t\t"
           		print (x!!i)
           		pinta x m (i+1)
------------------------------------------------------------
	   
-------INSERCION DE FICHAS----------------------------------	
	
{--inserta  una ficha en la maxima fila vacia (0) de dicha columna empezando en la de arriba del todo a comprobar--}  
inserta_previo x n num=  if (length(quitaultima 0 x ))==(length x)
			then x
			else a
		where a = quitaultima2 0 x num
		
{--Actualiza la lista en la que se ha inserta_previo do un elemento--}
inserta [] n i num=[]
inserta (x:xs) n i num
			|i==n = [inserta_previo x 0 num]++inserta xs n (i+1) num
			|otherwise= [x]++inserta xs n (i+1) num

-------------------------------------------------------------
		    		     
------------------------CALCULO DEL GANADOR------------------

{--calcula si existen 4 valores iguales en vertical seguidos existentes con valor num-}	
 
calcula_vertical [] num rep m i = 0
calcula_vertical x num rep m i  = if rep>=4 then 1
				else
				if i==m then calcula_vertical (tail x) num 0 m 0
					else	if (x!!0!!i)==num then  calcula_vertical x num (rep+1) m (i+1)
							     	  else  calcula_vertical x num 0 m (i+1) 
							     	   
{--Calcula si existen 4 valores iguales en horizontal seguidos existentes con valor num-}	
						     
calcula_horizontal x num rep n i=calcula_vertical (transpose x) num 0 n 0 --la horizontal sera calcular la vertical a traves de la matriz traspuesta


{--esta funcion obtiene las diagonales de una matriz--}

diagonales []       = []
diagonales ([]:xss) = xss
diagonales xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonales (map tail xss)))

{--esta funcion junta en una lista las diagonales que van de oeste a suroeste y las que van de este a noroeste--}

diagonales_totales  [] = [] 
diagonales_totales xss = (diagonales xss)++(diagonales (reverse xss))

{--Calcula si existen 4 valores iguales en diagonal seguidos con valor num--}

calcula_diagonales [[]]  num rep i j=0
calcula_diagonales xss num rep i j=if rep>= 4 
	then 1
	else
		if (i>=length xss) 
			then 0
		else 
		if j>=length (xss!!i) then calcula_diagonales xss num 0 (i+1) 0
		else	
		if xss!!i!!j == num then calcula_diagonales xss num (rep+1) i (j+1)
		else calcula_diagonales xss num 0 i (j+1)
			
			
{--esta funcion comprueba si existe alguna posicion ganadora en el tablero--}

calcula xss num n m= if h==1 || v ==1 || d == 1 then 1 else 0
	where h= calcula_horizontal xss num 0 n 0
	      v= calcula_vertical xss num 0 m 0 
	      d= calcula_diagonales (diagonales_totales xss) num 0 0 0

{--Esta funcion calcula si todas las filas superiores del tablero se han llenado si ocurre esto existe un empate--}

empate [] = 0
empate xss =if (serepite 0 ((transpose xss)!!0) )== 0 then 1 else 0


---------------CALCULOS MAQUINA------------------------------------------------

calculo_maquina xss intro n m i |i==n= busca_central xss intro 0 n 0
				|otherwise=if (xss!!i!!0)==0 && (calcula (inserta xss i 0 1) 1 n m)==1  
	then i
	else calculo_maquina xss intro n m (i+1)


busca_central xss intro i n derizq= if xss!!intro!!0 /=0 
 then 
  if (intro+i)<n || (intro-i)>=0 
   then if (intro+i<n && derizq==0) 
     then if (xss!!(intro+i)!!0)/=0 
       then busca_central xss intro i n 1 
       else (intro+i)
					
   else if (xss!!(intro-i)!!0)/=0 
    then busca_central xss intro (i+1) n 0
    else (intro-i)
				
			     else (-1)
  else intro

-----------------TURNOS----------------------------------------

---------HOMBRE CONTRA HOMBRE----------------------------------

{--Esta funcion realiza los cambios de jugadores y muestra quien gana en el caso hombre vs hombre o el empate--}

hombrevshombre xss n m num= do
		if empate xss == 1 then printf "\n\t\t !!EMPATE¡¡\n" 
		else do
		printf "\n\t\tTURNO DEL JUGADOR %d\n" (num::Int)
		printf "-Introduce una ficha en la columna:\n"
		 
		line <- getLine 
		let col=atoi(line)
		if col<=0 || col>n then do printf"\n\tError, seleccione una columna entre 1 y %d\n" (n::Int)
					   hombrevshombre xss n m num
				  else do
		if (xss!!(col-1)!!0) /= 0 then do printf "\n\t Error, columna llena seleccione otra columna\n"
					          hombrevshombre xss n m num
		else do
		let a=inserta xss (col-1) 0 num
		pinta (transpose a) m 0
		if (calcula a num n m) == 1 then printf "\n\t\t!!GANA EL JUGADOR %d¡¡\n" num 
		else if num ==1 then hombrevshombre a n m 2
				else hombrevshombre a n m 1

---------------------------------------------------------------------------

------------------HOMBRE CONTRA MAQUINA------------------------------------

{--Esta función realiza los cambios de turno entre hombre y maquina y acaba cuando alguno de los dos ganan o empatan--}

hombrevsmaquina xss n m num =do if empate xss == 1 then printf "\n\t\t EMPATE\n" 
				else do
			       	if num == 1 
				then do 
					printf "\n\t\tTURNO DEL HOMBRE (%d)\n" num 
					printf "-Introduce una ficha en la columna:\n"
					line <- getLine 
					let col=atoi(line)
					if col<=0 || col>n 
						then do printf"\n\tError, seleccione una columna entre 1 y %d\n" (n::Int)
							hombrevsmaquina xss n m num
						else do
							if (xss!!(col-1)!!0) /= 0 
								then do printf "\n\t Error, columna llena seleccione otra columna\n"
					          			hombrevshombre xss n m num
							else do
								let a=inserta xss (col-1) 0 num
								pinta (transpose a) m 0
								if (calcula a num n m) == 1 
									then printf "\n\t\t!!GANA EL HOMBRE¡¡\n"  
									else hombrevsmaquina a n m 2
				  
				else do
						printf "\n\t\tTURNO DE LA MAQUINA(%d)\n" num
						let intro=n`div`2
						let d=calculo_maquina xss intro n m 0
						printf "La maquina introduce una ficha en la columna %d\n\n" (d+1)
						let e=inserta xss d 0 num
						pinta(transpose e) m 0
						if(calcula e num n m) == 1
							then printf"\n\t\t !!GANA LA MAQUINA¡¡\n\n"
							else hombrevsmaquina e n m 1
						
												   
						   
						   
-----------------------------------------------------------------------------		

----FUNCIONES AUXILIARES-------------------------------------------------------

{--Esta funcion quita el ultimo elemento de una lista que tenga un elemento x y le añade [num],lo sustituye por num--}
quitaultima2 x []  num   = []
quitaultima2 x (l:ls) num= if x == l && c==0 then [num]++ls 						
	else [l]++quitaultima2 x ls num
	where c = serepite x ls

{--Esta funcion quita el ultimo elemento de la lista que tenga un elemento x --}
quitaultima x []     = []
quitaultima x (l:ls) = if x == l && c==0 then ls 						
	else [l]++quitaultima x ls
	where c = serepite x ls
{--Esta funcion cuenta cuantas veces se repite un elemento en la lista--}
serepite x [] 	  = 0
serepite x (l:ls) = if x==l then 1+i else i
	where i = serepite x ls
{-- Convierte de un string a un número entero--}
atoi::String->Int
atoi=read

-------------------------------------------------------------------------------	



----FUNCION PRINCIPAL----------------------------------------------------------
{--Toma los argumentos y comprueba que la n sea >= 7 y la m >= 6 sino dará error, despues si existe mas de 2 parametros entonces se juega contra la maquina y si hay 2 parametros entonces se juega una persona con otra persona--}
main = do
	arg<-getArgs 
	let m=atoi(arg!!1)
	let n=atoi(arg!!0)
	let xss=(crear_tablero m n)
	
	
	if n<7 || m<6 then error"Argumentos erroneos: columnas → arg(1) debe ser>=7 y filas → arg(2)>=6" 
		else 
		if length arg>2 then do printf "\n\t\tHUMANO VS MAQUINA\n\n" 
					pinta(transpose xss) m 0
					hombrevsmaquina xss n m 2
					
				else do printf "\n\t\tHUMANO VS HUMANO\n\n"
					pinta(transpose xss) m 0
					hombrevshombre xss n m 1
	                                   
--------------------------------------------------------------------------------
	
	
	
	 
	 
	 
	 

