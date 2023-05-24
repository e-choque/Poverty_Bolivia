********************************************************
*******Determinantes de la Pobreza Bolivia 2015*********
********************************************************

/*____________________________Entorno de trabajo_________________________________*/
cls
clear all

*use "C:\Users\usuario1\Desktop\Modelo microeconométrico EH 2015\eh2015_persona.dta", clear

*cd "C:\Users\usuario1\Desktop\Modelo microeconométrico EH 2015"
cd "C:\Users\usuario1\Desktop"
use "eh2015_persona.dta", clear
set more off 

/*___________________Generación y descripción de variables_______________________*/

*Descripcion de las variables
codebook condact
codebook s2a_02
codebook s2a_03
*Estadisticas de las variables 
sum s2a_03 s2a_02 if (s2a_03>14) & (s2a_03<65)
codebook condact
*renombrando la variable sexo, edad y escolaridad
ren s2a_02 sexo
ren s2a_03 edad
ren e escol
ren s2a_05 parentesco
rename s2a_10 civil
*Generando variable experiencia
gen expe=edad-escol-6
replace expe=0 if expe<0
***Generación de la variable indigena***
*Pertenece a NPIOC=1
codebook s3a_2a
gen indigena=1 if s3a_2a==1 
*Persona no pertence a NPIOC nación o pueblo indigena ori. camp.
replace indigena=0 if s3a_2a==2
replace indigena=0 if s3a_2a==3
*replace indigena=0 if s3a_2a!=1
*Agregar etiquetas a los datos
label var indigena "dummy indigena"
label define indigena 1 "indigena" 0 "no indigena"
label values indigena indigena

*Estadísticos de las variables
*Sin factor de expansión
tabulate indigena sexo, row
tabulate indigena sexo, col
*Con factor de expansión
tabulate indigena sexo [w=factor], row
tabulate indigena sexo [w=factor], col

*Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen eje=0
codebook departamento
*La Paz:2 Cochabamba:3 y Santa Cruz:7
* | este simbolo es "o" y & es el simbolo "y"
replace eje=1 if (departamento==2 | departamento==3 | departamento==7)

label var eje "Departamentos del eje"
label define eje 1 "eje" 0 "no eje"
codebook eje

*generando dummy de trabaja
*Es "1" si la variable Condact ocupado:1 cesantes:2 aspirantes:3 inactivo temporal:4 y permanente:5
gen trabaja=1 if condact==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace trabaja=0 if condact!=1
label var trabaja "persona trabaja"
label define trabaja 1 "ocupado" 0 "no ocupado"
label values trabaja trabaja
tab tra* sexo [w=factor], row
tab tra* sexo [w=factor], col


********************************************************************************
********************************************************************************
********************************************************************************
***************************Incluyendo variables*********************************
********************************************************************************
********************************************************************************
* Estado civil
cap drop married
gen married=(civil==2)
cap drop single
gen single=(civil==1) 
cap drop divorced
gen divorced=(civil==5)
 
* Hijos por edades
cap drop kids6
gen kids6=(parentesco==3 & edad<=6) 
egen kids6a=total(kids6), by (folio)

gen kids6d=(kids6a>0)

cap drop kids714
gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio)

gen kids714d=(kids714a>0)


********************************************************************************
********************************************************************************
************Los determinates de que una persona sea Pobre o no.*****************
********************Probabilidad de ser pobre***********************************
********************************************************************************


/*_________________________Modelo microeconométrico_____________________________*/

******************MPL Modelo de Probabilidad Lineal*****************************

reg p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14
predict p0_mpl
*tiene problemas el MPL sobrepasa la probabilidad o es negativo. 

*nula es homocedastico * estadistico Breushc Pagan

reg p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14, robust
quietly reg p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14

estat hett
sum p0_mpl
hist p0_mpl, normal
sktest p0_mpl
kdensity p0_mpl, normal
vif
hettest  /* Testear si existe problema de heterocedasticidad*/
reg p0 sexo edad trabaja escol indigena eje if edad>=14 
reg p0 sexo edad trabaja escol indigena eje if edad>=14 & edad<65
imtest, white

*Incorporando las variables Area:1 Urbana Area:2 Rural 
*Cuanto hijos vivos tiene actualmente
*Sabe leer o escribir
codebook s4b_10 /*Cuanto hijos vivos tiene actualmente*/
codebook s5a_1 /*Sabe leer o escribir*/
*renombrando las variables
ren s4b_10 num_hijos 
ren s5a_1 leer_escribir

*Para el area Urbana
reg p0 sexo edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) & (area==1)
*Para el area Rural
*reg p0 sexo edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) & (area==0)

reg p0 sexo edad trabaja escol indigena eje leer_escribir [w=factor] if (edad>=14) & (area==1)

*La probabilidad de ser probre:
reg p0 edad trabaja escol indigena eje num_hijos [w=factor] if (edad>=14) & (area==1)
**La probabilidad de ser probre: para mujeres respecto a los hombres
reg p0 sexo edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14)


*Test de sobre/sub identificación del modelo
ovtest 
/* regla si p es menor a 0.05 se rechaza Ho, Nula dice que existe variabes omitidas*/


/*______________________________________________________________________________*/
/*_____________________Estimacion del Modelo LOGIT y PROBIT_____________________*/
/*______________________________________________________________________________*/

********************************************************************************
****************************** Probit ******************************************
********************************************************************************

*P0 pobreza moderada=0 (la persona no es pobre)	P0=1 la persona es pobre
*Edad es continua años completos, trabaja es binaria, escolaridad es discreta y continua
*indigena es una variable binaria, eje es dicotomica. y el factor expansion para toda la poblacion el universo

probit p0 sexo edad trabaja escol indigena eje [w=factor]  /*toda la población*/
* la prob. de ser pobre, para mujeres
probit p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14 /*población en edad de trabajar*/
***Efectos marginales
margins, dydx(*) atmeans  
margins, dydx(*)

*La prob. de ser pobre si eres hombre
generate hombre=1 if sexo==1
replace hombre=0 if sexo==2
probit p0 hombre edad trabaja escol indigena eje [w=factor] if (edad>=14)
*La prob. de ser pobre si eres mujer
gen mujer=1 if sexo==2
replace mujer=0 if sexo==1
probit p0 mujer edad trabaja escol indigena eje [w=factor] if (edad>=14)

*Probit "si la persona sabe leer o escribir"
probit p0 sexo edad trabaja escol indigena eje leer_escribir [w=factor] if (edad>=14) 
*Probit con "numero de hijos"
probit p0 edad trabaja escol indigena eje num_hijos [w=factor] if (edad>=14)
*Probit con Ambas variables
probit p0 edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) 

/*____________________________ODDs Ratios_______________________________________*/
quietly probit p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14
logistic p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14
predict p0_hat_prob
gen p0_pred=0 if p0_hat_prob>0.5
recode p0_pred .=0	
tab p0 p0_pred


*odd=#eventos favorables/#eventos no favorables

estat classification

*bondad de ajuste de este modelo 
*Ajuste de la prediccion.


********************************************************************************
******************************* Heckman Model***********************************
********************************************************************************
generate dp0=p0>0
heckman p0 sexo edad trabaja escol indigena eje, nocon select(dp0=sexo edad trabaja escol indigena eje) twostep 
mfx


********************************************************************************
******************************* Logit ******************************************
********************************************************************************
					
logit p0 sexo edad trabaja escol indigena eje [w=factor]  /*toda la población*/
logit p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14 /*población en eda de trabajar*/

quietly logit p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14
logistic p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14

predict p0_hat_log
gen p0_pre=1 if p0_hat_log>0.5
recode p0_pre .=0	
tab p0 p0_pre

logit p0 sexo edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) & (area==1)
logit p0 sexo edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14)
*Analizando los aciertos
estat classification /*Validacion de la prediccion*/
***************************************************************
***************************************************************


*Tablas cruzadas de las variables
*Filas Columnas

tab p0 eje [w=factor], row
tab p0 eje [w=factor], col
tab p0 sexo [w=factor], row
tab p0 sexo [w=factor], col
tab p0 trabaja [w=factor], row
tab p0 trabaja [w=factor], col
tab escol p0 [w=factor],row
tab escol p0 [w=factor],col
tab p0 indigena [w=factor],row
tab p0 indigena [w=factor],col
tab p0 num_hijos [w=factor], row
tab p0 num_hijos [w=factor], col
tab p0 leer_escribir [w=factor], row
tab p0 leer_escribir [w=factor], col


*regresión de probabilidad de ser pobre moderado
probit p0 sexo edad trabaja escol indigena eje [w=factor], robust
logit p0 sexo edad trabaja escol indigena eje [w=factor], robust

probit p0 edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) 
logit p0 edad trabaja escol indigena eje num_hijos leer_escribir [w=factor] if (edad>=14) 
*******************************************************************************
*La pobreza en esta regresion , en bolivia en el año 2015 era de 36%.
*Los mujeres tienen la prob de 0,697% de ser pobre con respecto a los hombre
*Por un aÃño adicional de la persona, hay una prob de 0,2% de no ser pobre
*la estrella indica si la var es Dummy
*Si la person trabaja tiene un prob de 3% de no ser pobre
*si eres indigena, tiene la prob de 12,7% de ser pobre
*Solo con elegir la variable indigena, explicarian gran parte.
*Las persona que viven en el eje tienen x prob en comparacion a otra persona que no viven 
*en el eje, tienen menos prob de ser pobres.
*Analizando los aciertos
*estat classification
*Correctamente clasifgicados, al hacer las vairables estamos acertando 64% de los casos que son pobres,
*ese es nuestro ajuste




*Analizando probabilidades
predict a1 if sexo==1 & trabaja==0   /*analizando la probabilidad que un hombre desempleado sea pobre*/ 
sum a1 [w=factor]

predict a2 if sexo==2 & trabaja==0   /*analizando la probabilidad que una mujer desempleada sea pobre*/
sum a2 [w=factor]

predict a3 if indigena==1 & eje==1   /*analizando la probabilidad que un indígena que vive en el eje sea pobre*/
sum a3 [w=factor]

predict a4 if indigena==1 & eje==0   /*analizando la probabilidad que un indígena que no vive en el eje sea pobre*/
sum a4 [w=factor]



*tabulados simples
*ej. relación de parentesco (s2a_05) y sexo (s2a_02)
tab s2a_05 sexo 						  		/*sin factor de expansión*/
tab s2a_05 sexo [w=factor]    				/*con factor de expansión */
tab s2a_05 sexo if edad>=10 [w=factor]    	/*con factor de expansión */

*tabulados simples con variables promedio
*ej, ingresos promedio por sexo
tab sexo [w=factor], sum(yhogpc)    		/*ingreso del hogar percápita, por sexo*/
tab sexo p0 [w=factor], sum(yhogpc)    	/*ingreso del hogar percápita, por sexo y pobreza */

