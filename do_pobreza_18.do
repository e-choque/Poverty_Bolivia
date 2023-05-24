********************************************************
*******Determinantes de la Pobreza Bolivia 2017*********
*****************************************************
cls
clear all
cd "G:\seminarios_y_talleres\curso_stata_cegos\Material"
use "EH2017_persona", clear
set more off


/*___________________Generación y descripción de variables_______________________*/

*Descripcion de las variables
codebook condact
codebook s02a_02
codebook s02a_03
codebook aoesc
*renombrando la variable sexo, edad y escolaridad
ren s02a_02 sexo
ren s02a_03 edad
ren aoesc escol
ren s02a_05 parentesco
rename s02a_10 civil
sum edad sexo if (edad>14) & (edad<65)

*Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen eje=0
codebook depto
*La Paz:2 Cochabamba:3 y Santa Cruz:7
* | este simbolo es "o" y & es el simbolo "y"
replace eje=1 if (depto=="2" | depto=="3" | depto=="7")

label var eje "Departamentos del eje"
label define eje 1 "eje" 0 "no eje"
codebook eje

*generando dummy de trabaja
*Es "1" si la variable Condact ocupado:1 cesantes:2 aspirantes:3 inactivo temporal:4 y permanente:5
gen trabaja=0  
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace trabaja=1  if (condact==1 | condact==4 | condact==5)
label var trabaja "persona trabaja"
label define trabaja 1 "ocupado" 0 "no ocupado"
label values trabaja trabaja
tab tra* sexo [w=factor], row
tab tra* sexo [w=factor], col

***************************Incluyendo variables*********************************
********************************************************************************
********************************************************************************
 
              /***        Modelo microeconométrico_____________________________*/

******************MPL Modelo de Probabilidad Lineal*****************************

reg p0 sexo edad trabaja escol  eje [w=factor] if edad>=14
predict p0_mpl
*tiene problemas el MPL sobrepasa la probabilidad o es negativo. 

*nula es homocedastico * estadistico Breushc Pagan

reg p0 sexo edad trabaja escol  eje [w=factor] if edad>=14, robust
quietly reg p0 sexo edad trabaja escol eje [w=factor] if edad>=14

estat hett
sum p0_mpl
hist p0_mpl, normal
sktest p0_mpl
kdensity p0_mpl, normal
vif
hettest  /* Testear si existe problema de heterocedasticidad*/
reg p0 sexo edad trabaja escol eje if edad>=14 
reg p0 sexo edad trabaja escol  eje if edad>=14 & edad<65
imtest, white

*Sabe manejo TIC
codebook s05c_14a /*  ha utilizado computadora (de escritorio, laptop, table*/
*renombrando las variables
ren s05c_14a manejo_tic

gen tic=1 if manejo_tic==1
replace tic=0 if manejo_tic!=1
* kids for women por edades
cap drop kids6
gen kids6=(parentesco==3 & edad<=6) 
egen kids6a=total(kids6), by (folio)

gen kids6b=(kids6a>0)


cap drop kids714
gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio) 

** generando la variable binaria dependiente***
codebook p0 pext0
gen pobre=1 if p0==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace pobre=0 if p0!=1

*Pertenece a NPIOC=1
codebook s03a_04
gen indigena=1 if s03a_04==1 
*Persona no pertence a NPIOC nación o pueblo indigena ori. camp.
replace indigena=0 if s03a_04!=1


/* caracteristicas del mercado laboral */

* generacion de la variable informal  *


gen married=1 if civil==2
replace married=0 if civil!=2

ovtest 
/* regla si p es menor a 0.05 se rechaza Ho, Nula dice que existe variabes omitidas*/

********************************************************************************
******************************* Logit ******************************************
********************************************************************************
					
/* easy*/
*model 1
global ylist pobre
global xlist edad married indigena eje trabaja escol tic 
describe $ylist $xlist
summarize $ylist $xlist
tabulate $ylist

logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & sexo==1)

*model 2
global ylist pobre
global xlist edad married kids6a kids714a indigena  eje trabaja escol tic 
describe $ylist $xlist
summarize $ylist $xlist
tabulate $ylist

logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & sexo==1)
mfx
logistic $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)

/**marginal effect (at the mean and marginal effect)*/
quietly reg $ylist $xlist
margins, dydx(*) atmeans
margins, dydx(*)

quietly logit $ylist $xlist 
margins, dydx(*) atmeans
margins, dydx(*)  

quietly logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)
margins, dydx(*) atmeans
margins, dydx(*)

/*predicted probabilities*/
quietly logit $ylist $xlist
predict plogit, pr

quietly probit $ylist $xlist
predict pprobit, pr

quietly regress $ylist $xlist
predict pols, xb

summarize $ylist plogit pprobit pols
*percent correctly predict values
quietly logit $ylist $xlist
estat classification

quietly logit $ylist $xlist   [w=factor] if (edad>=14 & edad<65 & parentesco==1)
estat classification




*Tablas cruzadas de las variables
*Filas Columnas

tab pobre eje [w=factor], row
tab pobre eje [w=factor], col
tab pobre sexo [w=factor], row
tab pobre sexo [w=factor], col
tab pobre trabaja [w=factor], row
tab pobre trabaja [w=factor], col
tab escol p0 [w=factor],row
tab escol p0 [w=factor],col
tab pobre num_hijos [w=factor], row
tab pobre num_hijos [w=factor], col
tab pobre leer_escribir [w=factor], row
tab pobre leer_escribir [w=factor], col

*regresión de probabilidad de ser estar en pobreza extrema




