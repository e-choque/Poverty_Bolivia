********************************************************
*******Determinantes de la Pobreza Bolivia 2013*********
*****************************************************
cls
clear all
cd "C:\Users\usuario1\Desktop"
use "bolivia_personas_2013", clear
set more off


/*___________________Generación y descripción de variables_______________________*/

*Descripcion de las variables
codebook condact
codebook s2_02
codebook s2_03
codebook e
sum s2_03 s2_02 if (s2_03>14) & (s2_03<65)

*renombrando la variable sexo, edad y escolaridad
ren s2_02 sexo
ren s2_03 edad
ren e escol
ren s2_05 parentesco
rename s2_10 civil

*Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen eje=0
codebook id01
*La Paz:2 Cochabamba:3 y Santa Cruz:7
* | este simbolo es "o" y & es el simbolo "y"
*replace eje=1 if (id01=="2" | id01=="3" | id01=="7")
replace eje=1 if (id01==2 | id01==3 | id01==7)

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

***************************Incluyendo variables*********************************
********************************************************************************
********************************************************************************
 
/***Modelo microeconométrico_____________________________*/

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

*Incorporando las variables Area:1 Urbana Area:2 Rural 
*Cuanto hijos vivos tiene actualmente
*Sabe leer o escribir
codebook hnv_ult/*Cuanto hijos vivos tiene actualmente*/
codebook s5_01 /*Sabe leer o escribir*/
*renombrando las variables
codebook s4_12
ren s4_12 num_hijos 
ren s5_01 leer_escribir
** generando la variable binaria dependiente***
codebook p0 pext0
gen pobre=1 if p0==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace pobre=0 if p0!=1

codebook leer_escribir
gen  l_e=1 if leer_escribir==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace  l_e=0 if leer_escribir!=1

/* caracteristicas del mercado laboral */

/* generacion de la variable cuenta propista */
codebook s6_16
ren s6_20  size /* cuantas personas trabajan en tu empresas */
ren  s6_16 ocup
gen cta_p=1 if ocup==3
replace cta_p=0 if cta_p==.
/* generacion de la variable informal*/ 
gen byte informal=(ocup==3 | ocup==7)
replace informal=1 if ((ocup==1 | ocup==2 | ocup==4 | ocup==5) & (size<5 & size>=2))
replace informal=0 if informal==.

ovtest 
/* regla si p es menor a 0.05 se rechaza Ho, Nula dice que existe variabes omitidas*/

********************************************************************************
******************************* Logit ******************************************
********************************************************************************
					
logit pobre sexo edad trabaja escol eje [w=factor]  /*toda la población*/
logit pobre sexo edad trabaja escol eje [w=factor] if edad>=14 & edad<65 /*población en eda de trabajar*/

quietly logit p0 sexo edad trabaja escol area eje [w=factor] if edad>=14
logistic p0 sexo edad trabaja escol  eje [w=factor] if edad>=14
/*** cesar*/ 
predict pobre_hat_log
gen pobre_pre=1 if pobre_hat_log>0.5
recode pobre_pre .=0	
tab pobre pobre_pre


*civil
/* easy*/
global ylist pobre
global xlist sexo married edad trabaja escol eje num_hijos l_e totper cta_p informal
describe $ylist $xlist
summarize $ylist $xlist
tabulate $ylist

logit pobre sexo edad trabaja escol eje num_hijos l_e [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sexo edad trabaja escol eje num_hijos l_e totper [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sexo edad trabaja escol eje num_hijos l_e totper cta_p informal [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logistic $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sexo edad trabaja escol eje  l_e [w=factor] if (edad>=14 & edad<65 &  num_hijos<=14 & parentesco==1) 
logit p0 sexo edad trabaja escol eje num_hijos leer_escribir [w=factor] if (edad>=14 & edad<65)

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




