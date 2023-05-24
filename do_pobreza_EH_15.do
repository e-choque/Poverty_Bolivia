********************************************************
*******Determinantes de la Pobreza Bolivia 2015*********
*****************************************************
cls
clear all
cd "G:\seminarios_y_talleres\curso_stata_cegos\Material"
use "BOLIVIA_PERSONAS_2015", clear
set more off


/*___________________Generación y descripción de variables_______________________*/

*Descripcion de las variables
codebook condact
codebook s2a_02
codebook s2a_03
codebook f
sum s2a_03 s2a_02 if (s2a_03>14) & (s2a_03<65)

*renombrando la variable sexo, edad y escolaridad
ren s2a_02 sexo
ren s2a_03 edad
ren f escol
ren s2a_05 parentesco
rename s2a_10 civil

*Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen eje=0 
codebook departamento
*La Paz:2 Cochabamba:3 y Santa Cruz:7
* | este simbolo es "o" y & es el simbolo "y"
replace eje=1  if (departamento==2 | departamento==3 | departamento==7)

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

*Sabe leer o escribir
codebook s5a_1 /*Sabe leer o escribir*/
*renombrando las variables

ren s5a_1 leer_escribir
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
codebook s06b_16
ren s6b_21 size /* cuantas personas trabajan en tu empresas */
ren  s6b_16 ocup
gen cta_p=1 if ocup==3
replace cta_p=0 if cta_p==.
/* generacion de la variable informal*/ 
gen byte informal=(ocup==3 | ocup==7)
replace informal=1 if ((ocup==1 | ocup==2 | ocup==4 | ocup==5) & (size<5 & size>=2))
replace informal=0 if informal==.

codebook sexo 
gen sex=1  if sexo==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace  sex=0 if sexo!=1

gen married=1 if civil==2
replace married=0 if civil!=2

ovtest 
/* regla si p es menor a 0.05 se rechaza Ho, Nula dice que existe variabes omitidas*/

********************************************************************************
******************************* Logit ******************************************
********************************************************************************
					
logit pobre sexo edad trabaja escol eje [w=factor]  /*toda la población*/
logit pobre sexo edad trabaja escol eje [w=factor] if edad>=14 & edad<65 /*población en eda de trabajar*/

quietly logit p0 sexo edad trabaja escol indigena eje [w=factor] if edad>=14
logistic p0 sexo edad trabaja escol  eje [w=factor] if edad>=14
/*** cesar*/ 
predict pobre_hat_log
gen pobre_pre=1 if pobre_hat_log>0.5
recode pobre_pre .=0	
tab pobre pobre_pre

/* easy*/
global ylist pobre
global xlist sex  married edad trabaja escol  eje cta_p informal
describe $ylist $xlist
summarize $ylist $xlist
tabulate $ylist

logit pobre sex married  edad trabaja escol eje l_e [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sex married edad trabaja escol eje  l_e  [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sexo edad trabaja escol eje l_e  cta_p informal [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)
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

