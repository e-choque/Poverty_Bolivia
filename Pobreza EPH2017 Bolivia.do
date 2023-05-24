********************************************************************
*********************Pobreza Bolivia EPH - 2017*********************
********************************************************************


/**********************Preparación del Entorno de trabajo************************/
cls
clear all
cd "C:\Users\usuario1\Desktop\Base Encuesta Permanente de Hogares"
use "EH2017_persona", clear
set more off


/*___________________Generación y descripción de variables_______________________*/

*Descripcion de las variables
codebook condact
codebook s02a_02
codebook s02a_03
codebook aoesc
sum s02a_03 s02a_02 if (s02a_03>14) & (s02a_03<65)
codebook s02a_05
codebook s02a_10
*With Jefa o Jefe de Hogar
sum s02a_03 s02a_02 if (s02a_03>14) & (s02a_03<65) & (s02a_05==1)

*Renombrando la variable sexo, edad y escolaridad
ren s02a_02 sexo
ren s02a_03 edad
ren aoesc escol
ren s02a_05 parentesco
rename s02a_10 civil

*Generate Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen eje=0
codebook depto
*La Paz:2 Cochabamba:3 y Santa Cruz:7
* | este simbolo es "o" y & es el simbolo "y"
replace eje=1 if (depto=="2" | depto=="3" | depto=="7")

label var eje "Departamentos del eje"
label define eje 1 "eje" 0 "no eje"
codebook eje

*generando dummy de trabaja

*Es "1" si la variable Condact (*)ocupado:1 cesantes:2 aspirantes:3 inactivo temporal:4 e Incativo permanente:5
gen trabaja=1 if condact==1
*TODOS MENOS "UNO" !=1 no igual=1 (distinto a "1!)
*cuando la persona va a ser 2 3 4 5 la variables trabaja sera 0
replace trabaja=0 if condact!=1

label var trabaja "persona trabaja"
label define trabaja 1 "ocupado" 0 "no ocupado"
label values trabaja trabaja
tab tra* sexo [w=factor], row
tab tra* sexo [w=factor], col


* Generando la variable Informal


/*La empresa, negocio, taller o establecimiento donde trabaja, ¿cuenta con NIT*/
codebook s06b_19
tab s06b_19 sexo if (edad>14) & (edad<65) [w=factor]

gen informal=1 if s06b_19==3
*replace informal=0 if s06b_19!=1
replace informal=0 if (s06b_19==1 | s06b_19==2)

tab informal sexo if (edad>14) & (edad<65) [w=factor], col

tab informal sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col


/*¿En su actual ocupación Ud. Recibe o recibirá los siguientes beneficios: Seguro*/

gen informal1=1 if s06c_29b==2
replace informal1=0 if s06c_29b ==1 

tab informal1 sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col


/*¿Esta usted afiliado a:  AFP (Administradora de  Fondos de Pensiones)?*/


gen informal2=1 if s06h_58b==2
replace informal2=0 if s06h_58b ==1 

tab informal2 sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col


*______________________________________________________________________________*

gen p_informal=1 if (s06b_19==3 | s06c_29b==2 | s06h_58b==2)
replace p_informal=0 if p_informal==. 

tab p_informal sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col

*______________________________________________________________________________*
gen pp_informal=1 if (s06b_19==3 | s06c_29b==2)
replace pp_informal=0 if pp_informal==. 

tab pp_informal sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col

*USAR PP_INFORMAL (Sin oonsiderar AFP´s)


*Informalidad por Clasificación de Actividad Económica de Bolivia Ocupacion principal
tab caeb_op pp_informal [w=factor], col




********************************************************************************
*****''''¿¿ **     ************************************************************************
***** ******* *******************************************************************
***** *******   ***********************************************
***** ******* ********************************************************************
*****      **     *************************************************************************
********************************************************************************








********************************************************************************
********************************************************************************
***************************Estimación del modelo********************************
********************************************************************************
********************************************************************************

/*_______________________Modelo microeconométrico______________________________*/

******************MPL Modelo de Probabilidad Lineal*****************************

reg p0 sexo edad trabaja escol  eje [w=factor] if edad>=14
predict p0_mpl
*tiene problemas el MPL sobrepasa la probabilidad o es negativo. 

*nula es homocedastico * estadistico Breusch Pagan

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
codebook s05a_01 /*Sabe leer o escribir*/
*renombrando las variables
codebook s04b_12
ren s05a_01 leer_escribir
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
ren s06b_20  size /* cuantas personas trabajan en tu empresas */
ren  s06b_16 ocup
gen cta_p=1 if ocup==3
replace cta_p=0 if cta_p==.
/* generacion de la variable informal*/ 
gen byte informal=(ocup==3 | ocup==7)
replace informal=1 if ((ocup==1 | ocup==2 | ocup==4 | ocup==5) & (size<5 & size>=2))
replace informal=0 if informal==.

codebook sexo 
gen sex if sexo=1
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
					
logit pobre sexo edad trabaja escol pp_informal eje [w=factor]  /*toda la población*/
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
global xlist sex  married edad trabaja escol eje  l_e totper cta_p informal
describe $ylist $xlist
summarize $ylist $xlist
tabulate $ylist

logit pobre sex edad trabaja escol eje  l_e [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sex edad trabaja escol eje  l_e totper [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit pobre sexo edad trabaja escol eje l_e cta_p informal [w=factor] if (edad>=14 & parentesco==1)
logit $ylist $xlist [w=factor] if (edad>=14 & edad<65 & parentesco==1)
logit $ylist $xlist [w=factor] if (edad>=14  & parentesco==1)
logit $ylist $xlist [w=factor] 


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




