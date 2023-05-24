********************************************************************
*********************Pobreza Bolivia EPH2017 *********************
********************************************************************
*** check
/**********************Preparación del Entorno de trabajo*********************/
clear 
cd "G:\seminarios_y_talleres\curso_stata_cegos\Material"
use "EH2017_persona", clear
set more off
ren s02a_02 sexo
ren s02a_03 edad
ren aoesc escol
ren s02a_05 parentesco
rename s02a_10 civil
cap drop eth
gen eth=(s03a_04==1)
*Generate Dummy para los departamentos del eje central central:1 y 0 para el resto de los deptos.
gen occidental=0
replace occidental=1 if (depto=="2" | depto=="4" | depto=="5")

gen trabaja=0
replace trabaja=1 if (condact==1 | condact==5)

gen p_informal=1 if (s06b_19==3 | s06c_29b==2 | s06h_58b==2)
replace p_informal=0 if p_informal==. 
tab p_informal sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col

gen pp_informal=1 if (s06b_19==3 | s06c_29b==2)
replace pp_informal=0 if pp_informal==. 

tab pp_informal sexo if (edad>14) & (edad<65) & (trabaja==1) [w=factor], col
*Sabe manejo TIC
codebook s05c_14a /*  ha utilizado computadora (de escritorio, laptop, table*/
*renombrando las variables
ren s05c_14a manejo_tic

gen tic=1 if manejo_tic==1
replace tic=0 if manejo_tic!=1


gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio) 

gen pext0=1 if p0==1
replace pext0=0 if p0!=1

cap drop eth
gen eth=(s03a_04==1)

gen married=1 if civil==2
replace married=0 if civil!=2

gen educ_sup=0 
replace educ_sup=1 if (s05a_02a==71 | s05a_02a==72 | s05a_02a==73 | s05a_02a==74 | s05a_02a==75 | s05a_02a==76| s05a_02a==77 | s05a_02a==78 | s05a_02a==79 | s05a_02a==80 )      

gen cob_salud=0 
replace cob_salud=1 if (cobersalud== 1 |cobersalud== 2 |cobersalud== 3 )


****** MCO para el ingreso _ mincer****
cap drop templeo
gen templeo=ocupado/pea
cap drop agesq
gen agesq=edad*edad

cap drop married
gen married=(civil==2)
cap drop single
gen single=(civil==1) 

*Hijos por edades
cap drop kids6
gen kids6=(parentesco==3 & edad<=6) 
egen kids6a=total(kids6), by (folio)

gen kids6b=(kids6a>0)

cap drop kids714
gen kids714=(parentesco==3 & edad>=7 & edad<=14) 
egen kids714a=total(kids714), by (folio)

gen kids714d=(kids714a>0)

probit pea edad agesq married single kids6b kids714d if sexo==2
predict xb if e(sample), xb
generate aux= normalden(-xb)/(1-normal(-xb))
replace aux=0 if sexo==1 

*Experiencia 
cap drop exp
gen exp=edad-escol-6
replace exp=0 if exp<0

cap drop expsq
gen expsq=exp*exp


*Log ingreso
cap drop lnw
gen lnw=ln(yprilab)

*MINCER

reg lnw exp expsq escol 
reg lnw exp expsq escol aux
heckman lnw exp expsq escol, select(married single kids6b kids714d) twostep

reg lnw exp expsq escol eth
reg lnw exp expsq escol eth aux
heckman lnw exp expsq escol eth , select(married single  kids6b kids714d) twostep 




