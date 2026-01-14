cd "/Users/gabrielsaco/Desktop/INEI 2024" 

** Módulo 100

import spss using "/Users/gabrielsaco/Desktop/INEI 2024/965-Modulo1856/CAP_100E_URBANO_RURAL_3.sav", clear

rename *, lower

tostring hogar per, replace
replace conglomerado="00"+conglomerado if strlen(conglomerado)==5 	
egen idh=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per mes)

drop factor

save 2024BD1, replace

** Módulo 200 

import spss using "/Users/gabrielsaco/Desktop/INEI 2024/965-Modulo1857/CAP_200_URBANO_RURAL_4.sav", clear

rename *, lower
gen p201_str = string(p201, "%02.0f")
drop p201
rename p201_str p201

destring mes, replace
gen mes_str = string(mes, "%02.0f")
drop mes
rename mes_str mes

tostring hogar per, replace
replace conglomerado="00"+conglomerado if strlen(conglomerado)==5 	

egen id=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per p201 mes)
egen idh=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per mes)
drop factor

save 2024BD2, replace

** Módulo 300 

import spss using "/Users/gabrielsaco/Desktop/INEI 2024/965-Modulo1858/CAP_300_URBANO_RURAL_5.sav", clear

rename *, lower
gen p201_str = string(p201, "%02.0f")
drop p201
rename p201_str p201

destring mes, replace
gen mes_str = string(mes, "%02.0f")
drop mes
rename mes_str mes

tostring hogar per, replace
replace conglomerado="00"+conglomerado if strlen(conglomerado)==5 	

egen id=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per p201 mes)
egen idh=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per mes)
drop factor p207 

save 2024BD3, replace

** Módulo 600 

import spss using "/Users/gabrielsaco/Desktop/INEI 2024/965-Modulo1860/CAP_600_ANUAL_7.sav", clear

rename *, lower
gen p201_str = string(p201, "%02.0f")
drop p201
rename p201_str p201
tostring hogar per, replace

destring mes, replace
gen mes_str = string(mes, "%02.0f")
drop mes
rename mes_str mes_str

replace conglomerado="00"+conglomerado if strlen(conglomerado)==5 	

egen id=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per p201 mes)
egen idh=concat(ccdd ccpp ccdi conglomerado nselv vivienda hogar per mes)

* -----------------------------------------------------------------------------
* VICTIMIZATION
* -----------------------------------------------------------------------------

local all_vars p615_1 p615_2 p615_3 p615_4 p615_5 p615_6 p615_7 p615_8 p615_9 p615_10 p615_11 p615_12 p615_13 p615_14 p615_15 p615_16 p615_17 p615_18 ///
               p615_19 p615_20 p615_21 p615_22 p615_23 p615_24 p615_25 p615_26 p615_27
			   
recode `all_vars' (3 = .)
recode `all_vars' (2 = 0)


* Property Crimes
local property_vars p615_1 p615_3 p615_5 p615_7 p615_9 p615_19 p615_21
egen property = rowmax(`property_vars')


* Non-Property Crimes
local personal_vars p615_14 p615_15 p615_16 p615_17
egen personal_victim = rowmax(`personal_vars')


* Cyber Crimes
local cyber_vars p615_23 p615_24 p615_25 p615_26 p615_27
egen cyber_victim = rowmax(`cyber_vars')


* Attempted Crimes
egen attempt = rowmax(p615_2 p615_4 p615_6 p615_8 p615_10 p615_20)

* Vandalism
rename p615_13 vandalism


* -----------------------------------------------------------------------------
* PERCEPTION
* -----------------------------------------------------------------------------

local all_p601 p601_1 p601_2 p601_3 p601_4 p601_5 p601_6 p601_7 p601_8 ///
              p601_9 p601_10 p601_11 p601_12 p601_13 p601_14 p601_15 p601_16

recode `all_p601' (3 = .)
recode `all_p601' (2 = 0)

egen property_perception = rowmax(p601_1 p601_2 p601_3 p601_4 p601_5 p601_6 p601_11 p601_12)

egen personal_perception = rowmax(p601_7 p601_8 p601_9 p601_10)

drop p207 p208_a

save 2024BD4, replace

* -----------------------------------------------------------------------------
* MERGE
* ----------------------------------------------------------------------------

cd "/Users/gabrielsaco/Desktop/INEI 2024" 
use 2024BD2, clear 
drop regionnatu
merge m:1 idh using 2024BD1
drop regionnatu
drop _merge
merge 1:1 id using 2024BD3
drop _merge
merge 1:1 id idh using 2024BD4, keep(3)
save BD_2024, replace


* -----------------------------------------------------------------------------
* ADDING CRIME VARIABLES FROM CHAPTER 100
* ----------------------------------------------------------------------------

u BD_2024, clear

destring p116_1, replace
replace p116_1 = 0 if p116_1 == 2
rename p116_1 house_victim

* Property Victim (1 if property==1 OR house_victim==1)

egen property_victim = rowmax(property house_victim)

* Attempted Crime (1 if intento==1 OR p120_2==1)

replace p120_2 = 0 if p120_2 == 2
egen attempt_crime = rowmax(attempt p120_2)

* -----------------------------------------------------------------------------
* MERGE WITH EXTRA DATASETS
* ----------------------------------------------------------------------------

generate str codigo = ccdd + ccpp + ccdi

drop _merge
merge m:1 codigo using "/Users/gabrielsaco/Desktop/INEI 2024/datacrim.dta" , keepusing(valor) keep(3)
drop _merge

replace valor = subinstr(valor, " ", "", .)
gen district_crimes = real(valor)

rename codigo ubigeo

merge m:1 ubigeo using "/Users/gabrielsaco/Desktop/INEI 2024/pnud.dta", keepusing(IDH Población) keep(3)
rename IDH hdi
rename Población poblacion

* -----------------------------------------------------------------------------
* CONTROLS
* ----------------------------------------------------------------------------

rename p207 male
replace male = 0 if male == 2
rename p208_a age

rename p211 has_partner
replace has_partner = 1 if inlist(has_partner,1,3)
replace has_partner = 0 if inlist(has_partner,2,4,5,6)

generate graduate = inlist(p308, 8, 9, 10, 11, 12)
generate high_school = inlist(p308, 5, 6)
generate elementary = inlist(p308, 3, 4)

replace graduate = . if p308 == . | p308 == 7
replace high_school = . if p308 == . | p308 == 7
replace elementary = . if p308 == . | p308 == 7

rename p111 owned_home
replace owned_home = 0 if owned_home == 2


generate crimes_per_100 = (district_crimes/poblacion)*100

egen estrato = group(ccdd area)
gen province = ccdd + ccpp

* Disability

local p301_vars p301_1 p301_2 p301_3 p301_4 p301_5 p301_6 p301_7
recode `p301_vars' (2=0)
egen has_disability = rowmax(`p301_vars')

* Electricity and Internet

replace p115b=0 if p115b==2
replace p189_14=0 if p189_14==2

rename p115b electricity_interruption
rename p189_14 has_internet

rename p179_1 street_cleaning
replace street_cleaning=0 if street_cleaning==2

save BD_2024_2, replace













