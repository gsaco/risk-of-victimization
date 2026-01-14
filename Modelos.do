* Models

cd "/Users/gabrielsaco/Desktop/INEI 2024" 

u BD_2024_2 , clear

drop if property_perception == .

* Calcula medias ponderadas para cada par de variables

mean p601_1 [pw=factor_cap600b]
mean house_victim [pw=factor_cap600b]

mean p601_2 [pw=factor_cap600b]
mean p615_1 [pw=factor_cap600b]

mean p601_3 [pw=factor_cap600b]
mean p615_3 [pw=factor_cap600b]

mean p601_4 [pw=factor_cap600b]
mean p615_5 [pw=factor_cap600b]

mean p601_5 [pw=factor_cap600b]
mean p615_7 [pw=factor_cap600b]

mean p601_6 [pw=factor_cap600b]
mean p615_9 [pw=factor_cap600b]

mean p601_13 [pw=factor_cap600b]
mean p615_11 [pw=factor_cap600b]

mean p601_7 [pw=factor_cap600b]
mean p615_14 [pw=factor_cap600b]

mean p601_8 [pw=factor_cap600b]
mean p615_15 [pw=factor_cap600b]

mean p601_9 [pw=factor_cap600b]
mean p615_16 [pw=factor_cap600b]

mean p601_10 [pw=factor_cap600b]
mean p615_17 [pw=factor_cap600b]

mean p601_11 [pw=factor_cap600b]
mean p615_19 [pw=factor_cap600b]

mean p601_12 [pw=factor_cap600b]
mean p615_21 [pw=factor_cap600b]

label values has_partner
label values male
label values vandalism
label values street_cleaning
label values has_internet

destring ccdd, replace
gen female = 1 - male
gen no_street_cleaning = 1 - street_cleaning

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: mean property_perception personal_perception ///
    property_victim personal_victim cyber_victim attempt_crime vandalism ///
    age female has_partner ///
    graduate high_school elementary ///
    has_disability has_internet ///
    crimes_per_100 hdi

local cont age crimes_per_100 hdi
local bin  property_victim personal_victim cyber_victim attempt_crime vandalism ///
           has_partner graduate high_school elementary has_disability has_internet
local percep property_perception personal_perception
	
* female = 1
svy, subpop(if female==1): mean `percep' `cont' `bin'

* female = 0
svy, subpop(if female==0): mean `percep' `cont' `bin'
	
local cont age crimes_per_100 hdi
local bin  female cyber_victim attempt_crime vandalism ///
           has_partner graduate high_school elementary has_disability has_internet
local percep property_perception personal_perception

* prop = 1
svy, subpop(if property_victim==1): mean `percep' `cont' `bin'

* prop = 0
svy, subpop(if property_victim==0): mean `percep' `cont' `bin'

* persoanl = 1
svy, subpop(if personal_victim==1): mean `percep' `cont' `bin'

* personal = 0
svy, subpop(if personal_victim==0): mean `percep' `cont' `bin'
	
	
* Probit models

*******************************************************
* PROPERTY
*******************************************************

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    i.female#i.has_partner   ///
    i.female#c.age i.female#i.property_victim i.female#i.personal_victim i.property_victim#i.personal_victim  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd 

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post
		
* H0: coeficiente de c.age#c.age = 0
test c.age#c.age
* H0: todos los coeficientes de la interacción female#partner = 0
testparm i.female#i.has_partner

* H0: todos los coeficientes de la interacción female#age = 0
testparm i.female#c.age

* H0: interacción female × property_victim = 0
testparm i.female#i.property_victim

* H0: interacción female × personal_victim = 0
testparm i.female#i.personal_victim

* Submuestra mujeres
svy, subpop(if female==1): logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post


* Submuestra hombres
svy, subpop(if female==0): logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post
		
	
/*******************************************************
* 1. QUADRATIC TERM: AGE + AGE²
*******************************************************/

margins, at(age=(20(2)80)) dydx(age)  post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(black) lwidth(medium) lpattern(solid)) ///
    scheme(s1mono) ///
    yline(0, lpattern(solid) lcolor(gs8) lwidth(thin)) /// ← zero reference line
    xlabel(20(4)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%5.3f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("AMEs on Pr(Perceived likelihood of property vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure1a_age_ame, replace)

* For predictions
margins, at(age=(20(2)80)) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(black) lwidth(medium) lpattern(solid)) ///
    scheme(s1mono) ///
    xlabel(20(4)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%3.2f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("Pr(Perceived likelihood of property vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure1b_age_probs, replace)

/*******************************************************
* 2. INTERACTION: FEMALE × AGE
*******************************************************/

margins female, at(age=(20(2)80)) dydx(age) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(blue) lwidth(medium) lpattern(solid)) ///
    plot2opts(lcolor(red) lwidth(medium) lpattern(solid)) ///
    ci1opts(lcolor(blue) lwidth(medium)) ///
    ci2opts(lcolor(red) lwidth(medium)) ///
    ciopts(recast(rcap)) ///
    addplot((rcap _ci_lb _ci_ub _at if _by==0 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(blue) lwidth(medium)) ///
            (rcap _ci_lb _ci_ub _at if _by==1 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(red) lwidth(medium))) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           cols(1) size(*0.8) region(lcolor(gs8) fcolor(gs14))) ///
    scheme(s1color) ///
    xlabel(20(5)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%5.3f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("AMEs on Pr(Perceived likelihood of property vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure2a_age_sex_ame, replace)

margins female, at(age=(20(2)80)) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(blue) lwidth(medium) lpattern(solid)) ///
    plot2opts(lcolor(red) lwidth(medium) lpattern(solid)) ///
    ci1opts(lcolor(blue) lwidth(medium)) ///
    ci2opts(lcolor(red) lwidth(medium)) ///
    ciopts(recast(rcap)) ///
    addplot((rcap _ci_lb _ci_ub _at if _by==0 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(blue) lwidth(medium)) ///
            (rcap _ci_lb _ci_ub _at if _by==1 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(red) lwidth(medium))) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           cols(1) size(*0.8) region(lcolor(gs8) fcolor(gs14))) ///
    scheme(s1color) ///
    xlabel(20(5)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%3.2f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("Pr(Perceived likelihood of property vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure2b_age_sex_probs, replace)

/*******************************************************
* 1. FEMALE × HAS_PARTNER INTERACTION
*******************************************************/

margins female#has_partner, post   

marginsplot , ///
        xdimension(has_partner) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(gs8) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 0.94)) ///
        ylabel(0.78(0.04)0.94, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No partner" 1 "Has partner", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of property vicitmization)", size(medium)) ///
        title("")  ///
        xtitle("")  ///
        name(property_probs_final, replace)

mplotoffset , ///
    xdimension(has_partner) ///
    plotdimension(female)  offset(0.05)   ///
    recast(scatter) ///
    plot1opts(msymbol(O)  mcolor(blue)) ///
    plot2opts(msymbol(O)  mcolor(red))  ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No partner" 1 "Has partner", labsize(medium)) ///
	xscale(range(-0.5 1.5)) ///
    yscale(range(0.78 0.94)) ///
    ylabel(0.78(0.04)0.94, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of property vicitmization)") ///
    title("")  xtitle("") ///
    name(property_probs_final, replace)

margins, dydx(has_partner) over(female)

marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of having a partner on Pr(Perceived likelihood of property victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.04(0.02)0.10, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)
	
/*******************************************************
* 2. FEMALE × PROPERTY_VICTIM INTERACTION
*******************************************************/

margins female#property_victim, post

marginsplot , ///
        xdimension(property_victim) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(gs8) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 0.94)) ///
        ylabel(0.78(0.04)0.94, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No property victim" 1 "Property victim", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of property vicitmization)", size(medium)) ///
        title("")  ///
        xtitle("")  ///
        name(property_probs_final, replace)

mplotoffset , ///
    xdimension(property_victim) ///
    plotdimension(female)  offset(0.05)   /// 
    recast(scatter) ///
    plot1opts(msymbol(O)  mcolor(blue)) ///
    plot2opts(msymbol(O)  mcolor(red))  ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No property victim" 1 "Property victim") ///
	xscale(range(-0.5 1.5)) ///
    yscale(range(0.78 0.94)) ///
    ylabel(0.78(0.04)0.94, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of property vicitmization)") ///
    title("")  xtitle("") ///
    name(property_probs_final, replace)
	
margins, dydx(property_victim) over(female)
marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of property victimization on Pr(Perceived likelihood of property victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.04(0.02)0.18, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)

/*******************************************************
* 3. FEMALE × PERSONAL_VICTIM INTERACTION
*******************************************************/

margins female#personal_victim, post

marginsplot , ///
        xdimension(personal_victim) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(none) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 1)) ///
        ylabel(0.78(0.04)1, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No personal victim" 1 "Personal victim", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of property vicitmization)", size(medium)) ///
        title("")  ///
        xtitle("")  ///
        name(property_probs_final, replace)

mplotoffset , ///
    xdimension(personal_victim) ///
    plotdimension(female)  offset(0.05)   /// 
    recast(scatter) ///
    plot1opts(msymbol(O)  mcolor(blue)) ///
    plot2opts(msymbol(O)  mcolor(red))  ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No personal victim" 1 "Personal victim") ///
	xscale(range(-0.5 1.5)) ///
    yscale(range(0.78 1)) ///
    ylabel(0.78(0.04)1, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of property vicitmization)") ///
    title("")  xtitle("") ///
    name(property_probs_final, replace)
	
* AMEs variation
margins, dydx(personal_victim) over(female)
marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of personal victimization over Pr(Perceived likelihood of property victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.04(0.02)0.20, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)

	
/*********************************************************************
* Average Marginal Effects 
*********************************************************************/

margins, dydx( ///
    property_victim personal_victim ///
    attempt_crime cyber_victim vandalism ///
    has_internet has_disability ///
    female has_partner ///
    graduate high_school elementary ///
) post

local L1  "Property victim"
local L2  "Personal victim"
local L3  "Attempted property victim"
local L4  "Cyber victim"
local L5  "Vandalism victim/witness"
local L6  "Internet access"
local L7  "Has a disability"
local L8  "Female"
local L9  "Has partner"
local L10 "Graduate degree"
local L11 "High school"
local L12 "Elementary"

marginsplot, horizontal ///
    recast(scatter) ///
    plot1opts(msymbol(O) msize(small) mcolor(black)) ///
    ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
    xline(0, lpattern(dash) lcolor(gs10) lwidth(thin)) ///
    scheme(s1mono) ///
    xlabel(-.15(.05).20, format(%5.2f) labsize(*0.8) ///
           tlength(*0.5) grid gmin gmax gstyle(dot)) ///
    ylabel(12 "`L12'" 11 "`L11'" 10 "`L10'" 9 "`L9'" 8 "`L8'" ///
           7 "`L7'" 6 "`L6'" 5 "`L5'" 4 "`L4'" 3 "`L3'" ///
           2 "`L2'" 1 "`L1'", ///
           angle(0) labsize(*0.7) noticks labgap(*5) ///
           tlength(0)) ///
    ytitle("") ///
    xtitle("", ///
           size(*0.8) margin(t+5)) ///
    title("") ///
    subtitle("") ///
    note("") ///
    plotregion(margin(l+12 r+8 t+4 b+4) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(16) ysize(10)
	
* OTROS MODELOS
*********************************************************************

* SIN EF

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    i.female#i.has_partner   ///
    i.female#c.age i.female#i.property_victim i.female#i.personal_victim  ///
    c.crimes_per_100 c.hdi

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post

* SIN INTERACCIONES

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post

* SIN CONTEXTUALES

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit property_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet) post

* SIN VANDALISMO, ATTEMPT, CYBERCRIME

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit property_perception ///
    i.property_victim i.personal_victim  ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary

margins, dydx(i.property_victim i.personal_victim  ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet) post

* PERSONAL
*********************************************************************

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    i.female#i.has_partner   ///
    i.female#c.age i.female#i.property_victim i.female#i.personal_victim  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd 

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post

		
* H0: coeficiente de c.age#c.age = 0
test c.age#c.age
* H0: todos los coeficientes de la interacción female#partner = 0
testparm i.female#i.has_partner

* H0: todos los coeficientes de la interacción female#age = 0
testparm i.female#c.age

* H0: interacción female × property_victim = 0
testparm i.female#i.property_victim

* H0: interacción female × personal_victim = 0
testparm i.female#i.personal_victim



* Submuestra mujeres
svy, subpop(if female==1): logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post


* Submuestra hombres
svy, subpop(if female==0): logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi ///
    i.ccdd

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post
		
/*******************************************************
* 1. QUADRATIC TERM: AGE + AGE²
*******************************************************/

margins, at(age=(20(2)80)) dydx(age)  post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(black) lwidth(medium) lpattern(solid)) ///
    scheme(s1mono) ///
    yline(0, lpattern(solid) lcolor(gs8) lwidth(thin)) /// ← zero reference line
    xlabel(20(4)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%5.3f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("AMEs on Pr(Perceived likelihood of property vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure1a_age_ame, replace)

* For predictions
margins, at(age=(20(2)80)) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(black) lwidth(medium) lpattern(solid)) ///
    scheme(s1mono) ///
    xlabel(20(4)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%3.2f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("Pr(Perceived likelihood of personal vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure1b_age_probs, replace)

/*******************************************************
* 2. INTERACTION: FEMALE × AGE
*******************************************************/

margins female, at(age=(20(2)80)) dydx(age) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(blue) lwidth(medium) lpattern(solid)) ///
    plot2opts(lcolor(red) lwidth(medium) lpattern(solid)) ///
    ci1opts(lcolor(blue) lwidth(medium)) ///
    ci2opts(lcolor(red) lwidth(medium)) ///
    ciopts(recast(rcap)) ///
    addplot((rcap _ci_lb _ci_ub _at if _by==0 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(blue) lwidth(medium)) ///
            (rcap _ci_lb _ci_ub _at if _by==1 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(red) lwidth(medium))) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           cols(1) size(*0.8) region(lcolor(gs8) fcolor(gs14))) ///
    scheme(s1color) ///
    xlabel(20(5)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%5.3f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("AMEs on Pr(Perceived likelihood of personal vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure2a_age_sex_ame, replace)

margins female, at(age=(20(2)80)) post

marginsplot, ///
    recast(line) ///
    plot1opts(lcolor(blue) lwidth(medium) lpattern(solid)) ///
    plot2opts(lcolor(red) lwidth(medium) lpattern(solid)) ///
    ci1opts(lcolor(blue) lwidth(medium)) ///
    ci2opts(lcolor(red) lwidth(medium)) ///
    ciopts(recast(rcap)) ///
    addplot((rcap _ci_lb _ci_ub _at if _by==0 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(blue) lwidth(medium)) ///
            (rcap _ci_lb _ci_ub _at if _by==1 & inlist(_at,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80), lcolor(red) lwidth(medium))) ///
    legend(order(1 "Men" 2 "Women") position(2) ring(0) ///
           cols(1) size(*0.8) region(lcolor(gs8) fcolor(gs14))) ///
    scheme(s1color) ///
    xlabel(20(5)80, labsize(*0.8) grid gstyle(dot)) ///
    xmtick(20(2)80) ///
    ylabel(, format(%3.2f) labsize(*0.8) grid gstyle(dot)) ///
    ytitle("Pr(Perceived likelihood of personal vicitmization)", size(*0.8) margin(r+3)) ///
    xtitle("Age (Years)", size(*0.8) margin(t+3)) ///
    title("") subtitle("") note("") ///
    plotregion(margin(l+5 r+5 t+3 b+3) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(12) ysize(8) ///
    name(figure2b_age_sex_probs, replace)

/*******************************************************
* 1. FEMALE × HAS_PARTNER INTERACTION
*******************************************************/

* Get predicted probabilities for all groups
margins female#has_partner, post   

* Plot predicted probabilities
marginsplot , ///
        xdimension(has_partner) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(gs8) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 0.94)) ///
        ylabel(0.78(0.04)0.94, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No partner" 1 "Has partner", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of personal victimization)", size(medium)) ///
        title("")  ///
        xtitle("")  ///
        name(haspartner_probs, replace)

* Offset version
mplotoffset , ///
    xdimension(has_partner) ///
    plotdimension(female) offset(0.05) ///
    recast(scatter) ///
    plot1opts(msymbol(O) mcolor(blue)) ///
    plot2opts(msymbol(O) mcolor(red)) ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No partner" 1 "Has partner") ///
    xscale(range(-0.5 1.5)) ///
    yscale(range(0.35 0.6)) ///
    ylabel(0.4(0.04)0.6, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of personal victimization)") ///
    title("") xtitle("") ///
    name(property_offset, replace)

* AMEs variation
margins, dydx(has_partner) over(female)
marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of having a partner over Pr(Perceived likelihood of personal victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.04(0.02)0.10, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)
	
/*******************************************************
* 2. FEMALE × PROPERTY_VICTIM INTERACTION
*******************************************************/

margins female#property_victim, post

marginsplot , ///
        xdimension(property_victim) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(gs8) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 0.94)) ///
        ylabel(0.78(0.04)0.94, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No property victim" 1 "Property victim", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of personal victimization)", size(medium)) ///
        title("") xtitle("") ///
        name(property_probs, replace)

mplotoffset , ///
    xdimension(property_victim) ///
    plotdimension(female) offset(0.05) ///
    recast(scatter) ///
    plot1opts(msymbol(O) mcolor(blue)) ///
    plot2opts(msymbol(O) mcolor(red)) ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No property victim" 1 "Property victim") ///
    xscale(range(-0.5 1.5)) ///
    yscale(range(0.35 0.6)) ///
    ylabel(0.4(0.04)0.6, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of personal victimization)") ///
    title("") xtitle("") ///
    name(property_offset, replace)

margins, dydx(property_victim) over(female)
marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of property victimization over Pr(Perceived likelihood of personal victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.04(0.02)0.2, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)


/*******************************************************
* 3. FEMALE × PERSONAL_VICTIM INTERACTION
*******************************************************/

margins female#personal_victim, post

marginsplot , ///
        xdimension(personal_victim) ///
        plotdimension(female) ///
        recast(scatter) ///
        plot1opts(msymbol(O) mcolor(blue)) ///
        plot2opts(msymbol(O) mcolor(red)) ///
        ci1opts(recast(rcap) lwidth(medthick) lcolor(blue)) ///
        ci2opts(recast(rcap) lwidth(medthick) lcolor(red)) ///
        legend(order(1 "Men" 2 "Women") position(11) ring(0) size(medium) ///
               region(lcolor(gs8) fcolor(gs14))) ///
        xscale(range(-0.5 1.5)) ///
        yscale(range(0.78 1)) ///
        ylabel(0.78(0.04)1, format(%4.2f) labsize(medium) grid) ///
        xlabel(0 "No personal victim" 1 "Personal victim", labsize(medium)) ///
        ytitle("Pr(Perceived likelihood of personal victimization)", size(medium)) ///
        title("") xtitle("") ///
        name(personal_probs, replace)

mplotoffset , ///
    xdimension(personal_victim) ///
    plotdimension(female) offset(0.05) ///
    recast(scatter) ///
    plot1opts(msymbol(O) mcolor(blue)) ///
    plot2opts(msymbol(O) mcolor(red)) ///
    ci1opts(recast(rcap) lcolor(blue) lwidth(medthick)) ///
    ci2opts(recast(rcap) lcolor(red)  lwidth(medthick)) ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) ///
           region(lcolor(gs8) fcolor(gs14))) ///
    xlabel(0 "No personal victim" 1 "Personal victim") ///
    xscale(range(-0.5 1.5)) ///
    yscale(range(0.34 1)) ///
    ylabel(0.30(0.08)0.94, format(%4.2f) grid) ///
    ytitle("Pr(Perceived likelihood of personal victimization)") ///
    title("") xtitle("") ///
    name(personal_offset, replace)

margins, dydx(personal_victim) over(female)
marginsplot, ///
    recast(scatter) ///
    plotdimension(female) ///
    plot1opts(msymbol(O) msize(medium) mcolor(blue))        /// Men
    plot2opts(msymbol(O) msize(medium) mcolor(red))         /// Women
    ci1opts(recast(rcap) lwidth(medthick) lcolor(blue))     /// Men CI
    ci2opts(recast(rcap) lwidth(medthick) lcolor(red))      /// Women CI
    yline(0, lpattern(dash) lcolor(gs8))                   /// zero reference
    xlabel(1 "AMEs of personal victimization over Pr(Perceived likelihood of personal victimization)", labsize(medium))    /// one central tick
    xscale(range(0.5 1.5))                                 /// keep points visible
    xtitle("")                                              ///
    ylabel(-0.02(0.04)0.48, format(%4.2f) labsize(medium) grid) ///
    ytitle("") ///
	title("") ///
    legend(order(1 "Men" 2 "Women") position(11) ring(0) region(lcolor(gs8) fcolor(gs14)))  /// explain colours
    name(ame_by_sex, replace)
	
	
/*********************************************************************
* Average Marginal Effects 
*********************************************************************/

margins, dydx( ///
    property_victim personal_victim ///
    attempt_crime cyber_victim vandalism ///
    has_internet has_disability ///
    female has_partner ///
    graduate high_school elementary ///
) post

local L1  "Property victim"
local L2  "Personal victim"
local L3  "Attempted property victim"
local L4  "Cyber victim"
local L5  "Vandalism victim/witness"
local L6  "Internet access"
local L7  "Has a disability"
local L8  "Female"
local L9  "Has partner"
local L10 "Graduate degree"
local L11 "High school"
local L12 "Elementary"

marginsplot, horizontal ///
    recast(scatter) ///
    plot1opts(msymbol(O) msize(small) mcolor(black)) ///
    ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
    xline(0, lpattern(dash) lcolor(gs10) lwidth(thin)) ///
    scheme(s1mono) ///
    xlabel(-.15(.05).20, format(%5.2f) labsize(*0.8) ///
           tlength(*0.5) grid gmin gmax gstyle(dot)) ///
    ylabel(12 "`L12'" 11 "`L11'" 10 "`L10'" 9 "`L9'" 8 "`L8'" ///
           7 "`L7'" 6 "`L6'" 5 "`L5'" 4 "`L4'" 3 "`L3'" ///
           2 "`L2'" 1 "`L1'", ///
           angle(0) labsize(*0.7) noticks labgap(*5) ///
           tlength(0)) ///
    ytitle("") ///
    xtitle("", ///
           size(*0.8) margin(t+5)) ///
    title("") ///
    subtitle("") ///
    note("") ///
    plotregion(margin(l+12 r+8 t+4 b+4) lcolor(none)) ///
    graphregion(color(white) lcolor(white) margin(l+2 r+2 t+2 b+2)) ///
    xsize(16) ysize(10)
	
* OTROS MODELOS
*********************************************************************

* SIN EF

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age c.age#c.age   ///
    i.graduate i.high_school i.elementary  ///
    i.female#i.has_partner   ///
    i.female#c.age i.female#i.property_victim i.female#i.personal_victim  ///
    c.crimes_per_100 c.hdi

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post

* SIN INTERACCIONES

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary  ///
    c.crimes_per_100 c.hdi

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet ///
        crimes_per_100 hdi) post

* SIN CONTEXTUALES

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit personal_perception ///
    i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary

margins, dydx(i.property_victim i.personal_victim i.cyber_victim i.attempt_crime i.vandalism ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet) post

* SIN VANDALISMO, ATTEMPT, CYBERCRIME

svyset conglomerado [pweight = factor_cap600b], strata(estrato)

svy: logit personal_perception ///
    i.property_victim i.personal_victim  ///
    i.has_internet i.has_disability ///
    i.female i.has_partner ///
    c.age   ///
    i.graduate i.high_school i.elementary

margins, dydx(i.property_victim i.personal_victim  ///
        c.age i.female i.has_partner ///
        i.graduate i.high_school i.elementary ///
        i.has_disability i.has_internet) post
