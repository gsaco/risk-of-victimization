*===============================================================================
* PROPENSITY SCORE MATCHING ANALYSIS - BOTH OUTCOMES
* Property Victim and Personal Victim
* Following Caliendo & Kopeinig (2008)
* ESTIMATING BOTH ATT AND ATE WITH SAMPLE SIZES
*===============================================================================

cd "/Users/gabrielsaco/Desktop/INEI 2024" 

*===============================================================================
* PART A: PROPERTY VICTIM ANALYSIS
*===============================================================================

display ""
display "==============================================================================="
display "ANALYSIS 1: PROPERTY VICTIM"
display "==============================================================================="

*--- Setup ---
use BD_2024_2, clear
drop if property_perception == .
destring ccdd, replace
gen female = 1 - male

*--- Propensity Score Estimation ---
logit property_victim ///
    i.female c.age i.graduate i.high_school ///
    i.elementary i.has_disability i.has_partner i.has_internet ///
    c.crimes_per_100 c.hdi c.age##c.age i.female#c.age ///
    i.has_partner#i.female i.ccdd

predict ps_property, pr

*--- Common Support Analysis ---
quietly sum ps_property if property_victim == 1
local min_treat_prop = r(min)
local max_treat_prop = r(max)
quietly sum ps_property if property_victim == 0
local min_control_prop = r(min)
local max_control_prop = r(max)

local lower_bound_prop = max(`min_treat_prop', `min_control_prop')
local upper_bound_prop = min(`max_treat_prop', `max_control_prop')

gen in_cs_property = (ps_property >= `lower_bound_prop' & ps_property <= `upper_bound_prop')

display ""
display "PROPERTY VICTIM - Common Support:"
display "  Treated PS range:  [" %6.4f `min_treat_prop' ", " %6.4f `max_treat_prop' "]"
display "  Control PS range:  [" %6.4f `min_control_prop' ", " %6.4f `max_control_prop' "]"
display "  Common Support:    [" %6.4f `lower_bound_prop' ", " %6.4f `upper_bound_prop' "]"

tab property_victim in_cs_property, row col

*--- Pre-matching Balance ---
pstest female age graduate high_school ///
    elementary has_disability has_partner has_internet ///
    crimes_per_100 hdi, ///
    treated(property_victim) raw

*--- Matching on Common Support ---
preserve
keep if in_cs_property == 1

*--- NN 1:1 for ATE ---
psmatch2 property_victim, pscore(ps_property) neighbor(1) caliper(0.1) common ate
gen matched_nn1_prop = _weight != .
gen ps_weight_nn1_prop_att = _weight if property_victim == 1
gen ps_weight_nn1_prop_ate = _weight
replace ps_weight_nn1_prop_att = 0 if ps_weight_nn1_prop_att == .
replace ps_weight_nn1_prop_ate = 0 if ps_weight_nn1_prop_ate == .

*--- NN 1:3 for ATE ---
psmatch2 property_victim, pscore(ps_property) neighbor(3) caliper(0.1) common ate
gen matched_nn3_prop = _weight != .
gen ps_weight_nn3_prop_att = _weight if property_victim == 1
gen ps_weight_nn3_prop_ate = _weight
replace ps_weight_nn3_prop_att = 0 if ps_weight_nn3_prop_att == .
replace ps_weight_nn3_prop_ate = 0 if ps_weight_nn3_prop_ate == .

*--- NN 1:5 for ATE ---
psmatch2 property_victim, pscore(ps_property) neighbor(5) caliper(0.1) common ate
gen matched_nn5_prop = _weight != .
gen ps_weight_nn5_prop_att = _weight if property_victim == 1
gen ps_weight_nn5_prop_ate = _weight
replace ps_weight_nn5_prop_att = 0 if ps_weight_nn5_prop_att == .
replace ps_weight_nn5_prop_ate = 0 if ps_weight_nn5_prop_ate == .

*--- Radius for ATE ---
psmatch2 property_victim, pscore(ps_property) radius caliper(0.05) common ate
gen matched_radius_prop = _weight != .
gen ps_weight_radius_prop_att = _weight if property_victim == 1
gen ps_weight_radius_prop_ate = _weight
replace ps_weight_radius_prop_att = 0 if ps_weight_radius_prop_att == .
replace ps_weight_radius_prop_ate = 0 if ps_weight_radius_prop_ate == .

*--- Kernel for ATE ---
psmatch2 property_victim, pscore(ps_property) kernel kerneltype(epan) bwidth(0.06) common ate
gen matched_kernel_prop = _weight != .
gen ps_weight_kernel_prop_att = _weight if property_victim == 1
gen ps_weight_kernel_prop_ate = _weight
replace ps_weight_kernel_prop_att = 0 if ps_weight_kernel_prop_att == .
replace ps_weight_kernel_prop_ate = 0 if ps_weight_kernel_prop_ate == .

*--- Post-matching Balance ---
display ""
display "POST-MATCHING BALANCE - NN 1:1"
pstest female age graduate high_school ///
    elementary has_disability has_partner has_internet ///
    crimes_per_100 hdi, ///
    treated(property_victim) mweight(ps_weight_nn1_prop_ate) both

*--- ATT and ATE Estimation with Sample Sizes ---
matrix results_prop_att = J(5,3,.)
matrix results_prop_ate = J(5,3,.)
matrix sample_sizes_prop = J(5,3,.)
matrix rownames results_prop_att = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix rownames results_prop_ate = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix rownames sample_sizes_prop = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix colnames results_prop_att = "ATT" "SE" "P_value"
matrix colnames results_prop_ate = "ATE" "SE" "P_value"
matrix colnames sample_sizes_prop = "N_Total" "N_Treated" "N_Control"

* NN 1:1 - ATT
reg property_perception property_victim [pweight = ps_weight_nn1_prop_att]
matrix results_prop_att[1,1] = _b[property_victim]
matrix results_prop_att[1,2] = _se[property_victim]
matrix results_prop_att[1,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:1 - ATE
reg property_perception property_victim [pweight = ps_weight_nn1_prop_ate]
matrix results_prop_ate[1,1] = _b[property_victim]
matrix results_prop_ate[1,2] = _se[property_victim]
matrix results_prop_ate[1,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:1 - Sample sizes
count if matched_nn1_prop == 1
matrix sample_sizes_prop[1,1] = r(N)
count if matched_nn1_prop == 1 & property_victim == 1
matrix sample_sizes_prop[1,2] = r(N)
count if matched_nn1_prop == 1 & property_victim == 0
matrix sample_sizes_prop[1,3] = r(N)

* NN 1:3 - ATT
reg property_perception property_victim [pweight = ps_weight_nn3_prop_att]
matrix results_prop_att[2,1] = _b[property_victim]
matrix results_prop_att[2,2] = _se[property_victim]
matrix results_prop_att[2,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:3 - ATE
reg property_perception property_victim [pweight = ps_weight_nn3_prop_ate]
matrix results_prop_ate[2,1] = _b[property_victim]
matrix results_prop_ate[2,2] = _se[property_victim]
matrix results_prop_ate[2,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:3 - Sample sizes
count if matched_nn3_prop == 1
matrix sample_sizes_prop[2,1] = r(N)
count if matched_nn3_prop == 1 & property_victim == 1
matrix sample_sizes_prop[2,2] = r(N)
count if matched_nn3_prop == 1 & property_victim == 0
matrix sample_sizes_prop[2,3] = r(N)

* NN 1:5 - ATT
reg property_perception property_victim [pweight = ps_weight_nn5_prop_att]
matrix results_prop_att[3,1] = _b[property_victim]
matrix results_prop_att[3,2] = _se[property_victim]
matrix results_prop_att[3,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:5 - ATE
reg property_perception property_victim [pweight = ps_weight_nn5_prop_ate]
matrix results_prop_ate[3,1] = _b[property_victim]
matrix results_prop_ate[3,2] = _se[property_victim]
matrix results_prop_ate[3,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* NN 1:5 - Sample sizes
count if matched_nn5_prop == 1
matrix sample_sizes_prop[3,1] = r(N)
count if matched_nn5_prop == 1 & property_victim == 1
matrix sample_sizes_prop[3,2] = r(N)
count if matched_nn5_prop == 1 & property_victim == 0
matrix sample_sizes_prop[3,3] = r(N)

* Radius - ATT
reg property_perception property_victim [pweight = ps_weight_radius_prop_att]
matrix results_prop_att[4,1] = _b[property_victim]
matrix results_prop_att[4,2] = _se[property_victim]
matrix results_prop_att[4,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* Radius - ATE
reg property_perception property_victim [pweight = ps_weight_radius_prop_ate]
matrix results_prop_ate[4,1] = _b[property_victim]
matrix results_prop_ate[4,2] = _se[property_victim]
matrix results_prop_ate[4,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* Radius - Sample sizes
count if matched_radius_prop == 1
matrix sample_sizes_prop[4,1] = r(N)
count if matched_radius_prop == 1 & property_victim == 1
matrix sample_sizes_prop[4,2] = r(N)
count if matched_radius_prop == 1 & property_victim == 0
matrix sample_sizes_prop[4,3] = r(N)

* Kernel - ATT
reg property_perception property_victim [pweight = ps_weight_kernel_prop_att]
matrix results_prop_att[5,1] = _b[property_victim]
matrix results_prop_att[5,2] = _se[property_victim]
matrix results_prop_att[5,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* Kernel - ATE
reg property_perception property_victim [pweight = ps_weight_kernel_prop_ate]
matrix results_prop_ate[5,1] = _b[property_victim]
matrix results_prop_ate[5,2] = _se[property_victim]
matrix results_prop_ate[5,3] = 2*ttail(e(df_r), abs(_b[property_victim]/_se[property_victim]))

* Kernel - Sample sizes
count if matched_kernel_prop == 1
matrix sample_sizes_prop[5,1] = r(N)
count if matched_kernel_prop == 1 & property_victim == 1
matrix sample_sizes_prop[5,2] = r(N)
count if matched_kernel_prop == 1 & property_victim == 0
matrix sample_sizes_prop[5,3] = r(N)

display ""
display "PROPERTY VICTIM - ATT RESULTS:"
matrix list results_prop_att

display ""
display "PROPERTY VICTIM - ATE RESULTS:"
matrix list results_prop_ate

display ""
display "PROPERTY VICTIM - SAMPLE SIZES:"
matrix list sample_sizes_prop

* Save property results
tempfile property_matched
save `property_matched', replace

restore

*===============================================================================
* PART B: PERSONAL VICTIM ANALYSIS
*===============================================================================

display ""
display "==============================================================================="
display "ANALYSIS 2: PERSONAL VICTIM"
display "==============================================================================="

*--- Setup ---
use BD_2024_2, clear
drop if personal_perception == .
destring ccdd, replace
gen female = 1 - male

*--- Propensity Score Estimation ---
logit personal_victim ///
    i.female c.age i.graduate i.high_school ///
    i.elementary i.has_disability i.has_partner i.has_internet ///
    c.crimes_per_100 c.hdi c.age##c.age i.female#c.age ///
    i.has_partner#i.female i.ccdd

predict ps_personal, pr

*--- Common Support Analysis ---
quietly sum ps_personal if personal_victim == 1
local min_treat_pers = r(min)
local max_treat_pers = r(max)
quietly sum ps_personal if personal_victim == 0
local min_control_pers = r(min)
local max_control_pers = r(max)

local lower_bound_pers = max(`min_treat_pers', `min_control_pers')
local upper_bound_pers = min(`max_treat_pers', `max_control_pers')

gen in_cs_personal = (ps_personal >= `lower_bound_pers' & ps_personal <= `upper_bound_pers')

display ""
display "PERSONAL VICTIM - Common Support:"
display "  Treated PS range:  [" %6.4f `min_treat_pers' ", " %6.4f `max_treat_pers' "]"
display "  Control PS range:  [" %6.4f `min_control_pers' ", " %6.4f `max_control_pers' "]"
display "  Common Support:    [" %6.4f `lower_bound_pers' ", " %6.4f `upper_bound_pers' "]"

tab personal_victim in_cs_personal, row col

*--- Pre-matching Balance ---
pstest female age graduate high_school ///
    elementary has_disability has_partner has_internet ///
    crimes_per_100 hdi, ///
    treated(personal_victim) raw

*--- Matching on Common Support ---
preserve
keep if in_cs_personal == 1

*--- NN 1:1 for ATE ---
psmatch2 personal_victim, pscore(ps_personal) neighbor(1) caliper(0.1) common ate
gen matched_nn1_pers = _weight != .
gen ps_weight_nn1_pers_att = _weight if personal_victim == 1
gen ps_weight_nn1_pers_ate = _weight
replace ps_weight_nn1_pers_att = 0 if ps_weight_nn1_pers_att == .
replace ps_weight_nn1_pers_ate = 0 if ps_weight_nn1_pers_ate == .

*--- NN 1:3 for ATE ---
psmatch2 personal_victim, pscore(ps_personal) neighbor(3) caliper(0.1) common ate
gen matched_nn3_pers = _weight != .
gen ps_weight_nn3_pers_att = _weight if personal_victim == 1
gen ps_weight_nn3_pers_ate = _weight
replace ps_weight_nn3_pers_att = 0 if ps_weight_nn3_pers_att == .
replace ps_weight_nn3_pers_ate = 0 if ps_weight_nn3_pers_ate == .

*--- NN 1:5 for ATE ---
psmatch2 personal_victim, pscore(ps_personal) neighbor(5) caliper(0.1) common ate
gen matched_nn5_pers = _weight != .
gen ps_weight_nn5_pers_att = _weight if personal_victim == 1
gen ps_weight_nn5_pers_ate = _weight
replace ps_weight_nn5_pers_att = 0 if ps_weight_nn5_pers_att == .
replace ps_weight_nn5_pers_ate = 0 if ps_weight_nn5_pers_ate == .

*--- Radius for ATE ---
psmatch2 personal_victim, pscore(ps_personal) radius caliper(0.05) common ate
gen matched_radius_pers = _weight != .
gen ps_weight_radius_pers_att = _weight if personal_victim == 1
gen ps_weight_radius_pers_ate = _weight
replace ps_weight_radius_pers_att = 0 if ps_weight_radius_pers_att == .
replace ps_weight_radius_pers_ate = 0 if ps_weight_radius_pers_ate == .

*--- Kernel for ATE ---
psmatch2 personal_victim, pscore(ps_personal) kernel kerneltype(epan) bwidth(0.06) common ate
gen matched_kernel_pers = _weight != .
gen ps_weight_kernel_pers_att = _weight if personal_victim == 1
gen ps_weight_kernel_pers_ate = _weight
replace ps_weight_kernel_pers_att = 0 if ps_weight_kernel_pers_att == .
replace ps_weight_kernel_pers_ate = 0 if ps_weight_kernel_pers_ate == .

*--- Post-matching Balance ---
display ""
display "POST-MATCHING BALANCE - NN 1:1"
pstest female age graduate high_school ///
    elementary has_disability has_partner has_internet ///
    crimes_per_100 hdi, ///
    treated(personal_victim) mweight(ps_weight_nn1_pers_ate) both

*--- ATT and ATE Estimation with Sample Sizes ---
matrix results_pers_att = J(5,3,.)
matrix results_pers_ate = J(5,3,.)
matrix sample_sizes_pers = J(5,3,.)
matrix rownames results_pers_att = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix rownames results_pers_ate = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix rownames sample_sizes_pers = "NN_1to1" "NN_1to3" "NN_1to5" "Radius" "Kernel"
matrix colnames results_pers_att = "ATT" "SE" "P_value"
matrix colnames results_pers_ate = "ATE" "SE" "P_value"
matrix colnames sample_sizes_pers = "N_Total" "N_Treated" "N_Control"

* NN 1:1 - ATT
reg personal_perception personal_victim [pweight = ps_weight_nn1_pers_att]
matrix results_pers_att[1,1] = _b[personal_victim]
matrix results_pers_att[1,2] = _se[personal_victim]
matrix results_pers_att[1,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:1 - ATE
reg personal_perception personal_victim [pweight = ps_weight_nn1_pers_ate]
matrix results_pers_ate[1,1] = _b[personal_victim]
matrix results_pers_ate[1,2] = _se[personal_victim]
matrix results_pers_ate[1,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:1 - Sample sizes
count if matched_nn1_pers == 1
matrix sample_sizes_pers[1,1] = r(N)
count if matched_nn1_pers == 1 & personal_victim == 1
matrix sample_sizes_pers[1,2] = r(N)
count if matched_nn1_pers == 1 & personal_victim == 0
matrix sample_sizes_pers[1,3] = r(N)

* NN 1:3 - ATT
reg personal_perception personal_victim [pweight = ps_weight_nn3_pers_att]
matrix results_pers_att[2,1] = _b[personal_victim]
matrix results_pers_att[2,2] = _se[personal_victim]
matrix results_pers_att[2,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:3 - ATE
reg personal_perception personal_victim [pweight = ps_weight_nn3_pers_ate]
matrix results_pers_ate[2,1] = _b[personal_victim]
matrix results_pers_ate[2,2] = _se[personal_victim]
matrix results_pers_ate[2,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:3 - Sample sizes
count if matched_nn3_pers == 1
matrix sample_sizes_pers[2,1] = r(N)
count if matched_nn3_pers == 1 & personal_victim == 1
matrix sample_sizes_pers[2,2] = r(N)
count if matched_nn3_pers == 1 & personal_victim == 0
matrix sample_sizes_pers[2,3] = r(N)

* NN 1:5 - ATT
reg personal_perception personal_victim [pweight = ps_weight_nn5_pers_att]
matrix results_pers_att[3,1] = _b[personal_victim]
matrix results_pers_att[3,2] = _se[personal_victim]
matrix results_pers_att[3,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:5 - ATE
reg personal_perception personal_victim [pweight = ps_weight_nn5_pers_ate]
matrix results_pers_ate[3,1] = _b[personal_victim]
matrix results_pers_ate[3,2] = _se[personal_victim]
matrix results_pers_ate[3,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* NN 1:5 - Sample sizes
count if matched_nn5_pers == 1
matrix sample_sizes_pers[3,1] = r(N)
count if matched_nn5_pers == 1 & personal_victim == 1
matrix sample_sizes_pers[3,2] = r(N)
count if matched_nn5_pers == 1 & personal_victim == 0
matrix sample_sizes_pers[3,3] = r(N)

* Radius - ATT
reg personal_perception personal_victim [pweight = ps_weight_radius_pers_att]
matrix results_pers_att[4,1] = _b[personal_victim]
matrix results_pers_att[4,2] = _se[personal_victim]
matrix results_pers_att[4,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* Radius - ATE
reg personal_perception personal_victim [pweight = ps_weight_radius_pers_ate]
matrix results_pers_ate[4,1] = _b[personal_victim]
matrix results_pers_ate[4,2] = _se[personal_victim]
matrix results_pers_ate[4,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* Radius - Sample sizes
count if matched_radius_pers == 1
matrix sample_sizes_pers[4,1] = r(N)
count if matched_radius_pers == 1 & personal_victim == 1
matrix sample_sizes_pers[4,2] = r(N)
count if matched_radius_pers == 1 & personal_victim == 0
matrix sample_sizes_pers[4,3] = r(N)

* Kernel - ATT
reg personal_perception personal_victim [pweight = ps_weight_kernel_pers_att]
matrix results_pers_att[5,1] = _b[personal_victim]
matrix results_pers_att[5,2] = _se[personal_victim]
matrix results_pers_att[5,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* Kernel - ATE
reg personal_perception personal_victim [pweight = ps_weight_kernel_pers_ate]
matrix results_pers_ate[5,1] = _b[personal_victim]
matrix results_pers_ate[5,2] = _se[personal_victim]
matrix results_pers_ate[5,3] = 2*ttail(e(df_r), abs(_b[personal_victim]/_se[personal_victim]))

* Kernel - Sample sizes
count if matched_kernel_pers == 1
matrix sample_sizes_pers[5,1] = r(N)
count if matched_kernel_pers == 1 & personal_victim == 1
matrix sample_sizes_pers[5,2] = r(N)
count if matched_kernel_pers == 1 & personal_victim == 0
matrix sample_sizes_pers[5,3] = r(N)

display ""
display "PERSONAL VICTIM - ATT RESULTS:"
matrix list results_pers_att

display ""
display "PERSONAL VICTIM - ATE RESULTS:"
matrix list results_pers_ate

display ""
display "PERSONAL VICTIM - SAMPLE SIZES:"
matrix list sample_sizes_pers

* Save personal results
tempfile personal_matched
save `personal_matched', replace

restore

*===============================================================================
* PART C: COMBINED DENSITY PLOTS - SINGLE FIGURE
*===============================================================================

display ""
display "==============================================================================="
display "CREATING COMBINED DENSITY PLOTS"
display "==============================================================================="

*--- Load Property data for plotting ---
use BD_2024_2, clear
drop if property_perception == .
destring ccdd, replace
gen female = 1 - male

logit property_victim ///
    i.female c.age i.graduate i.high_school ///
    i.elementary i.has_disability i.has_partner i.has_internet ///
    c.crimes_per_100 c.hdi c.age##c.age i.female#c.age ///
    i.has_partner#i.female i.ccdd
predict ps_property, pr

quietly sum ps_property if property_victim == 1
local min_treat_prop = r(min)
local max_treat_prop = r(max)
quietly sum ps_property if property_victim == 0
local min_control_prop = r(min)
local max_control_prop = r(max)
local lower_bound_prop = max(`min_treat_prop', `min_control_prop')
local upper_bound_prop = min(`max_treat_prop', `max_control_prop')

*--- Property Common Support Density ---
twoway (kdensity ps_property if property_victim == 1, color(red) lwidth(medium)) ///
       (kdensity ps_property if property_victim == 0, color(blue) lwidth(medium)), ///
    xline(`lower_bound_prop' `upper_bound_prop', lpattern(dash) lcolor(black) lwidth(thin)) ///
    legend(order(1 "Treated" 2 "Control") position(6) rows(1)) ///
    xtitle("Propensity Score") ytitle("Density") ///
    subtitle("Panel (A) Property victimization — Propensity Score", size(small)) ///
    graphregion(color(white)) bgcolor(white) ///
    name(density_property, replace)

*--- Load Personal data for plotting ---
use BD_2024_2, clear
drop if personal_perception == .
destring ccdd, replace
gen female = 1 - male

logit personal_victim ///
    i.female c.age i.graduate i.high_school ///
    i.elementary i.has_disability i.has_partner i.has_internet ///
    c.crimes_per_100 c.hdi c.age##c.age i.female#c.age ///
    i.has_partner#i.female i.ccdd
predict ps_personal, pr

quietly sum ps_personal if personal_victim == 1
local min_treat_pers = r(min)
local max_treat_pers = r(max)
quietly sum ps_personal if personal_victim == 0
local min_control_pers = r(min)
local max_control_pers = r(max)
local lower_bound_pers = max(`min_treat_pers', `min_control_pers')
local upper_bound_pers = min(`max_treat_pers', `max_control_pers')

*--- Personal Common Support Density ---
twoway (kdensity ps_personal if personal_victim == 1, color(red) lwidth(medium)) ///
       (kdensity ps_personal if personal_victim == 0, color(blue) lwidth(medium)), ///
    xline(`lower_bound_pers' `upper_bound_pers', lpattern(dash) lcolor(black) lwidth(thin)) ///
    legend(order(1 "Treated" 2 "Control") position(6) rows(1)) ///
    xtitle("Propensity Score") ytitle("Density") ///
    subtitle("Panel (B) Personal victimization — Propensity Score", size(small)) ///
    graphregion(color(white)) bgcolor(white) ///
    name(density_personal, replace)

*--- COMBINED FIGURE: Property and Personal side by side ---
graph combine density_property density_personal, ///
    cols(2) rows(1) ///
    graphregion(color(white)) ///
    name(combined_density, replace)

graph export "combined_density_both_outcomes.png", replace width(1600) height(600)

*--- COMBINED FIGURE: Property and Personal stacked ---
graph combine density_property density_personal, ///
    cols(1) rows(2) ///
    graphregion(color(white)) ///
    name(combined_density_stacked, replace)

graph export "combined_density_both_outcomes_stacked.png", replace width(800) height(1200)

*===============================================================================
* PART D: FINAL SUMMARY TABLE
*===============================================================================

display ""
display "==============================================================================="
display "FINAL SUMMARY - ALL RESULTS"
display "==============================================================================="

display ""
display "========================================="
display "PROPERTY VICTIM - TREATMENT EFFECTS"
display "========================================="
display ""
display "ATT Estimates:"
matrix list results_prop_att
display ""
display "ATE Estimates:"
matrix list results_prop_ate
display ""
display "Sample Sizes (N_Total, N_Treated, N_Control):"
matrix list sample_sizes_prop

display ""
display "========================================="
display "PERSONAL VICTIM - TREATMENT EFFECTS"
display "========================================="
display ""
display "ATT Estimates:"
matrix list results_pers_att
display ""
display "ATE Estimates:"
matrix list results_pers_ate
display ""
display "Sample Sizes (N_Total, N_Treated, N_Control):"
matrix list sample_sizes_pers

display ""
display "==============================================================================="
display "Graphs exported:"
display "  1. combined_density_both_outcomes.png (side by side)"
display "  2. combined_density_both_outcomes_stacked.png (stacked)"
display "==============================================================================="

*===============================================================================
* END OF COMBINED ANALYSIS
*===============================================================================
