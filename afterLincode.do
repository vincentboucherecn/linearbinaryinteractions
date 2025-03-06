
clear
use /home/vincent/Desktop/tmpah/LPM/variables.dta
destring aid scid, replace
gen age2 = age*age/10
order aid scid fid* age age2 yr_school male black Asian hisp race_other both_par sport less_hs more_hs momedu_miss Prof job_other Welfare momjob_miss
rename gpa smoke

saveold "/home/vincent/Desktop/tmpah/LPM/cleandta.dta", replace version(12)


