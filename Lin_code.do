/*
CODE provided by the authors of L.-f. Lee, J. Li, and X. Lin. "Binary choice models with social network under heterogeneous rational expectations." Review of Economics and Statistics, 96(3):402–417, 2014.
do C:\Xu_Lin\Paper\variables.do


 */
clear
set more off
log using /home/vincent/Desktop/tmpah/LPM/variables.log, replace
set mem 900m

import sasxport5 /home/vincent/Desktop/tmpah/LPM/sfriend.xpt
sort sqid
save /home/vincent/Desktop/tmpah/LPM/sfriend_sort,replace

import sasxport5 /home/vincent/Desktop/tmpah/LPM/Inschool.xpt,clear
keep sqid aid sschlcde s1 s2 s3 s4 s6a s6b s6c s6d s6e s9 s10 s10a s10b s10c s10d s11 s12 s14 s15 s17 s18 s20 s21 s44a18 s44a19 s44a20 s44a21 s44a22  s44a23 s44a24 s44a25 s44a26 s44a27 s44a28 s44a29 s59a s59b s59c s59d s59e s59f s59g

sort sqid
merge sqid using /home/vincent/Desktop/tmpah/LPM/sfriend_sort
tab _merge
drop _merge

drop sqid

rename sschlcde scid

rename s1 age

gen sex_miss=0
replace sex_miss=1 if s2==9 | s2==.
gen male=.
replace male=0 if s2==2
replace male=1 if s2==1
drop s2


gen female=.
replace female=1 if male==0
replace female=0 if male==1

gen black=0
replace black=1 if s6b==1
gen Asian=0
replace Asian=1 if s6b~=1 & s6c==1
gen hisp=0
replace hisp=1 if s6b~=1 & s6c~=1 & s4==1
gen race_other=0
replace race_other=1 if s6b~=1 & s6c~=1 & s4~=1 & s6d==1
replace race_other=1 if s6b~=1 & s6c~=1 & s4~=1 & s6e==1
gen white=0
replace white=1 if s6b~=1 & s6c~=1 & s4~=1 & s6d~=1 & s6e~=1 & s6a==1

gen race_miss=. 
replace race_miss=1 if black==0 & Asian==0 & hisp==0 & race_other==0 & white==0

replace black=. if race_miss==1
replace Asian=. if race_miss==1
replace hisp=. if race_miss==1
replace race_other=. if race_miss==1
replace white=. if race_miss==1

drop s4 s6a s6b s6c s6d s6e

rename s9 yr_school

gen with_mom=0
replace with_mom=1 if s11==1
gen withmom_miss=0
replace withmom_miss=1 if s11==.|s11==9

gen with_dad=0
replace with_dad=1 if s17==1
gen withdad_miss=0
replace withdad_miss=1 if s17==.|s17==9

gen both_par=0
replace both_par=1 if with_mom==1 & with_dad==1
gen with_mom_only=0
replace with_mom_only=1 if with_mom==1 & with_dad==0
gen with_dad_only=0
replace with_dad_only=1 if with_mom==0 & with_dad==1
gen with_mom_miss=0
replace with_mom_miss=1 if with_mom==1 & withdad_miss==1
gen with_dad_miss=0
replace with_dad_miss=1 if withmom_miss==1 & with_dad==1
gen both_miss=0
replace both_miss=1 if withmom_miss==1 & withdad_miss==1

gen with_one_par =0
replace with_one_par =1 if with_mom_only==1 | with_dad_only==1
gen with_one_miss =0
replace with_one_miss =1 if with_mom_miss==1 | with_dad_miss==1

replace both_par=. if with_one_miss ==1 | both_miss==1

gen not_both_par=.
replace not_both_par=1 if both_par ==0
replace not_both_par=0 if both_par ==1

gen less_hs=0
replace less_hs=1 if s12==1 | s12==2 | s12==10
gen hs=0
replace hs=1 if s12==3 | s12==4
gen more_hs=0
replace more_hs=1 if s12==5 | s12==6 | s12==7 | s12==8

gen momedu_skip=0
replace momedu_skip=1 if s12==97

gen momedu_miss=0
replace momedu_miss=1 if s12==.|s12==9|s12==11|s12==99

gen Homemaker=1 if s14==1
replace Homemaker=0 if s14~=1
gen Prof1=1 if s14==2
replace Prof1=0 if s14~=2
gen Prof2=1 if s14==3
replace Prof2=0 if s14~=3
gen Manager=1 if s14==4
replace Manager=0 if s14~=4
gen Technical=1 if s14==5
replace Technical=0 if s14~=5
gen Office=1 if s14==6
replace Office=0 if s14~=6
gen Sales=1 if s14==7
replace Sales=0 if s14~=7
gen Restaurant=1 if s14==8
replace Restaurant=0 if s14~=8
gen Craft=1 if s14==9
replace Craft=0 if s14~=9
gen Construction=1 if s14==10
replace Construction=0 if s14~=10
gen Mechanic=1 if s14==11
replace Mechanic=0 if s14~=11
gen Factory=1 if s14==12
replace Factory=0 if s14~=12
gen Trans=1 if s14==13
replace Trans=0 if s14~=13
gen Military=1 if s14==14
replace Military=0 if s14~=14
gen Farm=1 if s14==15
replace Farm=0 if s14~=15
gen nowork_nodisabled=1 if s14==16
replace nowork_nodisabled=0 if s14~=16
gen nowork_disabled=1 if s14==17
replace nowork_disabled=0 if s14~=17
gen Retired=1 if s14==18
replace Retired=0 if s14~=18
gen Welfare=1 if s14==19
replace Welfare=0 if s14~=19

gen momjob_skip=0
replace momjob_skip=1 if s14==97

gen momjob_miss=0
replace momjob_miss=1 if s14==.|s14==20|s14==99

/* combine*/
gen Prof=0
replace Prof=1 if s14==2 | s14==3 |s14==4
gen Home=0
replace Home=1 if s14==1 |s14==16 |s14==17 |s14==18 
gen job_other=0
replace job_other=1 if s14==5 |s14==6 |s14==7 |s14==8 |s14==9 |s14==10 |s14==11 |s14==12 |s14==13 |s14==14 |s14==15


gen work_pay=0
replace work_pay=1 if s15==1
gen work_pay_skip=0
replace work_pay_skip=1 if s15==7

gen work_pay_miss=0
replace work_pay_miss=1 if s15==.|s15==8|s15==9

gen Homemaker_dad=1 if s20==1
replace Homemaker_dad=0 if s20~=1
gen Prof1_dad=1 if s20==2
replace Prof1_dad=0 if s20~=2
gen Prof2_dad=1 if s20==3
replace Prof2_dad=0 if s20~=3
gen Manager_dad=1 if s20==4
replace Manager_dad=0 if s20~=4 
gen Technical_dad=1 if s20==5
replace Technical_dad=0 if s20~=5
gen Office_dad=1 if s20==6
replace Office_dad=0 if s20~=6
gen Sales_dad=1 if s20==7
replace Sales_dad=0 if s20~=7
gen Restaurant_dad=1 if s20==8
replace Restaurant_dad=0 if s20~=8
gen Craft_dad=1 if s20==9
replace Craft_dad=0 if s20~=9
gen Construction_dad=1 if s20==10
replace Construction_dad=0 if s20~=10
gen Mechanic_dad=1 if s20==11
replace Mechanic_dad=0 if s20~=11
gen Factory_dad=1 if s20==12
replace Factory_dad=0 if s20~=12
gen Trans_dad=1 if s20==13
replace Trans_dad=0 if s20~=13
gen Military_dad=1 if s20==14
replace Military_dad=0 if s20~=14
gen Farm_dad=1 if s20==15
replace Farm_dad=0 if s20~=15
gen nowork_nodisabled_dad=1 if s20==16
replace nowork_nodisabled_dad=0 if s20~=16
gen nowork_disabled_dad=1 if s20==17
replace nowork_disabled_dad=0 if s20~=17
gen Retired_dad=1 if s20==18
replace Retired_dad=0 if s20~=18
gen Welfare_dad=1 if s20==19
replace Welfare_dad=0 if s20~=19

gen momjob_skip_dad=0
replace momjob_skip_dad=1 if s20==97

gen momjob_miss_dad=0
replace momjob_miss_dad=1 if s20==.|s20==20|s20==99

gen work_pay_dad=0
replace work_pay_dad=1 if s21==1
gen work_pay_skip_dad=0
replace work_pay_skip_dad=1 if s21==7

gen work_pay_miss_dad=0
replace work_pay_miss_dad=1 if s21==.|s21==8|s21==9

/* sport club*/
gen sport=0
replace sport=1   if s44a18==1 | s44a19==1 | s44a20==1 | s44a21==1 | s44a22==1 |  s44a23==1 | s44a24==1 | s44a25==1 | s44a26==1 | s44a27==1 | s44a28==1 | s44a29==1 

gen smoke=.
replace smoke=0      if s59a==0 |s59a==1
replace smoke=1    if s59a==2|s59a==3|s59a==4|s59a==5|s59a==6
drop if smoke==.
rename smoke gpa

rename s3 grade

foreach friends in mf1aid mf2aid mf3aid mf4aid mf5aid ff1aid ff2aid ff3aid ff4aid ff5aid {
replace `friends'=. if `friends'==77777777 | `friends'==88888888 | `friends'==99959995 | `friends'==99999999
}
rename mf1aid fid1
rename mf2aid fid2
rename mf3aid fid3
rename mf4aid fid4
rename mf5aid fid5
rename ff1aid fid6
rename ff2aid fid7
rename ff3aid fid8
rename ff4aid fid9
rename ff5aid fid10

drop s10 s10a s10b s10c s10d s11 s12 s14 s15 s17 s20 s21 s44a18 s44a19 s44a20 s44a21 s44a22  s44a23 s44a24 s44a25 s44a26 s44a27 s44a28 s44a29

sort aid
save /home/vincent/Desktop/tmpah/LPM/variables,replace

replace age=. if age==99
replace grade=. if grade==13|grade==99
replace yr_school=. if yr_school == 99

sum gpa age white black hisp less_hs hs Prof Home 

drop if aid==""
sum gpa age white black hisp less_hs hs Prof Home sport
drop if age==.
sum gpa age white black hisp less_hs hs Prof Home sport
drop if grade==.
sum gpa age white black hisp less_hs hs Prof Home sport
drop if sex_miss==1
sum gpa age white black hisp less_hs hs Prof Home sport
drop if race_miss==1
sum gpa age white black hisp less_hs hs Prof Home sport
drop if  yr_school == .
sum gpa age white black hisp less_hs hs Prof Home sport
drop if with_one_miss ==1 | both_miss==1
sum gpa age white black hisp less_hs hs Prof Home sport
drop if gpa==.
sum gpa age white black hisp less_hs hs Prof Home sport

/*variables*/

save /home/vincent/Desktop/tmpah/LPM/variables,replace

destring , replace
sort aid
save /home/vincent/Desktop/tmpah/LPM/variables_1,replace

/***************************************************************** delete those schools with less than 100 students , so that we can use school dummies.*/

use /home/vincent/Desktop/tmpah/LPM/variables_1, clear
keep aid scid
gen schsize=1
collapse (sum) schsize, by(scid)
keep scid schsize
list
sum schsize
tab schsize

sort scid
save /home/vincent/Desktop/tmpah/LPM/schG10,replace


drop if schsize<100
sum schsize
tab schsize

use /home/vincent/Desktop/tmpah/LPM/variables_1, clear
sort scid
merge scid using /home/vincent/Desktop/tmpah/LPM/schG10
tab _merge

drop _merge

/***************************************************************************/
list scid schsize if schsize<100
drop if schsize<100

sort scid
tab scid  
sort aid

sum schsize

keep aid scid grade age yr_school male black Asian hisp race_other both_par less_hs more_hs momedu_miss Welfare momjob_miss Prof job_other sport gpa fid*

save /home/vincent/Desktop/tmpah/LPM/variables_1,replace
sum
tostring aid scid, replace
sort aid
save /home/vincent/Desktop/tmpah/LPM/variables,replace

set more on

log close
clear
