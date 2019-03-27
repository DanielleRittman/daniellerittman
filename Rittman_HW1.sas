libname stat512 'M:\biostats 512';
*log transform salary;
data stat512.faculty;
set stat512.faculty;
log_sal=log(salary);
run;
*descriptive stats and histogram of new variable;
proc univariate data=stat512.faculty;
var log_sal;
histogram;
run;
*boxplot of log_salary and rank;
proc sgplot data=stat512.faculty;
vbox log_sal/ category=rank;
run;
*q4 glm for log_sal and rank;
proc glm data=stat512.faculty order=internal plots=diagnostics;
class rank (ref='1');
model log_sal=rank/ solution;
means rank / hovtest=levene welch;
lsmeans rank / pdiff tdiff adjust=bonferroni cl;
run;
quit;
*scatter plot of log sal and market;
proc sgplot data=stat512.faculty;
reg x=log_sal y=market/group=rank;
run;
*center market;
proc means data=stat512.faculty;
var market; run;
data stat512.faculty;
set stat512.faculty;
cent_market= market-0.9485214;
run;
*q7 interaction and q11 estimates;
proc glm data=stat512.faculty plots=diagnostics;
class rank (ref='1');
model log_sal= cent_market rank cent_market*rank/solution clparm;
estimate 'rank1' cent_market 1 cent_market*rank 1 0 0;
estimate 'rank2' cent_market 1 cent_market*rank 0 1 0;
estimate 'rank3' cent_market 1 cent_market*rank 0 0 1;
run;
quit;
