*q1 make log(salary);
gen logsalary=ln(salary)
summarize logsalary

*boxplot logsal and rank;
graph box logsalary, over(rank) ytitle(logsalary)

*q4 fit model of logsalry and rank;
reg logsalary ib1.rank

*4a anova;
anova logsalary ib1.rank
reg

*4b;
margins rank, asbalanced pwcompare(pveffects) mcompare(bon)

*5a graph;
twoway (scatter logsalary market if rank==1, mcolor(blue) msymbol(oh))  ///
(lfit logsalary market if rank==1, lcolor(blue)) ///
(scatter logsalary market if rank==2, mcolor(red) msymbol(plus)) ///
(lfit logsalary market if rank==2, lcolor(red)) ///(
(scatter logsalary market if rank==3, mcolor(green) msymbol(Sh)) ///
(lfit logsalary market if rank==3, lcolor(green) ytitle("Log of Salary (USD)") xtitle("Marketability of Discipline") legend(order(2 4 6) lab(2 "Assistant") lab(4 "Associate") lab(6 "Full")))

*q7 model;
reg logsalary ib1.rank cent_market ib1.rank#c.cent_market

*q11 margins;
margins rank, dydx(cent_market)

*q12 errors
rvfplot
predict rstud if e(sample), rstudent
hist rstud, normal
