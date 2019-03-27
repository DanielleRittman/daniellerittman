*q1;
summarize
*q2;
graph box attain, over(male), xtitle('sex')
graph box attain, over(momed)
graph box attain, over(dadunemp)
graph box attain, over(dadocc)
*mean attain by neighborhood
egen mn_attain=mean(attain), by(neighid)
sort neighid
merge 1:m neighid using "M:\biostats 512\attain_kid.dta"
save "M:\biostats 512\attain_full.dta"
scatter mn_attain deprive
*null model
mixed attain || neighid:, reml variance
*model 2
mixed attain i.male i.momed i.dadunemp dadocc || neighid:, reml variance
*q6
mixed attain i.male i.momed i.dadunemp dadocc deprive || neighid:, reml variance
*q7
mixed attain i.male i.momed i.dadunemp c.dadocc##c.deprive || neighid:, reml variance cov(ind)
*q8
bysort neighid: gen kid=_n
predict eblup*, reffects
histogram eblup1 if kid==1
qnorm eblup1 if kid==1

*diagnostics
predict resid, rstandard
predict fitted, fitted
qnorm resid
scatter resid fitted
