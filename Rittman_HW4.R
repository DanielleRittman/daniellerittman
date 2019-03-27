install.packages("gam")
install.packages("mgcv")
install.packages("geepack")
install.packages("epiDisplay")
library(epiDisplay)
library(gam)
library(sas7bdat)
library(foreign)
library(splines)
library(mgcv)
library(geepack)
as<- read.sas7bdat('as.sas7bdat')
hw4<- na.omit(as[,c('id', 'age', 'gender', 'raceeth', 'ucr', 'bmi', 'educat', 'SMK', 'scot', 'htnmed', 'sthg', 'bAs', 'tAs', 'glucfast', 'dmmed')])
hw4$t2dm<-ifelse(hw4$glucfast>=126|hw4$dmmed==1, 1, 0)

##crude LOGISTIC
crudel<-glm(hw4$t2dm~log(hw4$tAs), family=binomial, na.action=na.omit)
summary(crudel)
beta.tAs <- summary(crudel)$coef[2,1]
se.tAs <- summary(crudel)$coef[2,2]
or.crudel <- exp(beta.tAs*log(2))
lower95cl <-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95cl<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudel
lower95cl
upper95cl

##model1 age gender raceeth log(ucr) adj LOGISTIC
model1l<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr), family=binomial, na.action=na.omit)
summary(model1l)
beta.tAs <- summary(model1l)$coef[2,1]
se.tAs <- summary(model1l)$coef[2,2]
or.crudem1<- exp(beta.tAs*log(2))
lower95m1l<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m1l<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem1
lower95m1l
upper95m1l
##model 2 add educ bmi log(scot) htnmed
model2l<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr)+factor(educat)+bmi+log(scot)+factor(htnmed), family=binomial, na.action=na.omit)
summary(model2l)
beta.tAs <- summary(model2l)$coef[2,1]
se.tAs <- summary(model2l)$coef[2,2]
or.crudem2<- exp(beta.tAs*log(2))
lower95m2l<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m2l<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem2
lower95m2l
upper95m2l
##model 3 add log(sthg) AND log(bas)
model3l<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr)+factor(educat)+bmi+log(scot)+factor(htnmed)+log(sthg)+log(bAs), family=binomial, na.action=na.omit)
summary(model3l)
beta.tAs <- summary(model3l)$coef[2,1]
se.tAs <- summary(model3l)$coef[2,2]
or.crudem3<- exp(beta.tAs*log(2))
lower95m3l<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m3l<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem3
lower95m3l
upper95m3l

##crude POISSON
crudep<-glm(hw4$t2dm~log(hw4$tAs), family=poisson, na.action=na.omit)
summary(crudep)
beta.tAs <- summary(crudep)$coef[2,1]
se.tAs <- summary(crudep)$coef[2,2]
or.crudep<- exp(beta.tAs*log(2))
lower95p<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95p<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudep
lower95p
upper95p
##model1 age gender raceeth log(ucr) adj POISSON
model1p<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr), family=poisson, na.action=na.omit)
summary(model1p)
beta.tAs <- summary(model1p)$coef[2,1]
se.tAs <- summary(model1p)$coef[2,2]
or.crudem1<- exp(beta.tAs*log(2))
lower95m1p<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m1p<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem1
lower95m1p
upper95m1p
##model 2 add educ bmi log(scot) htnmed
model2p<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr)+factor(educat)+bmi+log(scot)+factor(htnmed), family=poisson, na.action=na.omit)
summary(model2p)
beta.tAs <- summary(model2p)$coef[2,1]
se.tAs <- summary(model2p)$coef[2,2]
or.crudem2<- exp(beta.tAs*log(2))
lower95m2p<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m2p<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem2
lower95m2p
upper95m2p
##model 3 add log(sthg) AND log(bas)
model3p<-glm(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+log(ucr)+factor(educat)+bmi+log(scot)+factor(htnmed)+log(sthg)+log(bAs), family=poisson, na.action=na.omit)
summary(model3p)
beta.tAs <- summary(model3p)$coef[2,1]
se.tAs <- summary(model3p)$coef[2,2]
or.crudem3<- exp(beta.tAs*log(2))
lower95m3p<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m3p<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.crudem3
lower95m3p
upper95m3p

##Q2 SMOOTHING METHODS

###model 4 ns
model4.ns<-gam(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+ns(ucr, df=4)+factor(educat)+bmi+ns(scot, df=4)+factor(htnmed)+ns(sthg, df=4)+ns(bAs, df=4), family=binomial, na.action=na.omit)
summary(model4.ns)
beta.tAs <- summary.glm(model4.ns)$coef[2,1]
se.tAs <- summary.glm(model4.ns)$coef[2,2]
or.m4<- exp(beta.tAs*log(2))
lower95m4<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m4<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.m4
lower95m4
upper95m4

##model 5 gam s()
model5.s<-gam(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model5.s)
beta.tAs <- summary.glm(model5.s)$coef[2,1]
se.tAs <- summary.glm(model5.s)$coef[2,2]
or.m5<- exp(beta.tAs*log(2))
lower95m5<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m5<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.m5
lower95m5
upper95m5

##model 6 lo()
model6.lo<-gam(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+lo(ucr)+factor(educat)+bmi+lo(scot)+factor(htnmed)+lo(sthg)+lo(bAs), family=binomial, na.action=na.omit)
summary(model6.lo)
beta.tAs <- summary.glm(model6.lo)$coef[2,1]
se.tAs <- summary.glm(model6.lo)$coef[2,2]
or.m6<- exp(beta.tAs*log(2))
lower95m6<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m6<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.m6
lower95m6
upper95m6

##model 7 mgcv s()

model7.mgcvs<-gam(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model7.mgcvs)
model7.mgcvs$aic
beta.tAs <- summary(model7.mgcvs)$p.table[2,1]
se.tAs <- summary(model7.mgcvs)$p.table[2,2]
or.m7<- exp(beta.tAs*log(2))
lower95m7<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m7<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.m7
lower95m7
upper95m7

##Q3 

model8.mg<-gam(data=hw4, t2dm~s(tAs)+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model8.mg)
par(mfrow=c(1,1))
plot(model8.mg)

model9.mg<-gam(data=hw4, t2dm~s(tAs)+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model9.mg)
par(mfrow=c(1,1))
plot(model9.mg)
beta.tAs <- summary(model9.mg)$p.table[2,1]
se.tAs <- summary(model9.mg)$p.table[2,2]
or.m9<- exp(beta.tAs*log(2))
lower95m9<-exp((beta.tAs-1.96*se.tAs)*log(2))
upper95m9<- exp((beta.tAs+1.96*se.tAs)*log(2))
or.m9
lower95m9
upper95m9

model10.mg<-gam(data=hw4, t2dm~s(log(tAs))+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed), family=binomial, na.action=na.omit)
summary(model10.mg)
par(mfrow=c(1,1))
plot(model10.mg)

model11.mg<-gam(data=hw4, t2dm~s(log(tAs))+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model11.mg)
par(mfrow=c(1,1))
plot(model11.mg)

model11.mg2<-gam(data=hw4, t2dm~log(tAs)+age+factor(gender)+factor(raceeth)+s(ucr)+factor(educat)+bmi+s(scot)+factor(htnmed)+s(sthg)+s(bAs), family=binomial, na.action=na.omit)
summary(model11.mg)
beta.tAs <- summary(model11.mg2)$p.table[2,1]
se.tAs <- summary(model11.mg2)$p.table[2,2]
or.m11<- exp(beta.tAs)
lower95m11<-exp((beta.tAs-1.96*se.tAs))
upper95m11<- exp((beta.tAs+1.96*se.tAs))
or.m11
lower95m11
upper95m11


##Q4

#split tas
summary(hw4$tAs)
tAscat <- hw4$tAs
tAscat[hw4$tAs<=9.285] <- 0
tAscat[hw4$tAs>9.285] <- 1

#split bmi
bmicat <-hw4$bmi
bmicat[hw4$bmi<30] <- 0
bmicat[hw4$bmi>=30] <- 1

table(bmicat, hw4$t2dm, tAscat)



hw4$lowfit<-ifelse(tAscat==0&bmicat==0, 1, 0)
hw4$highfit<-ifelse(tAscat==1&bmicat==0, 1, 0)
hw4$lowobs<-ifelse(tAscat==0&bmicat==1, 1, 0)
hw4$highobs<-ifelse(tAscat==1&bmicat==1, 1, 0)

model13p<-geeglm(data=hw4, t2dm~factor(highfit)+factor(lowobs)+factor(highobs)+age+factor(gender)+factor(raceeth)+factor(educat)+factor(htnmed)+log(ucr)+log(scot)+log(sthg)+log(bAs), family=poisson(link="log"), id=id)
summary(model13p)
exp(model13p$coef)
logistic.display(model13p)

#effect mod
model13p.em<-geeglm(data=hw4, t2dm~factor(highfit)+factor(lowfit)+factor(highobs)+age+factor(gender)+factor(raceeth)+factor(educat)+factor(htnmed)+log(ucr)+log(scot)+log(sthg)+log(bAs), family=poisson(link="log"), id=id)
summary(model13p.em)
exp(model13p.em$coef)
logistic.display(model13p)

#RERI
summary(model13p)$cov.unscaled

#Multiplicative
model14p<-geeglm(data=hw4, t2dm~factor(bmicat)*factor(tAscat)+age+factor(gender)+factor(raceeth)+factor(educat)+factor(htnmed)+log(ucr)+log(scot)+log(sthg)+log(bAs), family=poisson(link="log"), id=id)
summary(model14p)
exp(model14p$coef)

logistic.display(model14p)

#q5 chol and bmi from as
model15 <- lm(as$chol~as$bmi+I((as$bmi>24)*(as$bmi-24)))
summary(model15)
library(ggplot2)
ggplot(as, aes(x=as$bmi, y=as$chol))+geom_point()+stat_smooth(method = 'loess')+labs(title='bivariate association of chol and bmi', x='bmi', y='chol')

model16 <- lm(as$chol~as$bmi)
summary(model16)
plot(as$chol~as$bmi, xlim=c(15, 40), ylim=c(150, 300))
