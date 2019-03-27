#import dataset from sas
install.packages('sas7bdat')
library(sas7bdat)
??sas7bdat
setwd("C:/Users/ethridge/Desktop/BIOINF575/semester project")
data= read.sas7bdat(file='kidsdata.sas7bdat', debug=FALSE)
View(data)

#linear model for metabolic score and continuous telomere length
lm_mets= lm(data$i_mets_z~data$telomere_ts)
summary(lm_mets)
#95% CI
confint(lm_mets)
#correlation for metabolic score and continuous telomere length
cor.test(y=data$i_mets_z, x=data$telomere_ts)

#model for abdominal obesity and continuous telomere length
lm_ob=lm(data$i_wc_z~data$telomere_ts)
summary(lm_ob)
#95% CI
confint(lm_ob)
cor.test(y=data$i_wc_z, x=data$telomere_ts)

#model for blood pressure and continuous telomere length
lm_bp=lm(data$i_map_z~data$telomere_ts)
summary(lm_bp)
#95% CI
confint(lm_bp)
cor.test(y=data$i_map_z, x=data$telomere_ts)

#model for trglyceride and continuous telomere length
lm_tg=lm(data$i_tg_z~data$telomere_ts)
summary(lm_tg)
#95% CI
confint(lm_tg)
cor.test(y=data$i_tg_z, x=data$telomere_ts)

#model for hdl and continuous telomere length
lm_hd=lm(data$i_hdl_z~data$telomere_ts)
summary(lm_hd)
#95% CI
confint(lm_hd)
cor.test(y=data$i_hdl_z, x=data$telomere_ts)

#model for high fasting glucose and continuous telomere length
lm_gl=lm(data$i_homa_ir_z~data$telomere_ts)
summary(lm_gl)
#95% CI
confint(lm_gl)
cor.test(y=data$i_homa_ir_z, x=data$telomere_ts)

#covariate adjusted model for metabolic syndrome z score and continuous telomere length
lm_mets_adj= lm(data$i_mets_z~data$telomere_ts+
                  #demographics=sex, country of origin,height
                  data$i_male+data$id_country+data$i_haz+
                  #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                  data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                  #SES=household income, household assets, household editing, household owned, food security
                  data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_mets_adj)
#95% CI
confint(lm_mets_adj)

#covariate adjusted model for waist circumference z score and telomere length
lm_ob_adj= lm(data$i_wc_z~data$telomere_ts+
                #demographics=sex, country of origin, height
                data$i_male+data$id_country+data$i_haz+
                #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                #SES=household income, household assets, household editing, household owned, food security
                data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_ob_adj)
#95% CI
confint(lm_ob_adj)

#covariate adjusted model for hdl z score and telomere length
lm_hd_adj= lm(data$i_hdl_z~data$telomere_ts+
                #demographics=sex, country of origin, bmi for age z-score, height
                data$i_male+data$id_country+data$i_baz+data$i_haz+
                #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                #SES=household income, household assets, household editing, household owned, food security
                data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_hd_adj)
#95% CI
confint(lm_hd_adj)

#covariate adjusted model for TAG z score and telomere length
lm_tg_adj= lm(data$i_tg_z~data$telomere_ts+
                #demographics=sex, country of origin bmi for age z-score
                data$i_male+data$id_country+data$i_baz+data$i_haz+
                #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                #SES=household income, household assets, household editing, household owned, food security
                data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_tg_adj)
#95% CI
confint(lm_tg_adj)

#covariate adjusted model for high fasting glucose z score and telomere length
lm_gl_adj= lm(data$i_homa_ir_z~data$telomere_ts+
                #demographics=sex, country of origin, bmi for age z-score, height
                data$i_male+data$id_country+data$i_baz+data$i_haz+
                #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                #SES=household income, household assets, household editing, household owned, food security
                data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_gl_adj)
#95% CI
confint(lm_gl_adj)

#covariate adjusted model for blood pressure z score and telomere length
lm_bp_adj= lm(data$i_map_z~data$telomere_ts+
                #demographics=sex, country of origin, bmi for age z-score, height
                data$i_male+data$id_country+data$i_baz+data$i_haz+
                #parental factors=mother bmi, parents have metabolic syndrome, household smoking, mother height, mother parity
                data$m_bmi+data$h_mets_atp3+data$h_smoke+data$m_height+data$m_parity+
                #SES=household income, household assets, household editing, household owned, food security
                data$h_income+data$h_assets+data$h_educ+data$h_owned+data$h_fs_gp)
summary(lm_bp_adj)
#95% CI
confint(lm_bp_adj)

#time to graph!
library(ggplot2)
#xlab modifies the x-axis labeling
#ylab modifies the y-axis labeling
#geom_point() allows for plotting of the scatter graph
#ggtitle() allows for changing of the plot title

#mets and ltl graphed and faceted by sex
#i_male was refactored from 0,1 to Female, Male for ease of labeling in a facet_wrap
data$i_male <- factor(data$i_male, labels = c('Female', 'Male'))
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_mets_z))+geom_point()+facet_wrap(~data$i_male) + 
  ylab("Metabolic Score") + xlab('Telomere Length')

#waist circumference and ltl graphed and faceted by country
#id_country was refactored from 4,5,6,7,8,9 to 'Honduras', 'Nicaragua', "Panama", 'Costa Rica', 'Mexico', 'Belize' for ease of labeling in a facet_wrap
data$id_country <- factor(data$id_country, labels = c('Honduras', 'Nicaragua', "Panama", 'Costa Rica', 'Mexico', 'Belize'))
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_wc_z))+geom_point()+facet_wrap(~data$id_country) +   
  xlab("Telomere Length") + ylab('Waist Circumference')

#hdl and ltl graphed and faceted by height for age (z-score adj)
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_hdl_z))+geom_point()+facet_wrap(~data$i_haz_gp) + 
  xlab('Telomere Length') + ylab('Cholesterol') + ggtitle('Telomere Length vs Cholesterol with Increasing Height (age-adjusted)')
 
#bp and ltl graphed and faceted by household assets
#h_assets_gp was refactored from 0,1,2,3 to 'Few', 'Some', 'More','Numerous' for ease of labeling in a facet_wrap
data$h_assets_gp <- factor(data$h_assets_gp, labels = c('Few', 'Some', 'More','Numerous' ))
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_map_z))+geom_point()+facet_wrap(~data$h_assets_gp) +
xlab('Telomere Length') + ylab('Blood Pressure') + ggtitle('Telomere Length vs Blood Pressure with Increasing Household Assets')

#tag and ltl graphed and faceted by household education
#h_educ_gp was refactored from 1,2,3,4,5 to 'Incomplete Elementary', 'Complete Elementary', 'Incomplete Secondary','Complete Secondary', 'Post Secondary' 
#for ease of labeling in a facet_wrap
data$h_educ_gp <- factor(data$h_educ_gp, labels = c('Incomplete Elementary', 'Complete Elementary', 'Incomplete Secondary','Complete Secondary', 'Post Secondary' ))
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_tg_z))+geom_point()+facet_wrap(~data$h_educ_gp)+
  xlab('Telomere Length') + ylab('Triacylglycerol Levels') + ggtitle('Telomere Length vs Triacylglycerol Levels with Increasing Household Education')

#hf glucose and ltl graphed and faceted by household income
#i_income_gp was refactored from 1,2,3 to 'Low', 'Medium', 'High','Not Recorded' for ease of labeling in a facet_wrap
data$h_income_gp <- factor(data$h_income_gp, labels = c('Low', 'Medium', 'High','Not Recorded'))
ggplot(data=data, aes(x=data$telomere_ts, y=data$i_homa_ir_z))+geom_point()+facet_wrap(~data$h_income_gp)+
  xlab('Telomere Length') + ylab('High Fasting Glucose Levels') + ggtitle('Telomere Length vs High Fasting Glucose Levels with Increasing Household Income')
str(data)
