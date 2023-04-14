#Final HRM 753
#############################Q-5############################################

Performance_data <- read_excel("C:/Users/humay/Dropbox/HRM/HRM 753/Final/RQ5-Performance2023.xlsx")

save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Final/Performance.RData")
load("C:/Users/humay/Dropbox/HRM/HRM 753/Final/Performance.RData")

view(Performance_data)

Performance_data$hispanic
Performance_data$black
Performance_data$performance

options (scipen=999)
library (tidyverse)
library(freqtables)

Performance_data %>% 
  freq_table(hispanic, black)
#black= 1, hispanic= 2, other= 3
Performance_data$race <- ifelse(Performance_data$black == 1, 1, 
                               ifelse(Performance_data$hispanic == 1, 2, 
                                      3))
Performance_data$race

#a)	Normal distribution of the outcome

library(ggplot2)

histogram.performance=ggplot(Performance_data, aes(x = performance)) + 
  geom_histogram(aes(y =..density..), binwidth = 1,
                 colour = "white", 
                 fill = "#00BA38") +
  labs(x="Performance",
       y= NULL,
       title= "")+
  stat_function(fun = dnorm, args = list(mean = mean(Performance_data$performance), sd = sd(Performance_data$performance)))

histogram.performance

#b)	Shapiro-Wilk test
shapiro.test(Performance_data$performance)

#c) Correlation matric 
cor.Performance_data= data.frame(Performance_data$performance, 
                                 Performance_data$race,
                                 Performance_data$ses,
                                 Performance_data$female)
head (cor.Performance_data)
sum(is.na(cor.Performance_data$Performance_data.race ))


cor.Performance_data$performance= as.numeric(cor.Performance_data$Performance_data.performance) 
cor.Performance_data$race = as.numeric(cor.Performance_data$Performance_data.race)
cor.Performance_data$ses= as.numeric(cor.Performance_data$Performance_data.ses)
cor.Performance_data$female= as.numeric(cor.Performance_data$Performance_data.female)

cor_matrix=cor(cor.Performance_data, use = "pairwise.complete.obs")
cor_matrix

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45)

#d) check for confounding 
library (chest)

vlist <- c("ses", "female")

results <-chest_lm (crude = "performance ~ race", xlist = vlist, data = Performance_data)
results

chest_plot (results, 
            cex.lab=5, 
            cex.axis=5, 
            hjust = 0.5,
            height = 0.04,
            point_size = 4,
            nudge_y = 0.2,
            zero = 1.5)

#only the ses variable shows 23.29% changes, so, this might be a potential confounder  

#manually
#conf
lm.ses=lm (performance ~ race+ses, data=Performance_data)
lm.ses
summary (lm.ses)

#inter
lm.race.ses=lm (performance ~ race+race*ses, data=Performance_data)
lm.race.ses
summary (lm.race.ses)

lm.race.sex=lm (performance ~ race+race*female, data=Performance_data)
lm.race.sex
summary (lm.race.sex)

#Final model
library(lmerTest)
library(lme4)
lm_random.int <- lmer(performance ~ as.factor (race)+ as.factor (ses) +female+ (1|childid), REML=F, data = Performance_data)
lm_random.int
summary (lm_random.int)
confint(lm_random.int, level = 0.95)

lm_random.slope <- lmer(performance ~ as.factor (race)+ as.factor (ses) +female+ (0+race|childid), REML=F, data = Performance_data)
lm_random.slope
summary (lm_random.slope)

lm_random.int.slope <- lmer(performance ~ as.factor (race)+ as.factor (ses) +female+ (1+race|childid), REML=F, data = Performance_data)
lm_random.int.slope
summary (lm_random.int.slope)

#Model fitness

# Create a Q-Q plot of the residuals
qqnorm(resid(lm_random.int))
qqline(resid(lm_random.int))

# Create residual plots
plot(lm_random.int, which = 1:3)



###################################Q-6################################
#Looking for the missing data
sum(is.na (Mortality_data))
Mortality_data
12/800
0.015*100
#here i have only 1.5% missing 
nrow(Mortality_data)

#therefore, the Multiple imputation (MI) is not recommended method 
#for handling MNAR missing data.

#model
Mortality_data$syblings
Mortality_data$death

glm.sib= glm (death~ syblings, data=Mortality_data, family= binomial)
summary (glm.sib)
or.glm.sib=exp (cbind(coef(glm.sib), confint(glm.sib, level=0.95)))
or.glm.sib

AIC(glm.sib)
AICc(glm.sib)
BIC (glm.sib)

glm.mage= glm (death~ mage, data=Mortality_data,family= binomial)
summary (glm.mage)
or.glm.mage=exp (cbind(coef(glm.mage), confint(glm.mage, level=0.95)))
or.glm.mage

AIC(glm.mage)
AICc(glm.mage)
BIC (glm.mage)

#check for confounding
vlist.6.a <- c("mage", "income", "sex", "periphery", "home")

results.6.a <-chest_glm (crude = "death ~ syblings", xlist = vlist.6, data = Mortality_data)
results.6.a

#there was no change >10%, so, there might not be potential confounded 


vlist.6.b <- c("syblings", "income", "sex", "periphery", "home")

results.6.b <-chest_glm (crude = "death ~  mage", xlist = vlist.6, data = Mortality_data)
results.6.b

glm (death ~ mage+income,  data = Mortality_data)
glm (death ~ mage,  data = Mortality_data)

(0.001149-0.0011337 )*100

#there was no change >10%, so, there might not be potential confounded 
#therefore, we do not need to add any potential co-variate in the model 


#check for the effect modifier in 6.a 
glm.death.mage.a=glm (death ~ syblings+syblings*mage, data=Mortality_data)
glm.death.mage.a
summary (glm.death.mage.a) #not 

glm.death.income.a=glm (death ~ syblings+syblings*income, data=Mortality_data)
glm.death.income.a
summary (glm.death.income.a) #yes 

glm.death.sex.a=glm (death ~ syblings+syblings*sex, data=Mortality_data)
glm.death.sex.a
summary (glm.death.sex.a) #not 

glm.death.periphery.a=glm (death ~ syblings+syblings*periphery, data=Mortality_data)
glm.death.periphery.a
summary (glm.death.periphery.a) #not 

glm.death.home.a=glm (death ~ syblings+syblings*home, data=Mortality_data)
glm.death.home.a
summary (glm.death.home.a) #not 

#Mortality_data$income has interaction, we have to do stratified analysis
summary (Mortality_data$income)

#

#1 >99, 0 <100
Mortality_data$income.cat= ifelse (Mortality_data$income>99, 1, 0)
Mortality_data$income.cat

glm.death.income.cat.a=glm (death ~ syblings+syblings*income.cat, data=Mortality_data)
glm.death.income.cat.a
summary (glm.death.income.cat.a) #no 
#but income was interacted when it was contentious, but when the income was categorized, it was not  interacted. 

#check for the effect modifier in 6.b 
glm.death.syb.b=glm (death ~ mage+mage*syblings, data=Mortality_data)
glm.death.syb.b
summary (glm.death.syb.b) #no

glm.death.income.b=glm (death ~ mage+mage*income.cat, data=Mortality_data)
glm.death.income.b
summary (glm.death.income.b) #no

glm.death.sex.b=glm (death ~ mage+mage*sex, data=Mortality_data)
glm.death.sex.b
summary (glm.death.sex.b) #yes 

lm.race.periphery=glm (death ~ mage+mage*periphery, data=Mortality_data)
lm.race.periphery
summary (lm.race.periphery) #not 

lm.race.home=glm (death ~ mage+mage*home, data=Mortality_data)
lm.race.home
summary (lm.race.home) #not 

#as the sex was interacted the association between the death and mage, need to be stratified
Mortality_data$sex
glm.death.sex.b <- by(Mortality_data, Mortality_data$sex, function(df) {
  glm(death ~ mage, data = df)
})
glm.death.sex.b

# extract p-values for each model
pvals.glm.death.sex.b <- sapply(glm.death.sex.b, function(model) {
  summary(model)$coefficients[, 4]
})

# print the p-values for each sex
pvals.glm.death.sex.b

#the p value for the sex=0 was 0.08 and for sex=1 was 0.21, 
#so, the stratified analysis also confirmed 
#that there was not significant association between the death and mage

# extract odds ratios and 95% CIs for each model
or.glm.death.sex.b <- lapply(glm.death.sex.b, function(model) {
  exp(coef(model))
})
ci.glm.death.sex.b <- lapply(glm.death.sex.b, function(model) {
  exp(confint(model))
})

# format the results
or.glm.death.sex.b <- do.call(rbind, or.glm.death.sex.b)
ci.glm.death.sex.b <- do.call(rbind, ci.glm.death.sex.b)
colnames(or.glm.death.sex.b) <- colnames(ci.glm.death.sex.b) <- c("OR", "2.5%", "97.5%")
rownames(or.glm.death.sex.b) <- paste("sex", names(glm.death.sex.b))

# print the results
or.glm.death.sex.b
ci.glm.death.sex.b

#Building model

#looking at the correlation
Mortality_data.na.omit= na.omit(Mortality_data)

cor.Pressures.data= data.frame(Pressures$htn, 
                               Pressures$cholesterol,
                               Pressures$bmi,
                               Pressures$sex.fct,
                               Pressures$age)
head (cor.Pressures.data)



cor.Pressures.data$sex= as.numeric(cor.Pressures.data$sex) 


cor_matrix.pressure=cor(cor.Pressures.data, use = "pairwise.complete.obs")
cor_matrix

#Final model

glm.sib= glm (death~ syblings, data=Mortality_data, family= binomial)


#ROC
library(pROC)
sum (is.na (Mortality_data))

Mortality_data.na.omit <- na.omit(Mortality_data)

glm.sib.prob= predict(glm.sib, type=c("response"))

typeof(Pressures$htn)
Pressures.na.omit$htn.fct= as.factor (Pressures.na.omit$htn)

glm.final.6.roc_object <- roc( Mortality_data.na.omit$death,  glm.sib.prob)

auc( glm.final.6.roc_object )


#roc plot 
plot (glm.final.6.roc_object, col=rainbow(7), main="ROC curve for death", print.auc=TRUE)


###################################Q-7###############################

#create HTN
sum (is.na (Pressures))/nrow (Pressures)

sum (is.na (Pressures$cholesterol))
sum (is.na (Pressures$bmi))

#the bmi and cholesterol has 9 and 31 missing value that is less than 1%.
#therefore, we can ignore this missingness as it is less than 5% 

sum (is.na(Pressures$sbp))
sum (is.na(Pressures$dbp))

Pressures$htn = ifelse(Pressures$sbp > 129 & Pressures$dbp > 89, 1, 0)
Pressures$htn 

#What personal characteristics are related to blood pressure? 

#characteristics are related to blood pressure

glm.htn.cholesterol= glm(htn~cholesterol, data=Pressures)
summary (glm.htn.cholesterol) #p < 0.001
glm.htn.cholesterol

or.glm.htn.cholesterol=exp (cbind(coef(glm.htn.cholesterol), confint(glm.htn.cholesterol, level=0.95)))
or.glm.htn.cholesterol

glm.htn.bmi= glm(htn~bmi, data=Pressures)
summary (glm.htn.bmi) #p < 0.001
or.glm.htn.bmi=exp (cbind(coef(glm.htn.bmi), confint(glm.htn.bmi, level=0.95)))
or.glm.htn.bmi

glm.htn.age= glm(htn~age, data=Pressures)
summary (glm.htn.age) #p < 0.001
or.glm.htn.age=exp (cbind(coef(glm.htn.age), confint(glm.htn.age, level=0.95)))
or.glm.htn.age

glm.htn.sex= glm(htn~sex, data=Pressures)
summary (glm.htn.sex) #p = 0.000292
or.glm.htn.sex=exp (cbind(coef(glm.htn.sex), confint(glm.htn.sex, level=0.95)))
or.glm.htn.sex

#in the bivariate analysis, we found bmi, cholesterol, age, sex were associated in p<0.05.


Pressures$sex.fct= ifelse (Pressures$sex=="Men", 1, 2)

#check for correlation
cor.Pressures.data= data.frame(Pressures$htn, 
                               Pressures$cholesterol,
                               Pressures$bmi,
                               Pressures$sex.fct,
                               Pressures$age)
head (cor.Pressures.data)



cor.Pressures.data$sex= as.numeric(cor.Pressures.data$sex) 


cor_matrix.pressure=cor(cor.Pressures.data, use = "pairwise.complete.obs")
cor_matrix

#multiple regression model
glm.7= glm(htn~cholesterol+bmi+sex+age, data=Pressures)
summary (glm.7)

or.glm.7=exp (cbind(coef(glm.7), confint(glm.7, level=0.95)))
or.glm.7


#we have to create new age.cat variable 

#age.cat= 1>50, 0<51
Pressures$age.cat= ifelse (Pressures$age>50, 1, 0)
Pressures$age.cat

#Were these different for age > 50 vs age <= 50? 
glm.age.int= glm(htn~cholesterol+cholesterol*age.cat, data=Pressures)
summary (glm.age.int)

#yes, this difference is influence by age
glm.age.bmi.int= glm(htn~bmi+bmi*age.cat, data=Pressures)
summary (glm.age.bmi.int)

#BMI >25
Pressures$bmi.cat= ifelse (Pressures$bmi>24, 1, 0)
#Were these different for BMI >25 vs BMI <25? 
glm.bmi.int= glm(htn~cholesterol+cholesterol*bmi.cat, data=Pressures)
summary (glm.bmi.int)
#no, this difference is not influence by bmi


Pressures.age.50.more <- subset(Pressures, age >50)
Pressures.age.50.less <- subset(Pressures, age <=50)

#characteristics are related to blood pressure by age 
glm.htn.bmi.age.50.more= glm(htn~bmi, data=Pressures.age.50.more)
summary (glm.htn.bmi.age.50.more) #p < 0.001
glm.htn.bmi.age.50.less= glm(htn~bmi, data=Pressures.age.50.less)
summary (glm.htn.bmi.age.50.less) #p < 0.001

glm.htn.cholesterol.50.more= glm(htn~cholesterol, data=Pressures.age.50.more)
summary (glm.htn.cholesterol.50.more) #p > 0.05
glm.htn.cholesterol.50.less= glm(htn~cholesterol, data=Pressures.age.50.less)
summary (glm.htn.cholesterol.50.less) #p < 0.001

glm.htn.sex.age.50.more= glm(htn~sex, data=Pressures.age.50.more)
summary (glm.htn.sex.age.50.more) #p = 0.085
glm.htn.sex.age.50.less= glm(htn~sex, data=Pressures.age.50.less)
summary (glm.htn.sex.age.50.less) #p < 0.001

#characteristics are related to blood pressure by age <50
glm.htn.bmi.age.50.less.7= glm(htn~bmi+cholesterol+sex, data=Pressures.age.50.less)
summary (glm.htn.bmi.age.50.less.7)

or.glm.htn.bmi.age.50.less.7=exp (cbind(coef(glm.htn.bmi.age.50.less.7), confint(glm.htn.bmi.age.50.less.7, level=0.95)))
or.glm.htn.bmi.age.50.less.7

library (car)
vif(glm.htn.bmi.age.50.less.7)

#bmi >25
Pressures.bmi.25.more <- subset(Pressures, bmi >24)
Pressures.bmi.25.more

#characteristics are related to blood pressure by bmi >25
glm.htn.age.bmi.25.more= glm(htn~age, data=Pressures.bmi.25.more)
summary (glm.htn.age.bmi.25.more) #p < 0.001

glm.htn.cholesterol.bmi.50.more= glm(htn~cholesterol, data=Pressures.bmi.25.more)
summary (glm.htn.cholesterol.bmi.50.more) #p < 0.001

glm.htn.sex.bmi.25.more= glm(htn~sex, data=Pressures.bmi.25.more)
summary (glm.htn.sex.bmi.25.more) #p > 0.17

#Model characteristics are related to blood pressure by bmi >25
glm.htn.bmi.25.more.7= glm(htn~cholesterol+age, data=Pressures.bmi.25.more)
summary (glm.htn.bmi.50.more.7)

or.glm.htn.bmi.50.more.7=exp (cbind(coef(glm.htn.bmi.50.more.7), confint(glm.htn.bmi.50.more.7, level=0.95)))
or.glm.htn.bmi.50.more.7

#multi-collinearity
library (car)
vif(glm.htn.bmi.50.more.7)

#Buidling the final model

glm.final.7= glm(htn~cholesterol+bmi+sex, data=Pressures)
summary (glm.final.7)

or.glm.final.7=exp (cbind(coef(glm.final.7), confint(glm.final.7, level=0.95)))
or.glm.final.7

library (car)
vif(glm.final.7)

#ROC
library(pROC)
sum (is.na (Pressures))
Pressures.na.omit <- na.omit(Pressures)

glm.final.7.prob= predict(glm.final.7, type=c("response"))

typeof(Pressures$htn)
Pressures.na.omit$htn.fct= as.factor (Pressures.na.omit$htn)

glm.final.7.roc_object <- roc( Pressures.na.omit$htn.fct,  glm.final.7.prob)

auc( glm.final.7.roc_object )


#roc plot 
plot (glm.final.7.roc_object, col=rainbow(7), main="ROC curve for hypertention", print.auc=TRUE)

library(AICcmodavg) # for the AICc() function

AIC(glm.final.7)
AICc(glm.final.7)
BIC (glm.final.7)
