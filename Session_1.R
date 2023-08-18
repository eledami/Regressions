# Regressions with R 2021 - Session 1

# load data.sets
load("~/Desktop/Regressions with R/Regressions with R datasets.Rdata") # pasted from console

# linear regression #### 

# tango data.set 

View(tango) # variables information: tango (continuous numerical), 
# RU (treatment): A Absent P present (categorical, binary), genotype (caterogical, nominal)

summary(tango$Tango) # results in mean
summary(tango$RU) # results is counts 

# t-test
t.test(tango$Tango~tango$RU) # p-value is just not significant (borderline significance)

# linear regression
m.one <- lm(Tango~RU, data=tango) # Tango is outcome (Y), RU is predictor (X) 

m.one

# NOTE: coefficients interpretation: mean(tango) = 0.7095 + 0.3001 (RU)

# b0 (intercept) is constant, b0 is the value of y when x is 0 (xi=0 > y=b0)
# > when the treatment is (0) absent, the outcome is absent tango = 0.7095

# b1 is slope. b1 is the change in the outcome for any unit, change in the predictor after adjusting whatever else in the model.
# > when th treatment is (1) present, the outcome is 0.7095 + 0.3001 (coefficient linked to X). 
# The table display the variable type RUP P for present! 

# >> The RUP coefficient is the change in the outcome in every unit change in the predictor. 

summary(m.one) # more info
# residuals you can check if there is a little bit of symmetry. 

# confidential intervals
confint(m.one)

# residuals
residuals(m.one)
hist(residuals(m.one)) # check normality, there is also calculations around that. 
# That is a good normal distribution especially small 
# Spiro test (normality test), Wicox test, not encourage people to use it. 

# plot all model 
plot(m.one) # go in console and hit return four times for seeing 4 different plots, alternative:

par(mfrow=c(2,2)) # par graphical parameters to change, mfrow (multiple figures rows)
plot(m.one)

# graph 1: fitted values axes is the range between the means of A and P (coefficients)

# plot residuals again predictor, equality distributed around zero and equally distributed. 
plot(tango$RU, resid(m.one))
class(tango$RU)# when we have a factor, R automatically plot a boxplot, we want something better:
plot(as.numeric(tango$RU), resid(m.one))
abline(h=0) # we have two columns of values/dots because of categorical variable
# interpretation: good close to 0 and equal positive and negative. 


# check specific things
names(summary(m.one)) # check all the options 
summary(m.one)$adj.r.squared # picking a specific onecoef
summary(m.one)$adj
summary(m.one)$coef
coef(m.one)
confint(m.one)
BIC(m.one)
AIC(m.one)
deviance(m.one)

# fitted model 

fitted(m.one)
table(fitted(m.one)) # only two possible values (not strange cos categorical varaible A and P 9 and 9)

predict(m.one)
table(predict(m.one))
predict(m.one, newdata=data.frame(RU='A')) # can do this only with predict not fitted function (main difference). 
# I want a prediction for the absent level of RU treatment only (not the P), use newdata for selecting only a 
# section of the data. 
predict(m.one, newdata=data.frame(RU='P')) # result is the mean of the tango values with P and A. It is the same as this function:
tapply(tango$Tango, tango$RU, mean)
coef(m.one) # the sum of the two coefficients is equal to the result of predict(m.one, newdata=data.frame(RU='P')) (1)

# residuals
residuals(m.one)
hist(residuals(m.one)) # check normality, there is also calculations around that. 
# That is a good normal distribution especially small 
# Spiro test (normality test), Wicox test, not encourage people to use it. 

# plot all model 
plot(m.one) # go in console and hit return four times for seeing 4 different plots, alternative:

par(mfrow=c(2,2)) # par graphical parameters to change, mfrow (multiple figures rows)
plot(m.one)

# graph 1 (residuals vs fitted) fitted values axes is the range between the means of A and P (coefficients), 
# graph 2 (QQ plot): the diagonal line represents perfect distribution, thus the dots need to be close to the line
# graph 3 (scale-location): transformation of residuals (y axes, squared root), 
# sometimes this measure is more robust than the row (graph 1) very similar do (depends on field). 
# graph 4 (similar as well) just division of A and P
# note R highlights the residuals that are outliers > sensitivity analysis. You could try to play with things (conventionally: try to remove all of them, but not standard way).
# In this case, problem: small sample, loose power. 

# influence measures 

influence.measures(m.one) #  many values...dfBeta (difference in fitted values), Cooks distance
# the computer re-run the modules without that value (ID) and then re-run the  new intercepts, coefficents, 
#fitted, residuals..calculate the difference from original model and changes are in the table!

inf.m.one <- influence.measures(m.one)

inf.m.one$infmat[,1] # infmat to call columns, 1 first column, 2 second column, etc. 
hist(inf.m.one$infmat[,1]) # histogram of difference in fitted values for the intercepts. Dfbeta for intercepts, 
#by removing each observation there will be some changes to results, the changes are documented and plotted. 
#Look for extreme cases, most of the changes are between -0,2 and 0. Ideal it is good symmetrical distribution and 
#majority of values close to 0. What you do not want to see is cases away from the others (non-asseymetry), 
#for example -0.6 cases are a bit strange outliers -> go to check ID and pull them out.  
hist(inf.m.one$infmat[,2]) # histogram of difference in fitted values for the coefficients. Not looking to odd again. 
hist(inf.m.one$infmat[,3]) # difference in fitted values
hist(inf.m.one$infmat[,5]) # difference in residuals
layout(1)

# alternative functions for same above actions
hist(dffits(m.one))
hist(cooks.distance(m.one))
dfbeta(m.one)[,1]
hist(dfbeta(m.one)[,1])
hist(dfbeta(m.one)[,2])

# in genreal it is impossible to see all close to 0, 
# the important is not to see any extreme changes! If you see an extreme change you can look at the values specifically:

# need ID case column
names(tango)
tango$ID <- 1:18
View(tango)
tango[dfbeta(m.one)[,1] < -0.03, 'ID'] # associate the results of dfbeta(m.one)[,1] with values less than the value -0.03, 
# and give me as results the ID
# now let's check 10 and 11 and check them, the values are in the 
# lower end of the the A group, but they are not so far from the mean. So not sure it is not very odd, only 18 cases.

# add the line (the connection of the fitted values)
plot(as.numeric(tango$RU), tango$Tango)
points(as.numeric(tango$RU), fitted(m.one), col ='red', pch="*", cex=2) # och = point character, cex = character expansion
lines(as.numeric(tango$RU), fitted(m.one), col ='red', pch="*", cex=2)
fitted(m.one)

# Pratical 1 

table(tango$Genotype) 
class(tango$Genotype)

summary(tango$Tango) 
summary(tango$Genotype) 
summary(tango$RU)

# linear regression
m.two <- lm(Tango~Genotype, data=tango) 
m.two
summary(m.two)

# a.a Abeta is the default reference category for genotype
# a.b. the average difference between Abeta and RNAi is -0.38
tapply(tango$Tango, tango$Genotype, mean) # 0.94-0.56 = 0.38
# a.c. 0.1169 - (-0.38)  = 0.49 X 0.56-1.06 = -0.5 (means difference)

# multiple regression
m.three <- lm(Tango~Genotype+RU, data=tango)
m.three
summary(m.three)
plot(m.three)

# a.a. No, we could remove UAS Arm, but no.  
# a.b  UAS is not significant. 

# genotype + treatment + interaction between the two
m4 <- lm(Tango~RU*Genotype, data=tango) # OR
m4 <- lm(Tango~RU+Genotype+RU:Genotype, data=tango)
m4 

summary(m4)

# how to change the referece

tango$Genotype2 <- relevel(tango$Genotype,
                           ref='RNAi')
table(tango$Genotype2)
m2.new <- lm(Tango~Genotype2,data=tango)
summary(m2.new)

# extra plot 

plot(as.numeric(tango$Genotype),tango$Tango)

plot(as.numeric(tango$Genotype),tango$Tango,
     ylab="Tango",xlab="Genecode",
     cex=2,xaxt="n",
     col=c("red","green")[tango$RU],
     cex.axis=2,
     cex.lab=1.5,
     pch=c("X","O")[tango$RU])
axis(1,1:3,labels=levels(tango$Genotype),
     cex.axis=2)
legend("top",legend=levels(tango$RU)[c(1,2)],
       col=c("red","green")[c(1,2)],
       pch=c("X","O")[c(1,2)],box.lty=2)

# a.c 0.3001, mean, -0.38, adjusting for gynotype (because we do not have an interaction in the model -> model 4)

# comparing models
summary(m.two)$adj
summary(m.three)$adj
summary(m4)$adj
BIC(m.two)
BIC(m.three)
BIC(m4)

#multicollinearity 
install.packages('car')
library(car)
vif(m.three) # check the generalized VIF. 5 or 10 is the treeshold.In our case they are very low. 

?lm

# null model 

m.null <- lm(Tango~1,data=tango)
summary(m.null)
mean(tango$Tango)
BIC(m.null)
summary(m.null)$adj

# logistic regression ####
View(titanic)
dim(titanic)

# explore binary outcome variable (survived)
titanic$survived
t.s <- table(titanic$survived)
addmargins(t.s)
prop.table(t.s)*100 # add percentage

titanic$survived2 <- factor(titanic$survived,
                            levels=c(0,1),
                            labels=c('died','survived')) # change the names of the categorical varaibles (created a new column)
table(titanic$survived2)
View(titanic)

# predictor

summary(titanic$age)

# relationship plotted
plot(titanic$age, titanic$survived2, cex=0.5) # labels of categorical variables in number just case it is the standard. 
plot(titanic$age, titanic$survived, cex=0.5)

# fit the logistic regression model 
m.log.1 <- glm(survived2~age, data=titanic, family = binomial) # family is the type of regression without family it runs linear, binomial is for regression,
                                                                # a distribution showing how binary variables distribution. Linear 'guaniass'
summary(m.log.1)
# interpretation: residuals is not important
# coefficient table: p values not significant. Estimate is negative. We cannot the change in ln in the odds of survival, we need to exponentiate. 
# deviance of null module (deviance from the modules without predictors only intercepts (without age only 1)) 
# Residual deviance is the deviance of this fitted module. # by reading this we can understand we are doing something better than putting nothing (residual deviance less than null deviance)
# can do the same with BIC

exp(coef(m.log.1)) # 0.99 the changes of odds in survival for the unit change in age (odds ratios) # very low
exp(confint(m.log.1)) # very small so not good. 

# practical 2

## exploration

summary(titanic$fare)
hist(titanic$fare)

summary(titanic$sex)

summary(titanic$pclass)
unique(titanic$pclass)

# change variable pclass
titanic$pclass2 <- factor(titanic$pclass,
                            levels=c(1, 2, 3),
                            labels=c('first','second', 'third'))
View(titanic)
summary(titanic$pclass2) # frequency now! 
prop.table(table(titanic$pclass2))*100
           
# all variables 
m.log.2 <- glm(survived2~age + sex + fare + pclass2, data=titanic, family=binomial)
summary(m.log.2) # interesting age here is significant, when it was alone it wasn't. 
exp(coef(m.log.2))
exp(confint(m.log.2))

# gender -> very poor model -> BIC

m.log.3 <- glm(survived2~sex, data=titanic, family=binomial)
plot(titanic$gender, titanic$survived2, cex=0.5)
summary(m.log.3)
exp(coef(m.log.3))
exp(confint(m.log.3))

# fare -> very poor model -> BIC

m.log.4 <- glm(survived2~fare, data=titanic, family=binomial)
plot(titanic$fare, titanic$survived2, cex=0.5)
summary(m.log.4)
exp(coef(m.log.4))
exp(confint(m.log.4))

# Possibility to remove fare 
m.log.5 <- glm(survived2~age + sex + pclass2, data=titanic, family=binomial)
summary(m.log.5) 
exp(coef(m.log.5))
exp(confint(m.log.5))

BIC(m.log.2)
BIC(m.log.3)
BIC(m.log.4)
BIC(m.log.5) # this model better lower value

# collinearity 
library(car)
vif(m.log.5)

# interaction check needs plot 

par(pty='s')
plot (titanic$age, jitter(titanic$survived),
      ylab="Survived",xlab="Age",
      cex=1,yaxt="n",
      cex.axis=2,
      cex.lab=1.5,
      col=c(1,2)[titanic$sex])
axis(2,0:1,labels=c('no','yes'),
     cex.axis=2)
legend('center',
       legend=c('female','male'),
       col=c(1,2),pch=1) 

par(pty='s')
plot (titanic$fare, jitter(titanic$survived),
      ylab="Survived",xlab="Age",
      cex=1,yaxt="n",
      cex.axis=2,
      cex.lab=1.5,
      col=c(1,2, 3)[titanic$pclass2])
axis(2,0:1,labels=c('no','yes'),
     cex.axis=2)
legend('center',
       legend=c('first','second', 'third'),
       col=c(1,2),pch=1)

# interpretation: try to play with. We see age and gender overlapping over the age line, a good spread, 
# so we can see their interaction. 
# How the interaction affect above or below the main effect. So need to keep reference term in the new model.

# interaction
m.log.int <- glm(survived2~sex+age+pclass2+age*sex,
                 data=titanic, family=binomial)
summary(m.log.int)
exp(coef(m.log.int))
exp(confint(m.log.int))

# impact on outcome for simultaneous variables. 

# prediction -> look at specific category. 

predict(m.log.int, 
        newdata=data.frame(sex=c('male','female'),age=30,pclass2='second'))

# the result is the ln of the odds. A male had lower change of surviving comparing to a fmale (-1.3 vs. 1.24). This is probability.

# a. Odds ratio for gender: 0.08 (95%, CI 0.05, 0.11). Being male is associate with a decreases in the odds of survival. # 92% decrease for males, 89% to 95% decrease (CI)
# The decrease is 1-0.08 = 0.92. 92% decrease in the odds of survival for male survival. For finding female coeafficent 1/0.08 (or change reference)
# b. first class (transformed above)
# c. Odds ratio 1.01 or 1.0, 1-0 is 0 increases. 
# However, it had a low p-value in the model 2, but high p values in model 4. 
# d. interactions: fare and class because they measure something very similar
# e. predictions:....

# extra test: 
# F-test is translated in Hosmer and Lemeshow test for logistic regression.-> code of solutions 
# ROC - reciever operationg curves. -> code in the solutions

# ordinal logistic regression ####

install.packages('ordinal')
library(ordinal)

# data
View(cd)

# variables used 
table(cd$height)
class(cd$height) # problem I need factor! 

cd$height2 <- factor(cd$height, 
                     levels=c(1,2,3),
                     labels=c('low', 'middle', 'high'))
table(cd$height2)
addmargins(table(cd$height2))

table(cd$genomic) # it is categorical variable

# regression

m.ord.1 <- clm(height2~genomic, data=cd)
summary(m.ord.1) # not residual anymore, loglik is the deviance, nobs. 

exp(coef(m.ord.1))
exp(confint(m.log.int))

# genomicwt: wt is the genotype the predictor. 
# Flies with wt have higher odds of flying higher than the flies with floxonull gene. 4.57-1 = 35.7 357% 

table(cd$genomic, cd$height2) # just the counts also show that. 

# low/middle -> is a comparison between levels. odds of bottom level against higher. 
# 552/(234+206) refers to the fownowell (the reference). 
# middle/high -> odds of middle or lower against higher (552+234)/206. 

552/(234+206) # odds of bottom level against higher
(552+234)/206 # odds of middle or lower against higher

predict(m.ord.1,newdata=data.frame('genomic'='wt'),type='class')

predict(m.ord.1,newdata=data.frame('genomic'='foxonull'),type='class')

# predicted cumulative prob for each level for each genomic group:

predict(m.ord.1,newdata=data.frame('genomic'='wt'),type='cum.prob')

# Poisson regresssion ####

View(fa)
# sumfeeding -> groups of flies and how many of them ate by genotype. Rows are groups. 

table(fa$Genotype)
table(fa$Food)
fa$Sumfeeding+fa$Sumnonfeeding

summary(fa$Sumfeeding) # average 37.35
tapply(fa$Sumfeeding, fa$Food, mean)
tapply(fa$Sumfeeding, fa$Food, var)

# 
m.pos.1 <- glm(Sumfeeding~Food,data=fa,
               family=poisson)

summary(m.pos.1)
# interpretation 
# residuals -> evaluate histogram, assymetry. 
# we cannot interpret coefficents they need to be exponentiated. 

levels(fa$Food)
exp(coef(m.pos.1)) # 31.75 the avarage feeding for food type 0.2, 1.3952 is the rate ratio and 1.13 is also a rate ratio. 
# 1.39 means 39% increases in the outcome (avarage count of feeding) for food 1 vs food 0. 
# It is the difference between 1Y and and 0.2Y. It is the difference between 2Y 

exp(confint(m.pos.1))

tapply(fa$Sumfeeding,fa$Food,mean)

# you can do the same checks as other regressions

# differences, there are some assumptions Poisson regression: 
# it requires that the mean and the variate parameters. Outcome have the mean and varaint are equal 

mean(fa$Sumfeeding, fa$Food, mean)
var(fa$Sumfeeding, fa$Food, var)

tapply(fa$Sumfeeding, fa$Food, mean)
tapply(fa$Sumfeeding, fa$Food, var)

# need to build different model! 

library(MASS)
m.nb.1 <- glm.nb(Sumfeeding~Food,
                 data=fa)

summary(m.nb.1) # changes in p-value! 

# sensitivity analysis
BIC(m.pos.1)
BIC(m.nb.1)

exp(coef(m.nb.1))
exp(confint(m.nb.1))
exp(confint(m.pos.1))

# Poisson with offset element 

fa$totalsize <- fa$Sumfeeding+fa$Sumnonfeeding
fa$totalsize

m.pos.offset <- glm(Sumfeeding~Food,
                    data=fa,
                    offset=totalsize,
                    family=poisson)

#or 

m.pos.offset <- glm(Sumfeeding~Food+offset(totalsize),
                    data=fa,
                    family=poisson)

summary(m.pos.offset)
exp(coef(m.pos.offset))

# yes there is a limit, and impact power! Any analysis should start with a power calculation, 
# how many predictors, how many levels. 1710 of your sample size or 
# the square route of the sample size should be the amount of predictors. Problematic for the model. 
# 10 events for a predictor. If only ended. Power calculations. Sample size!
# lower categories -> power calculation. 

# cox regression, time-to-event regression ####
# survival regression 

View(lfs)
table(lfs$event) # 1050 died, 28 censor observations

addmargins(table(lfs$event))
prop.table(table(lfs$event))*100

summary(lfs$day)
table(lfs$trial)
table(lfs$genotype)
table(lfs$RU)

install.packages('survival')
library(survival)

# need to create time to event object (bring together time and event)

# surv object 

lsfo <- Surv(time= lfs$day, 
             event= lfs$event) # we use the default 'right', people censured included. 
?Surv
View(lsfo) # At the bottom of the we have some day values with a + means these are the one with event '0', the censured. 
class(lsfo)
summary(lsfo)

survfit(lsfo~1) # interpretation.  
plot(survfit(lsfo~1), conf.int = F) # removing confident intervals
abline(v=63)

plot(survfit(lsfo~genotype,data=lfs),
     mark.time=T,
     conf.int = F,
     lty=2:3)
legend('bottomleft',
       legend=levels(lfs$genotype),
       lty=2:3) # 1 start of study , 0 end of study, the + are the flies censured (living the study). x axis days needed for dieing 
survfit(lsfo~genotype,data=lfs) 
abline(v=49)

# interpretation, which group died sooner? 
# foxonull died sooner (graph), but also comparing the median in the table

# regression model 

m.cox.1 <- coxph(lsfo~genotype,
                 data=lfs)
summary(m.cox.1)
exp(coef(m.cox.1))
exp(confint(m.cox.1))

# coxph: proportional hazard, the risk of the 
# event, does not change with time
# otherwise non propotional hazard regression

# interpretation: the exp is included. S coeffi 0.1193 means 98.9% difference in comparison to Foxenall. 
# the exp - is a comparison the hazard, the risk of the event for the reference category is higher 8 times higher than Foxenall. 

cox.zph(m.cox.1)

# coxph: proportional hazard, the risk of the 
# event, does not change with time
# otherwise non propotional hazard regression

# predictions

m.cox.2 <- coxph(lsfo~genotype+RU,
                 data=lfs)

summary(m.cox.2)


