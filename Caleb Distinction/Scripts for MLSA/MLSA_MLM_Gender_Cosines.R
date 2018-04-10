#####Author's Note#####
###Inferential Analysis of MLSA###
###Created 02/18 by Caleb Marshall (marshall628@live.missouristate.edu)###

####Set working directory and source file####
setwd("~/Desktop/Lab Work/Caleb/Caleb Distinction")

master = read.csv("Analysis Final MLSA.csv")

#install.packages("nlme")
#install.packages("reshape")
library(nlme)
library(reshape)

####Accuracy####
summary(master)
##This dataset is accurate##


####Outliers####
mahal = mahalanobis(master[, -c(1:4)],
                    colMeans(master[, -c(1:4)], na.rm = TRUE),
                    cov(master[, -c(1:4)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master[, -c(1:4)]))

summary(mahal < cutoff)
noout = master[mahal < cutoff,]


####Assumptions####
###Multicollinearity###
correlation = cor(master[, -c(1:4)], use = "pairwise.complete.obs")
symnum(correlation)

###Set-up###
random = rchisq(nrow(master), 7)
fake = lm(random ~ ., data = master[,-c(1:4)])
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

###Normality###
hist(standardized)

###Linearity###
qqnorm(standardized)
abline(0,1)

###Heteroz vs. Homoz###
plot(fitted, standardized)
abline(0,0)
abline(v = 0)

######Analysis######
####Initial Models####
###Intercept only Linear Model###
model1 = gls(cosine ~ 1,
             data = master,
             method = "ML",
             na.action = "na.omit")
summary(model1)

###Random Intercept Model###
model2 = lme(cosine ~ 1,
          data = master,
          method = "ML",
          na.action = "na.omit",
          random = ~1|male_partno)
summary(model2)

####Openness####
###Random Intercept with Predictor###
ope_model1 = lme(cosine ~ openness,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~1|male_partno)
summary(ope_model1)

ope_model1 = lme(cosine ~ openness,
                 data = master,
                 method = "ML",
                 na.action = "na.omit",
                 random = ~1|male_partno)
summary(ope_model1)

###Random Intercept with Predictors###
ope_model2 = lme(cosine ~ openness,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~|fem_partno,
             control = lmeControl(msMaxIter = 200))
summary(ope_model2)

##Anova for comparison##
anova(model1, model2, ope_model1)

###Effect Size###
m1 = mean(master[2:551, 4])
m2 = mean(master[552:1100, 4])
sd1 = sd(master[2:551, 4])
sd2 = sd(master[552:1100, 4])
sdmean = mean(sd1,sd2)
dvale = (m1 - m2)/sdmean



####Extraversion####
###Random Intercept with Predictor###
ext_model1 = lme(cosine ~ extraversion,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~1|male_partno)
summary(ext_model1)

###Random Intercept with Predictors###
ext_model2 = lme(cosine ~ extraversion,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~male_partno|fem_partno,
             control = lmeControl(msMaxIter = 200))
summary(ext_model2)
###Model ANOVA###
anova(model1, model2, ext_model1)

####Agreeableness####
options(scipen = 999)
###Random Intercept with Predictor###
agrb_model1 = lme(cosine ~ agreeableness,
                  data = master,
                  method = "ML",
                  na.action = "na.omit",
                  random = ~1|male_partno)
summary(agrb_model1)

###Random Intercept with Predictors###
agrb_model2 = lme(cosine ~ agreeableness,
                 data = master,
                 method = "ML",
                 na.action = "na.omit",
                 random = ~male_partno|fem_partno,
                 control = lmeControl(msMaxIter = 200))
summary(agrb_model2)

###Model ANOVA###
anova(model1, model2, agrb_model1)

####Conscientiousness####
###Random Intercept with Predictor###
conscs_model1 = lme(cosine ~ conscientiousness,
                  data = master,
                  method = "ML",
                  na.action = "na.omit",
                  random = ~1|male_partno)
summary(conscs_model1)

###Random Intercept with Predictors###
conscs_model2 = lme(cosine ~ conscientiousness,
                  data = master,
                  method = "ML",
                  na.action = "na.omit",
                  random = ~male_partno|fem_partno,
                  control = lmeControl(msMaxIter = 200))
summary(conscs_model2)

###Model ANOVA###
anova(model1, model2, conscs_model1)

####Emotional-Stability####
###Random Intercept with Predictor###
emo_model1 = lme(cosine ~ emotional_stability,
                    data = master,
                    method = "ML",
                    na.action = "na.omit",
                    random = ~1|male_partno)
summary(emo_model1)

###Random Intercept with Predictors###
emo_model2 = lme(cosine ~ emotional_stability,
                    data = master,
                    method = "ML",
                    na.action = "na.omit",
                    random = ~male_partno|fem_partno,
                    control = lmeControl(msMaxIter = 200))
summary(emo_model2)

###Model ANOVA###
anova(model1, model2, emo_model1)

####Final Info####
summary(ope_model1)
summary(ext_model1)
summary(agrb_model1)
summary(conscs_model1)
summary(emo_model1)
