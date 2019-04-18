#####Author's Note#####
###Inferential Analysis of MLSA###
###Created 02/18 by Caleb Marshall (marshall628@live.missouristate.edu)###

####Set working directory and source file####

setwd("~/Desktop/Caleb/Caleb Distinction")

master = read.csv("../Analysis Final MLSA.csv")

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

psych::describe(noout$cosine)
# psych::describe(noout$openness)
# psych::describe(noout$extraversion)
# psych::describe(noout$agreeableness)
# psych::describe(noout$conscientiousness)
# psych::describe(noout$emotional_stability)

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

ope_model12 = lme(cosine ~ scale(openness),
                 data = master,
                 method = "ML",
                 na.action = "na.omit",
                 random = ~1|male_partno)
summary(ope_model12)

##Anova for comparison##
anova(model1, model2, ope_model1)

####Extraversion####
###Random Intercept with Predictor###
ext_model1 = lme(cosine ~ extraversion,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~1|male_partno)
summary(ext_model1)

with(master, plot(extraversion, cosine))

ext_model12 = lme(cosine ~ scale(extraversion),
                 data = master,
                 method = "ML",
                 na.action = "na.omit",
                 random = ~1|male_partno)
summary(ext_model12)

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

agrb_model12 = lme(cosine ~ scale(agreeableness),
                  data = master,
                  method = "ML",
                  na.action = "na.omit",
                  random = ~1|male_partno)
summary(agrb_model12)

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

conscs_model12 = lme(cosine ~ scale(conscientiousness),
                    data = master,
                    method = "ML",
                    na.action = "na.omit",
                    random = ~1|male_partno)
summary(conscs_model12)

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

emo_model12 = lme(cosine ~ scale(emotional_stability),
                 data = master,
                 method = "ML",
                 na.action = "na.omit",
                 random = ~1|male_partno)
summary(emo_model12)

###Model ANOVA###
anova(model1, model2, emo_model1)
