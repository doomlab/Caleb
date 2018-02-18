###This script created for PSY 527H Module 13: MLM###
####Set working directory and source file####
setwd("~/Desktop/Source")

master = read.csv("MLSA_MLM_Cosines.csv")

#install.packages("nlme")
#install.packages("reshape")
library(nlme)
library(reshape)

####Accuracy####
summary(master)
##This dataset is accurate##


####Outliers####
mahal = mahalanobis(master[, -3],
                    colMeans(master[, -3], na.rm = TRUE),
                    cov(master[, -3], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(master[, -3]))

summary(mahal < cutoff)
noout = master[mahal < cutoff,]


####Assumptions####
###Multicollinearity###
correlation = cor(master[, -3], use = "pairwise.complete.obs")
symnum(correlation)

###Set-up###
random = rchisq(nrow(master), 7)
fake = lm(random ~ ., data = master[,-3])
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

####Intercept only Linear Model####
model1 = gls(cosine ~ 1,
             data = master,
             method = "ML",
             na.action = "na.omit")
summary(model1)

####Random Intercept Model####
model2 = lme(cosine ~ 1,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~1|fem_partno)
summary(model2)

####Random Intercept with Predictor####
model3 = lme(cosine ~ prompt,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~1|fem_partno)
summary(model3)

####Random Intercept with Predictors####
model4 = lme(cosine ~ prompt,
             data = master,
             method = "ML",
             na.action = "na.omit",
             random = ~prompt|fem_partno,
             control = lmeControl(msMaxIter = 200))
summary(model4)

##Anova for comparison##
anova(model1, model2, model3, model4)

####Effect Size####
m1 = mean(master[2:551, 4])
m2 = mean(master[552:1100, 4])
sd1 = sd(master[2:551, 4])
sd2 = sd(master[552:1100, 4])
sdmean = mean(sd1,sd2)
dvale = (m1 - m2)/sdmean

