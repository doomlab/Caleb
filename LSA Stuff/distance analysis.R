setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Caleb/LSA Stuff")

master = read.csv("cos_distance.csv")
master$distance = master$second - master$first

output = lm(cosine ~ distance, 
            data = master)
summary(output)

with(master, cor(cosine, distance))
##plot(output)

####outliers####



##get the linearity plot
##create the standardized residuals
standardized = rstudent(output)
qqnorm(standardized)
abline(0,1)

##multivariate normality
hist(standardized, breaks=15)

##homogeneity and homoscedaticity
fitvalues = scale(output$fitted.values)
plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)

library(nlme)
####intercept only model####
model1 = gls(cosine ~ 1, 
             data = master, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

####random intercept only model####
model2 = lme(cosine ~ 1, 
             data = master, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|first)
summary(model2)

anova(model1, model2)

####predictor models####
model3 = lme(cosine ~ distance, 
              data = master, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|first)
summary(model3)

##test some residuals here
##Looks the same, so nesting = eh
##tempstuff = as.data.frame(model3$residuals)
##tempstuff2 = as.data.frame(model3$fitted)
##tempstuff$fixed = scale(tempstuff$fixed)
##tempstuff2$fixed = scale(tempstuff2$fixed)
##plot(tempstuff2$fixed, tempstuff$fixed)

anova(model1, model2, model3)

with(master, plot(distance, cosine))

library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 25))

master$groupauthor = as.factor(master$groupauthor)

scatter = ggplot(master, aes(distance, cosine, color = groupauthor))
scatter +
  geom_point() +
  xlab("Distance") +
  ylab("Cosine") +
  cleanup + 
  scale_fill_manual(name = "Author",
                    labels = c("1", "2", "3"),
                    values = c("red", "blue", "green")) +
  scale_color_manual(name = "Author",
                    labels = c("1", "2", "3"),
                    values = c("red", "blue", "green"))


####interesting idea####
library(cocor)

with(master, table(groupauthor, secondauthor))
master$patterns = paste(master$groupauthor, master$secondauthor)
master$patterns2 = paste(master$grouptheme, master$secondtheme)
sapply(split(master, master$patterns), function(X) cor(X$distance, X$cosine))
sapply(split(master, master$patterns2), function(X) cor(X$distance, X$cosine))

library(sm)
hm <- hcv(master$distance, master$cosine, display="lines")
sm.regression(master$distance, master$cosine, 
              h=hm, xlab="Distance", ylab="Cosine",
              group = master$patterns)


##this is just notes if we wanted to test those correlations against each other
##independent correlations
new = subset(liardata, Novice == "First Time")
old = subset(liardata, Novice == "Had entered Competition Before")
inddata = list(new, old)
cocor(~Creativity + Position | Creativity + Position,
      data = inddata)

##dependent correlations
cocor(~Revise + Exam | Revise + Anxiety, 
      data = examdata)