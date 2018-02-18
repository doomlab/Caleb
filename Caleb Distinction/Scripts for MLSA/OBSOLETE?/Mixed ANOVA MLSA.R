##This script created for PSY 527H Assig. 7: Two Way Mixed ANOVA
####Libraries Source Files####
setwd("~/Desktop/Source")
library(ez)
library(ggplot2)
library(reshape)

hobfem = read.csv("hobfem individual pearson.csv")
hobmal = read.csv("hobmal individual pearson.csv")
romfem = read.csv("romfem individual pearson.csv")
rommal = read.csv("rommal individual pearson.csv")

View(hobfem)
####Means####
mhobfem = apply(hobfem[,-1], 1, mean)
mhobmal = apply(hobmal[,-1], 1, mean)
mromfem = apply(romfem[,-1], 1, mean)
mrommal = apply(rommal[,-1], 1, mean)

overall = matrix(NA, nrow = (length(mhobfem)+length(mhobmal))*2, ncol = 4)
colnames(overall) = c("partno", "gender", "prompt", "cosine")
overall = as.data.frame(overall)
overall$partno = c(1:(length(mhobfem)+length(mhobmal)), 1:(length(mhobfem)+length(mhobmal)))
overall$gender = c(rep("male", length(mhobmal)), rep("female", length(mhobfem)),
                   rep("male", length(mhobmal)), rep("female", length(mhobfem)))
overall$prompt = c(rep("hobby", length(mhobmal)+length(mhobfem)),
                   rep("romantic", length(mhobmal)+ length(mhobfem)))
overall$cosine = c(mhobmal, mhobfem, mrommal, mromfem)


View(overall)
##Export##
#write.csv(x = mhobfem, file = "romfem individual mean.csv")
#write.csv(x = mhobmal, file = "hobfem individual mean.csv")
#write.csv(x = mromfem, file = "rommal individual mean.csv")
#write.csv(x = mrommal, file = "hobmal individual mean.csv")

####Data Screening####
View(overall)
outliers = overall[, -c(1:3)]
##The lovely Ms. Mahalanobis
mahal = mahalanobis(outliers,
                    colMeans(outliers, na.rm = T),
                    cov(outliers, use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(outliers))
cutoff
ncol(outliers)
summary(mahal < cutoff)
noout = subset(outliers, mahal < cutoff)

final = statsgenocide

####Assumptions####
###Set-up###
random = rchisq(nrow(final[, -1]), 7)
fake = lm(random ~ ., data = final)

standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

###Additivity###
correl = cor(final[, -1], use = "pairwise.complete.obs")
symnum(correl)
correl
###Normality###
hist(standardized)
###Linearity###
qqnorm(standardized)
abline(0,1)
###Homogeneity###
plot(fitted, standardized)
abline(0,0)
abline(v = 0)


####ANOVA Time###
##MELTMELTMELTMELTMELTMELTMELTMELTHAILSATANMELTMELTMELTMELTMELTMELTMELTILIKEBOYZMELTMELT##
names(final)
final$partno= 1:nrow(final)
longdata = melt(final,
                id = c("?..group", "partno"),
                measured = c("L_H", "L_M", "L_L"))

colnames(longdata) = c("group", "participant", "method", "score")
names(longdata)

##For real ANOVA time##
options(scipen = 999)
ezANOVA(data = overall,
        dv = cosine,
        within = prompt ,
        between = gender,
        wid = partno,
        type = 3)

##Levene's##
ezANOVA(data = longdata,
        dv = score,
        between = group,
        within = method,
        wid = participant,
        type = 3)

##Numbers##
sd(final$L_L)
with(longdata, tapply(score, list(method), mean))
with(longdata, tapply(score, list(method), sd))


####Post-Hocs####
##tapply##
#Means#
tapply(low$score, list(low$group), mean)
tapply(mid$score, list(mid$group), mean)
tapply(high$score, list(high$group), mean)
#SD's#
tapply(low$score, list(low$group), sd)
tapply(mid$score, list(mid$group), sd)
tapply(high$score, list(high$group), sd)
#Sample Size#
tapply(low$score, list(low$group), length)
tapply(mid$score, list(mid$group), length)
tapply(high$score, list(high$group), length)
##Splitting up the dataset##
table(master$?..group)

low = subset(longdata, method == "L_L")
mid = subset(longdata, method == "L_M")
high = subset(longdata, method =="L_H")
###Independent t-test##
with(low, t.test(score ~ group, 
                 paired = F,
                 var.equal = T,
                 p.adjust.methods = "bonferroni"))




with(mid, t.test(score ~ group, 
                 paired = F,
                 var.equal = T,
                 p.adjust.methods = "bonferroni"))

with(high, t.test(score ~ group, 
                  paired = F,
                  var.equal = T),
     p.adjust.methods = "bonferroni")

##Effect Size##
d.indt(m1 = .24, m2 = .20, sd1 = .13, sd2 = .13, n1 = 70, n2 = 59, a = .05, k = 2)
d.indt(m1 = .28, m2 = .28, sd1 = .17, sd2 = .16, n1 = 70, n2 = 59, a = .05, k = 2)
d.indt(m1 = .42, m2 = .34, sd1 = .29, sd2 = .30, n1 = 70, n2 = 59, a = .05, k = 2)

####Bar Graph for ANOVA####
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(color = "black"),
              axis.line.y = element_line(color = "black"),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

summary(final)
bargraph = ggplot(longdata, aes(method, score, fill = group))
bargraph +
  theme +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = position_dodge(width = .90)) +
  xlab("Backward Strength") +
  ylab("Score") +
  scale_fill_manual(name = "Numbers",
                    labels = c("With Numbers", "Without Numbers"),
                    values = c("Light Green", "Dark Orange")) +
  scale_x_discrete(labels = c("High", "Medium", "Low"))

