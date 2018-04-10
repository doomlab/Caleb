#Thisff script create for Mate Preference/LSA data reshaping#
##November 15, 2017/Coder: Caleb Z. Marshall (calebzmarshall@gmail.com)
setwd("~/Desktop")
master = read.csv("Overall Dataset MLSA.csv")

View(master)

####Finding Sums for Personality Measures####
###Reverse Scoring###
library(psych)
keys <- c(1,1,1,1,-1,1,-1,1,-1,1,
          1,1,-1,1,-1,1,1,1,-1,-1,
          1,-1,1,-1,1,1,1,-1,1,-1,
          -1,-1,1,1,1,1,1,-1,1,-1,
          -1,1,1,-1,-1,1,1,-1,-1,-1)
personality = master[, c(11:60)]
rev_personality <- reverse.code(keys,personality,mini=1,maxi=5)
View(rev_personality)
###Creating Total Personality Score###
master$openness = rowSums(rev_personality[, c(1,6,11,16,21,26,31,36,41,46)], na.rm = FALSE, dims = 1)
master$extraversion = rowSums(rev_personality[, c(2,7,12,17,22,27,32,37,42,47)], na.rm = FALSE, dims = 1)
master$agreeableness = rowSums(rev_personality[, c(3,8,13,18,23,28,33,38,43,48)], na.rm = FALSE, dims = 1)
master$conscientiousness = rowSums(rev_personality[, c(4,9,14,19,24,29,34,39,44,49)], na.rm = FALSE, dims = 1)
master$emo_stab = rowSums(rev_personality[, c(5,10,15,20,25,30,35,40,45,50)], na.rm = FALSE, dims = 1)

final = master[, -c(1,4:60)]

#write.csv(x = final, file = "MLSA Total Personality.csv")
View(final)
table(final$gender)
####Means and SD's for Personality####
#Openness#
tapply(final$openness, list(final$gender), mean)
tapply(final$openness, list(final$gender), sd)
d.indt(m1 = 36.88679, m2 = 36.83, sd1 = 6.05, sd2 = 6.27, n1 = 53, n2 = 52, a = .05, k = 2)
#Extraversion#
tapply(final$extraversion, list(final$gender), mean)
tapply(final$extraversion, list(final$gender), sd)
d.indt(m1 = 37.15, m2 = 39.60, sd1 = 7.63, sd2 = 6.96, n1 = 53, n2 = 52, a = .05, k = 2)
#Agreeableness#
tapply(final$agreeableness, list(final$gender), mean)
tapply(final$agreeableness, list(final$gender), sd)
d.indt(m1 = 34.91, m2 = 37.65, sd1 = 6.03, sd2 = 7.09, n1 = 53, n2 = 52, a = .05, k = 2)
#Conscientiousness
tapply(final$conscientiousness, list(final$gender), mean)
tapply(final$conscientiousness, list(final$gender), sd)
d.indt(m1 = 34.64, m2 = 37.65, sd1 = 6.03, sd2 = 7.09, n1 = 53, n2 = 52, a = .05, k = 2)
#Emotional Stability#
tapply(final$emo_stab, list(final$gender), mean)
tapply(final$emo_stab, list(final$gender), sd)
d.indt(m1 = 32, m2 = 26.61, sd1 = 7.94, sd2 = 7.51, n1 = 53, n2 = 52, a = .05, k = 2)
