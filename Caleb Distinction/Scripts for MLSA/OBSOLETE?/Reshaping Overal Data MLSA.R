#Thisff script create for Mate Preference/LSA data reshaping#
##November 15, 2017/Coder: Caleb Z. Marshall (calebzmarshall@gmail.com)
setwd("~/Desktop/Source")
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

write.csv(x = final, file = "MLSA Total Personality.csv")
