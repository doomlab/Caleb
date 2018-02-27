####Author' Note####
###MLSA reshape of personality data
###Created on 02/18
###Coder: Caleb Z. Marshall (marshall628@live.missouristate.edu)

####Global Set-Up####
setwd("~/Desktop/Lab Work/Caleb/Caleb Distinction")

####Importing Dataset to Build Personality####
total_data = read.csv("MLSA_MASTER.csv")

View(total_data)

####Reverse Scoring####
library(psych)
keys <- c(1,1,1,1,-1,1,-1,1,-1,1,
          1,1,-1,1,-1,1,1,1,-1,-1,
          1,-1,1,-1,1,1,1,-1,1,-1,
          -1,-1,1,1,1,1,1,-1,1,-1,
          -1,1,1,-1,-1,1,1,-1,-1,-1)
personality = total_data[, c(5:54)]
rev_personality <- reverse.code(keys,personality,mini=1,maxi=5)
View(rev_personality)

####Creating Total Personality Score####
total_data$openness = rowSums(rev_personality[, c(1,6,11,16,21,26,31,36,41,46)], na.rm = FALSE, dims = 1)
total_data$extraversion = rowSums(rev_personality[, c(2,7,12,17,22,27,32,37,42,47)], na.rm = FALSE, dims = 1)
total_data$agreeableness = rowSums(rev_personality[, c(3,8,13,18,23,28,33,38,43,48)], na.rm = FALSE, dims = 1)
total_data$conscientiousness = rowSums(rev_personality[, c(4,9,14,19,24,29,34,39,44,49)], na.rm = FALSE, dims = 1)
total_data$emo_stab = rowSums(rev_personality[, c(5,10,15,20,25,30,35,40,45,50)], na.rm = FALSE, dims = 1)

####Individual Personality Dataset####
final_personality = total_data[-c(8,26,28,44,48,53,54,59,64,67,75,85,95,100,102), -c(5:55)]
write.csv(x = final_personality, file = "MLSA Total Personality Band 2.csv")

####Building Analysis File (Personality Difference Scores)####
master = read.csv("ALMOST Analysis MLSA.csv")
View(master)

####Creating Absolute Difference Scores####
master$openness = abs(master$male_openness - master$fem_openness)
master$extraversion = abs(master$male_extraversion - master$fem_extraversion)
master$agreeableness = abs(master$male_agreeableness - master$fem_agreeableness)
master$conscientiousness = abs(master$male_conscientiousness - master$fem_conscientiousness)
master$emotional_stability = abs(master$male_emo_stab - master$fem_emo_stab)

####Isolating Dependent Variable (Personality Difference Scores)####
analysis_final = master[,-c(5:14)]
####Exporting Final Data Set####
write.csv(x = analysis_final, file = "Analysis Final MLSA.csv")
