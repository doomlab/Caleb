####Author's Note####
###Script written for Cosines for MLSA###
###This is the second version of this script###
###Created by Caleb Z. Marshall, 02/18###

####Gender Cosines for MLSA####
#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
#Set Pathname
pathname = "~/Desktop/Lab Work/Caleb/Mate LSA Band 2/WORK"

# read files into a document-term matrix
myMatrix = textmatrix(pathname, 
                       minWordLength = 1, 
                       stopwords = stopwords_en)
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)

# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_share())

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix = as.textmatrix(myLSAspace)

myNewMatrix

#Calculate Cosine
cosinematrix = cosine(myNewMatrix)
romanticosine = cosinematrix

####Exporting Cosines in Excel####
View(cosinematrix)
#export
write.csv(x = romanticosine, file = "romantic gender cosines.csv")

####Reshaping Mixed Cosines####
#Dataset
setwd("~/Desktop/Lab Work/Caleb/Caleb Distinction")
mixcosines = read.csv("Mixed_Cosines.csv")
#Libraries
library(reshape)

#Reshape Data with Males as the ID
View(mixcosines)
longdata = melt(mixcosines,
                id = "X",
                measured = mixcosines[,2:46])
View(longdata)
colnames(longdata) = c("Male", "Female", "Cosine")

#Export Final Dataset for Cosines
write.csv(x = longdata, file = "Mix Gender Cosines Reshape MLSA.csv")




