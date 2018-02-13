##Trying to fuck around with programming LSA
##Hail Mary, Mother, Full of Grace, Protect us in our Hour of Need
##Libraries needed for this script: "tm" and dependencies, "slam", "SnowballC", "lsa" and "ggplot2"

#Working Dir.
#setwd("C:/Users/Caleb/Desktop/Fall 2016/Lab Work")
#pathname = "C:/Users/Caleb/Desktop/Fall 2016/Lab Work"
  
##drb directory
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Caleb/LSA Stuff/Isaiah PlainText Files")
pathname = "~/OneDrive - Missouri State University/RESEARCH/2 projects/Caleb/LSA Stuff/Isaiah PlainText Files"
#pathname = "~/OneDrive - Missouri State University/RESEARCH/2 projects/Caleb/LSA Stuff/Isaiah combo"

#Preparing package "tm"
#install.packages("tm",dependencies=TRUE)

#Load Libraries
library(tm)
library(lsa)

# read files into a document-term matrix
myMatrix = textmatrix(pathname, 
                      minWordLength = 1, 
                      stopwords = stopwords_en, 
                      stemming = TRUE)
myMatrix = lw_logtf(myMatrix) * gw_idf(myMatrix)
myMatrix

# create the latent semantic space
myLSAspace = lsa(myMatrix, dims=dimcalc_share())

# display it as a textmatrix again for viewing
round(as.textmatrix(myLSAspace),2) # should give the original

# display it as a textmatrix again
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix # should look be different!

# compare two terms with the cosine measure
cosine(myNewMatrix["lord",], myNewMatrix["ruler",])
saved = cosine(myNewMatrix)
saved2 = cor(myNewMatrix)
# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")



