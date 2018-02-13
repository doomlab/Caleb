#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
library("ggplot2")

#Set Pathname
pathname1 = "C:/Users/Caleb/Desktop/Fall 2016/Lab Work/Isaiah Source Texts/Work Folder/A1"
pathname2 = "C:/Users/Caleb/Desktop/Fall 2016/Lab Work/Isaiah Source Texts/Work Folder/B1"

# read 1-39 files into a document-term matrix
myMatrix1 = textmatrix(pathname1, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix1 = lw_logtf(myMatrix) * gw_idf(myMatrix)

#Read 40-55 files into a document-term matrix
myMatrix2 = textmatrix(pathname2, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix2 = lw_logtf(myMatrix) * gw_idf(myMatrix)

# create the latent semantic space
myLSAspace1 = lsa(myMatrix1, dims=dimcalc_share())
myLSAspace2 = lsa(myMatrix2, dims=dimcalc_share())

# display it as a textmatrix again for viewing
checkvalues = table(round(as.textmatrix(myLSAspace),2)) # should give the original

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix1 = as.textmatrix(myLSAspace1)
myNewMatrix2 = as.textmatrix(myLSAspace2)

myNewMatrix1
myNewMatrix2
#NewMatrixtable = table(myNewMatrix)
#View(NewMatrixtable)

#compare cosines of terms
cosine(myNewMatrix1["earth",], myNewMatrix1["mistress",])
cosine(myNewMatrix2["earth",], myNewMatrix2["mistress",])

cosine(myNewMatrix1["earth",], myNewMatrix1["name",])
cosine(myNewMatrix2["earth",], myNewMatrix2["name",])

cosine(myNewMatrix1["wicked",], myNewMatrix1["cry",])
cosine(myNewMatrix2["wicked",], myNewMatrix2["cry",])

#compare overall cosines
cosinematrix1 = cosine(myNewMatrix1)
cosinematrix2 = cosine(myNewMatrix2)
