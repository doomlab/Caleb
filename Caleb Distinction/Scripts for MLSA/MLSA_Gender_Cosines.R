###Script written for Cosines for LSA of Mate Selection###
#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
library("ggplot2")
#Set Pathname
pathname1 = "~/Desktop/Mate LSA Band 1/romantic"
pathname2 = "~/Desktop/Mate LSA Band 1/hobbies"

# read files into a document-term matrix
myMatrix1 = textmatrix(pathname1, 
                       minWordLength = 1, 
                       stopwords = stopwords_en)
myMatrix1 = lw_logtf(myMatrix1) * gw_idf(myMatrix1)

myMatrix2 = textmatrix(pathname2, 
                       minWordLength = 1, 
                       stopwords = stopwords_en)
myMatrix2 = lw_logtf(myMatrix2) * gw_idf(myMatrix2)

# create the latent semantic space
myLSAspace1 = lsa(myMatrix1, dims=dimcalc_share())
myLSAspace2 = lsa(myMatrix2, dims=dimcalc_share())

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix1 = as.textmatrix(myLSAspace1)
myNewMatrix2 = as.textmatrix(myLSAspace2)

myNewMatrix1
myNewMatrix2
#NewMatrixtable = table(myNewMatrix)
#View(NewMatrixtable)

# compare two terms with the cosine measure
#Working towards terms to use here:
#cosine(myNewMatrix["perfect",], myNewMatrix["walk",])
cosinematrix1 = cosine(myNewMatrix1)
cosinematrix2 = cosine(myNewMatrix2)
romanticosine = cosinematrix1
hobbicosine = cosinematrix2
####Open to View Cosine Stuffs####
View(cosinematrix1)
View(cosinematrix2)
#export
write.csv(x = romanticosine, file = "romantic gender cosines.csv")
write.csv(x = hobbicosine, file = "hobbies gender cosines.csv")



