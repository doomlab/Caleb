#Load Libraries
library("tm")
library("SnowballC")
library("lsa")
library("ggplot2")
library("xlsx")
#Set Pathname
pathname1 = "~/Desktop/Mate LSA Band 1/female/fem1"
pathname2 = "~/Desktop/Mate LSA Band 1/female/fem2"
pathname3 = "~/Desktop/Mate LSA Band 1/male/male1"
pathname4 = "~/Desktop/Mate LSA Band 1/male/male2"

# read files into a document-term matrix
myMatrix1 = textmatrix(pathname1, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix1 = lw_logtf(myMatrix1) * gw_idf(myMatrix1)

myMatrix2 = textmatrix(pathname2, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix2 = lw_logtf(myMatrix2) * gw_idf(myMatrix2)

myMatrix3 = textmatrix(pathname3, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix3 = lw_logtf(myMatrix3) * gw_idf(myMatrix3)

myMatrix4 = textmatrix(pathname4, 
                      minWordLength = 1, 
                      stopwords = stopwords_en)
myMatrix4 = lw_logtf(myMatrix4) * gw_idf(myMatrix4)
# create the latent semantic space
myLSAspace1 = lsa(myMatrix1, dims=dimcalc_share())
myLSAspace2 = lsa(myMatrix2, dims=dimcalc_share())
myLSAspace3 = lsa(myMatrix3, dims=dimcalc_share())
myLSAspace4 = lsa(myMatrix4, dims=dimcalc_share())

#Convert LSA to Text-Frequency Vector Matrix for Cosines
myNewMatrix1 = as.textmatrix(myLSAspace1)
myNewMatrix2 = as.textmatrix(myLSAspace2)
myNewMatrix3 = as.textmatrix(myLSAspace3)
myNewMatrix4 = as.textmatrix(myLSAspace4)

myNewMatrix1
myNewMatrix2
myNewMatrix3
myNewMatrix4

#NewMatrixtable = table(myNewMatrix)
#View(NewMatrixtable)

# compare two terms with the cosine measure
#Working towards terms to use here:
#cosine(myNewMatrix["perfect",], myNewMatrix["walk",])
cosinematrix1 = cosine(myNewMatrix1)
cosinematrix2 = cosine(myNewMatrix2)
cosinematrix3 = cosine(myNewMatrix3)
cosinematrix4 = cosine(myNewMatrix4)
####Open to View Cosine Stuffs####
View(cosinematrix1)
View(cosinematrix2)
View(cosinematrix3)
View(cosinematrix4)
####Correlation Time####
#Total Correlation for each writing condition
romfem = cor(cosinematrix1[,], cosinematrix1[,], method="pearson")
hobfem = cor(cosinematrix2[,], cosinematrix2[,], method="pearson")
rommal = cor(cosinematrix3[,], cosinematrix3[,], method="pearson")
hobmal = cor(cosinematrix4[,], cosinematrix4[,], method="pearson")
#export
write.xlsx(x = romfem, file = "romfem individual pearson.xlsx")
write.xlsx(x = hobfem, file = "hobfem individual pearson.xlsx")
write.xlsx(x = rommal, file = "rommal individual pearson.xlsx")
write.xlsx(x = hobmal, file = "hobmal individual pearson.xlsx")
####save correlation as variable####
matrixcorrelation = 
  cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")
#compare cosines of terms
cosine(myMatrix["mistress",], myNewMatrix["mistress",])

#Create CSV for 1_39 <=> 56_66
write.csv(cosinematrix, "Total_Cosine_Matrix.csv")

#Hypothesis Test Information
matrixcorrelation


