cat("\014")
##1
load('HW5_BostonHousing.RDAT')
#a
Boston$chas<-NULL
#b
Boston_sample_covariance<-cov(Boston)
#c
Boston_eigen<-eigen(Boston_sample_covariance)
Boston_eigenvalues<-Boston_eigen$values
Boston_eigenvectors<-Boston_eigen$vectors
remove(Boston_eigen)
#d
Lambda<-diag(Boston_eigenvalues)
v_lambda_v<-Boston_eigenvectors%*%Lambda%*%t(Boston_eigenvectors)
print(Boston_sample_covariance - v_lambda_v)
remove(Lambda,v_lambda_v)
#e
pca<-princomp(Boston,cor = T)
#f
Boston_pca_variance<-(pca$sdev)^2
#g
pca.V<-pca$loadings
print(pca.V)
pca.L <- pca$sdev^2
{
  p1 <- sapply(1:13,function(u) pca.L[u]/sum(pca.L))
  p2 <- sapply(1:13,function(u) sum(pca.L[1:u])/sum(pca.L))
  data.frame(Proportion_of_Total_Variance=p1,Percentage_Explained=p2)
}


plot(Boston_pca_variance,ylab='Eigenvalues',main='Eigenvalues of Sample Covariance Matrix S')
#h
plot(pca$scores[,1],pca$scores[,2],xlab='Tax',ylab='Race',main='Taxes vs Race')
#i
biplot(pca,choices=c(1,2),col=c('gray25','red'),cex=.75,pc.biplot = T)
#The two biggest eigenvalues were tax and race
#All the others are insignificant in relation

##2
cat("\014")
library("readxl")
hw5_wiki<-read_excel('HW5_wiki.xlsx')
#a
hw5<-hw5_wiki[complete.cases(hw5_wiki),]
#b
f <- factanal(hw5,factors=6)
#c
print(f$loadings, digits=3, cutoff=0.5, sort=FALSE)
#d
print(f$uniquenesses)
#e
library(psych)
f_psych <- fa(hw5,nfactors=6,covar=T,fm='pa')
factor2cluster(f_psych,aslist = TRUE)
#Factors 1 and 4 are the most significant
#Factor 5 is the least significant

#3
banknotes<-read.csv('HW5_banknotes.csv')
banknotes$X<-NULL
#a
km<-kmeans(banknotes, centers = 2)
#b
rownames(banknotes[which(km$cluster==1),])
rownames(banknotes[which(km$cluster==2),])
#c
print(km$cluster[11])
genuine<-km$cluster[51]
print(counterfeit<-rownames(banknotes[which(km$cluster != genuine),]))
