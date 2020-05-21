
# Math 4090 Notes

#banknote <- readxl::read_excel('./BankNotes.xlsx')
#save(banknote,file='./BankNote.RDAT')

x=2
y=4
x*y-sqrt(x)
x <- 2

# vector
x <- c(2,-1,3,3,4,6,6)
x[1:3]
x[4:8]
x[x<0]
x[x==6]

# matrix
x <- 1:15
A <- matrix(x,nrow=3)
A[2,3]
A[,3]
A[2:3,3:4]

B <- matrix(runif(15,min=2,max=4),ncol=5)
B

A+B
C <- t(A)
D <- A%*%C
C%*%A
solve(D)
E <- solve(B%*%t(B))

B

# data frame
load('BankNote.RDAT')
View(banknote)
names(banknote)
banknote$Length
banknote$Top
banknote[1:3,]
banknote[,2]

# Example of normal samples
x <- rnorm(10000,2,3)
mean(x)
var(x)
sd(x)
summary(x)
boxplot(x)
hist(x)
hist(x,breaks=50)
hist(x,breaks = 100)
hist(x,breaks=50,probability = T)
hist(x,breaks=100,probability = T)
lines(density(x),col='red')

#Example of non-normal samples
u <- rnorm(10000,2,1)
x <- u^2 + 3*u + 10
summary(x)
boxplot(x)
hist(x)
hist(x,breaks = 100)
hist(x,breaks=400,probability = T)
hist(x,breaks=100,probability = T)
lines(density(x),col='red')

{ # sampling in a 2-D unit square
  n <- 10000
  x1 <- runif(n,0,1)
  x2 <- runif(n,0,1)
  plot(x1,x2,cex=.2)
  mean(x1<.6 & x2<.5)
}

{ # Sampling uniform distribution in an unit square
p <- 2
n <- 10000
b <- vapply(1:n,function(u) max(abs(runif(p,0,1)))<0.99,c(0))
cat('Prob a sample point is from [0,1]^p =', mean(b))
}

{ #percentage of points have distance < 0.99
  c <- array()
  n <- 10000
  k <- 0:11
  for(i in k){
    c[i+1] <- mean(vapply(1:n,function(u) max(runif(2^i,0,1))<0.99,c(0)))
  }
  print(cbind(p=2^k,c))
  plot(2^k,c,type='l')
}

{ #length of vectors
  p <- 2^11
  n <- 10000
  d <- vapply(1:n,function(u) sqrt(sum(runif(p,-1,1)^2)),c(0))
  hist(d,breaks=200,main='Histogram of Distrances from the Origin')
  boxplot(d)
  summary(d)
}

# Swiss bank data: multivariate distribution
load('BankNote.RDAT')
View(banknote)
names(banknote)
real_note <- banknote[banknote$Y==0,]
fake_note <- banknote[banknote$Y==1,]
summary(real_note)
summary(fake_note)
boxplot(real_note[,2],fake_note[,2])
hist(real_note[,2])
hist(fake_note[,2])
plot(density(fake_note[,2]),col='red')
lines(density(real_note[,2]),col='blue')

pairs(banknote[,1:6])
r <- cor(banknote[,1:6])
pairs(real_note[,1:6])
r0 <- cor(real_note[,1:6])
plot(real_note[,4],real_note[,5])
plot(fake_note[,4],fake_note[,5])
plot(banknote[,4],banknote[,5])

w <- 0.5*log((1+r[4,5])/(1-r[4,5]))
z <- sqrt(nrow(banknote-3))*w
p <- 2*(1-pnorm(abs(z),mean=0,sd=1))
p

w0 <- 0.5*log((1+r0[4,5])/(1-r0[4,5]))
z0 <- sqrt(nrow(banknote-3))*w0
p0 <- 2*(1-pnorm(abs(z0),mean=0,sd=1))
p0

X <- as.matrix(banknote[,1:6])
n <- nrow(X)
e <- rep(1,n)
H <- diag(1,n) - e%*%t(e)/n
S <- (t(X)%*%H%*%X)/(n-1)
D <- sqrt(diag(1/diag(S)))
R <- D%*%S%*%D

# Simple Linear Regression
y <- real_note$Bottom
x <- real_note$Top
plot(x,y)
fit1 <- lm(y~x)
summary(fit1)
abline(fit1,col='red')
beta_hat <- var(x,y)/var(x)
fit$coefficients

# Boston housing data
library(MASS)
?Boston
load('BostonHousing.RDAT')
fit <- lm(medv ~ .,data=Boston)
summary(fit)
beta_hat <- fit$coefficients
residuals <- fit$residuals
y_hat <- fit$fitted.values

plot(residuals)
boxplot(residuals)
qqnorm(residuals)

fit2 <- lm(medv~crim+zn+indus,data=Boston)
summary(fit2)
plot(fit2$residuals)
qqnorm(fit2$residuals)

summary(lm(medv ~ .-tax,data=Boston))

# Plot a bivariate normal density function
library(plot3D)
{
  x <- seq(-4,4,by=0.1)
  y <- seq(-4,4,by=0.1)
  grid <- mesh(x,y)
  
  # set parameter values
  a <- 1
  b <- 2
  c <- 0.7^2
  
  # calculate function values
  d <- 2*pi*sqrt(a*b*(1-c))
  z <- with(grid, exp(-((x-1)^2/a + (y+1)^2/b - 2*(x-1)*(y+1)/sqrt(a*b/c))/(2*(1-c)))/d)
  
  # 3-d plots
  ribbon3D(z=z,x=x,y=y,  along = "xy", space = 0.2,contour = T)
}
persp3D(z, x = x, y = y, scale = FALSE, contour = list(nlevels = 20, col = "red"))

# Hotelling T-squared Test
load('BankNote.RDAT')
{
  # set up values for null hypothesis
  mu0 <- c(215,130,130)
  # get data
  X <- banknote[1:15,2:4]
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  View(X)
  # calculate Hotelling test statistic
  xbar <- colMeans(X)
  S <- var(X)
    S0 <- t(X)%*%X/n - (xbar)%*%t(xbar) # formula from book
    S - S0*n/(n-1)
  t <- n*t(xbar-mu0)%*%solve(S)%*%(xbar-mu0)
  t0 <- (n-1)*t(xbar-mu0)%*%solve(S)%*%(xbar-mu0)
  # calculate p-value
  p_value <- 1-pf(t,p,n-p)
  cat(' p_value =',p_value,'\n','t_squared =',t)
  # show the distribution of the test statistics under null hypothesis
  hist(rf(10000,p,n-p),breaks=250)
}

# Check if a given mean vector is in the confidence region
mu <- c(215,130,130)
mu <- c(215.1,129.8,129.4)
mu <- c(214,129,129)
{
  n <- nrow(X)
  p <- ncol(X)
  cv <- qf(0.95,p,n-p)
  d <- t(colMeans(X)-mu)%*%solve(var(X))%*%(colMeans(X)-mu)
  cat(' critical value from f-distribution =',cv,'\n','distance from center =',d,'\n')
  cat(' Is mu1 in the confidence region:',(d<cv))
}

d <- apply(X, 1, function(mu) n*t(colMeans(X)-mu)%*%solve(var(X))%*%(colMeans(X)-mu))

library(Hotelling)
?hotelling.test
{
  # get data
  X <- banknote[banknote$Type=='real',2:4]
  Y <- banknote[banknote$Type=='fake',2:4]
  # calculate the p-value for the data set
  ht <- hotelling.test(X[1:25,],Y[1:25,])
  ht$stats
  ht$pval
}

{
  X <- banknote[1:10,2:7]
  Y <- banknote[101:110,2:7]
  ht <- hotelling.test(X,Y)
  cat(' test statistic =',ht$stats$statistic,'\n','p-value =',ht$pval)
}

# CVD19 Example
cvd19 <- as.matrix(readxl::read_excel('CVD19-30.xlsx'))
View(cvd19)
us_growth <- cvd19[,1]
plot(us_growth,type='l')
plot(log(us_growth+1),type='l')
us_daily_count <- us_growth[2:67] - us_growth[1:66]
barplot(us_daily_count)
plot(log(us_daily_count+1),type='l')

# estimating growth rate
x <- 40:length(us_growth)
y <- log(us_growth[x])

# use model us_growth = exp(b0 + b1*x)
fit <- lm(y~x)

summary(fit)
cat(' Estimated growth rate =',fit$coefficients[2])

plot(x,y,cex=1,col='blue')
abline(fit,col='red')

plot(x,exp(y))
lines(x,exp(predict(fit)),col='red')
exp(predict(fit,newdata = data.frame(x=c(67,68,69))))

load('BankNote.RDAT')

###################################################
###################################################

# Spectral Decomposition of Sample Covriance Matrix
{
  X <- banknote[1:200,2:7]
  X <- as.matrix(X)
  S <- var(X)
  E <- eigen(S)
  V <- E$vectors
  L <- E$values 
  S1 <- V%*%diag(L)%*%t(V)
  max(abs(S-S1))
  Y <- (X- rep(1,nrow(X))%*%t(colMeans(X)))%*%V
}

# PCA
pca <- princomp(X)
pca.V <- pca$loadings[1:ncol(X),1:ncol(X)]
pca.L <- pca$sdev^2
Y <- (X- rep(1,nrow(X))%*%t(colMeans(X)))%*%pc.V
summary(pca)
biplot(pca,choices=c(1,2),col=c('gray25','red'),cex=.75,pc.biplot = T)

# Display results

# eiginevalues
plot(pc.L,ylab='Eigenvalues',main='Eigenvalues of Sample Covariance Matrix S')
# PC1 vs PC2
plot(Y[,1],Y[,2],xlab='PC 1',ylab='PC 2',type='n')
text(Y[,1],Y[,2],c(rep('r',100),rep('f',100)))
plot(Y[,1],Y[,2],col=(banknote[,1]=='real')+2,xlab='PC 1',ylab='PC 2')
legend(-2.5,3,c('Real','Fake'),col=c(3,2),pch=c(1,1))
# PC1 vs PC3
plot(Y[,1],Y[,3],xlab='PC 1',ylab='PC 3')
plot(Y[,1],Y[,3],col=(banknote[,1]=='real')+2,xlab='PC 1',ylab='PC 2')
legend(-2.5,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1))
# PC2 vs PC3
plot(Y[,2],Y[,3],xlab='PC 2',ylab='PC 3')
plot(Y[,2],Y[,3],col=(banknote[,1]=='real')+2,xlab='PC 1',ylab='PC 2')
legend(-3,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1))

# Put all plots together
{
  par(mfrow=c(2,2))
  
  plot(Y[,1],Y[,2],col=(banknote[,1]=='real')+2,xlab='PC 1',ylab='PC 2',main='PC1 vs PC2')
  legend(-2.5,3,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  plot(Y[,1],Y[,3],col=(banknote[,1]=='real')+2,xlab='PC 1',ylab='PC 3',main='PC1 vs PC3')
  legend(-2.5,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)

  plot(Y[,2],Y[,3],col=(banknote[,1]=='real')+2,xlab='PC 2',ylab='PC 3',main='PC2 vs PC3')
  legend(-3,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  plot(pc.L, ylab='Eigenvalues',col='blue',main='Eigenvalues')
  
  par(mfrow=c(1,1))  
}

{
  par(mfrow=c(2,2))
  
  plot(X[,1],X[,2],col=(banknote[,1]=='real')+2,xlab='X1',ylab='X2',main='X1 vs X2')
  legend(-2.5,3,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  plot(X[,1],X[,3],col=(banknote[,1]=='real')+2,xlab='X1',ylab='X3',main='X1 vs X3')
  legend(-2.5,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  plot(X[,3],X[,4],col=(banknote[,1]=='real')+2,xlab='X',ylab='X3',main='X3 vs X4')
  legend(-3,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  plot(X[,4],X[,6],col=(banknote[,1]=='real')+2,xlab='X4',ylab='X6',main='X4 vs X6')
  legend(-3,1.4,c('Real','Fake'),col=c(3,2),pch=c(1,1),cex=.5)
  
  par(mfrow=c(1,1))  
}

# Proportion of variance explained by PCs
{
  p1 <- sapply(1:6,function(u) pc.L[u]/sum(pc.L))
  p2 <- sapply(1:6,function(u) sum(pc.L[1:u])/sum(pc.L))
  data.frame(Eigenvalues=pc.L,Proportion_of_Total_Variance=p1,Percentage_Explained=p2)
}

# PCA Example 1
load('BankNote.RDAT')
X <- banknote[,2:7]
summary(pca <- princomp(X),loadings = TRUE)
plot(pca$sdev,main='Standard Deviation of the PCs',ylab='Standard Deviations')
# Get PCs
Y <- pca$scores
# Plot pairwise PCs
pairs(Y[,1:3])
# show biplot
biplot(pca,choices=c(1,2),col=c('gray25','red'),cex=.75)

# PCA Example 2
load('BostonHousing.RDAT')
X <- Boston[,-c(4)]
summary(pca <- princomp(X,cor=T),loadings = T)
pairs(pca$scores[,1:5])
biplot(pca,choices=c(1,2),col=c('gray25','red'),cex=.75)

# PCA Example 3
load('heptathlon.RDAT')
X <- heptathlon[,-which(colnames(heptathlon)=='score')]
# data cleaning
{
  X$hurdles <- max(X$hurdles)-X$hurdles
  X$run200m <- max(X$run200m)-X$run200m
  X$run800m <- max(X$run800m)-X$run800m
  X <- X[-grep("PNG", rownames(heptathlon)),]
}
summary(pca <- princomp(X,cor=T),loadings=T)
biplot(pca,cex=.5,xlim=c(-.5,.7))


## factor analysis

load('WaisData.RDAT')
X <- wais[which(wais[,ncol(wais)]==0),-ncol(wais)]
(f <- factanal(X,factors=1))

S <- matrix(c(1,.83,.78,.83,1,.67,.78,.67,1),ncol=3)
rownames(S) <- colnames(S) <- c('Classics','French','English')
S
(f <- factanal(covmat=S,factors = 1))

load('heptathlon.RDAT')
# data preparation
X <- heptathlon[,-which(colnames(heptathlon)=='score')]
{
  X$hurdles <- max(X$hurdles)-X$hurdles
  X$run200m <- max(X$run200m)-X$run200m
  X$run800m <- max(X$run800m)-X$run800m
  X <- X[-grep("PNG", rownames(heptathlon)),]
}
# PCA
biplot(princomp(X,cor=T),choices=c(1,2),cex=.5,xlim=c(-.5,.7))
biplot(princomp(X,cor=T),choices=c(1,3),cex=.5,xlim=c(-.5,.7))
biplot(princomp(X,cor=T),choices=c(2,3),cex=.5,xlim=c(-.5,.7))
cor(X)
# Factor analysis
(f <- psych::fa(X,nfactors=3,fm='ml'))
#(f <- factanal(X,factors=3))
Q <- f$loadings
{ # set all values close to 0 in Q to 0
  for(i in 1:nrow(Q)){
    for(j in 1:ncol(Q)){
      if(abs(Q[i,j])<0.5) Q[i,j]=0
    }
  }
  Q
}

load('druguse_covmat.RDAT')
(S <- druguse)
(f <- factanal(covmat = S,factors=6))
Q <- f$loadings
{ # set all values close to 0 in Q to 0
  for(j in 1:ncol(Q)){
    for(i in 1:nrow(Q)){
      if(abs(Q[i,j])<0.35) Q[i,j]=0
    }
  }
  Q
}

{
  print(f <- psych::fa(druguse,nfactors=6,covar=T,fm='pa'))
  factor.plot(f)
}


## Cluster analysis

load('heptathlon.RDAT')
X <- heptathlon[,1:7]
{
  X$hurdles <- max(X$hurdles)-X$hurdles
  X$run200m <- max(X$run200m)-X$run200m
  X$run800m <- max(X$run800m)-X$run800m
}
dsm <- dist(X)
(hc <- hclust(dsm,method='complete'))
plot(hc,cex=.85)
plot(hclust(dsm,method='average'),cex=.85)

(km <- kmeans(X,centers=4,nstart=10))
# identify rows names in each cluster
rownames(X)
rownames(X)[which(km$cluster==1)]
rownames(X)[which(km$cluster==2)]
rownames(X)[which(km$cluster==3)]
rownames(X)[which(km$cluster==4)]

# identify rows in each cluster
X[which(km$cluster==1),]
X[which(km$cluster==2),]
X[which(km$cluster==3),]
X[which(km$cluster==4),]

y <- princomp(X,cor=T)$scores
plot(y[,1],y[,3],xlab='x',ylab='y',main='x vs y')
plot(y[,1],y[,3],col=(km$cluster+1),xlab='x',ylab='y',main='x vs y')

## an impletementation of k-means in R^2

# pick colors to lable clusters
col <- c("red2","green","blue","cyan4","magenta4","yellow","black",
          "gray","wheat","violet","lightblue1","tan4","turquoise","slateblue",
          "orange","maroon2","yellow4")

# uniform distribution data points
{ # run this for K=3,4 or 5 and large n for fun
  #set.seed(1)
  n <- 5000
  X <- matrix(runif(2*n,0,1),nrow=n)
}
# two sets of normal data points  
{
  #set.seed(1)
  n <- 2500
  X1 <- matrix(c(rnorm(n,1,1),rnorm(n,1,1)),nrow=n)
  X2 <- matrix(c(rnorm(n,-1,1),rnorm(n,-1,1)),nrow=n)
  X <- rbind(X1,X2)
}
plot(X,cex=.5)
K <- 17

# starting with random cluseter assignment
{
  plot(X,cex=.5)
  # generate a random cluster assignment
  C <- ceiling(runif(nrow(X),0,K))
  # show cluster assignment through coloring
  points(X,pch=21,col=col[C],cex=.5)
  # calculate centroids
  centers <- t(sapply(1:K, function(u) apply(X[C==u,],2,mean)))
  # add centroids to the graph
  points(centers,pch=17,col=col[1:K],cex=1.5)   
}

# starting with random centroids
{
  #set.seed(2)
  plot(X,cex=.5)
  #centers <- matrix(runif(2*K,min(X),max(X)),ncol=2)
  centers <- X[sample(1:nrow(X),K),]
  points(centers,pch=17,col=col[1:K],cex=1.5)  
}

for(i in 1:500){
  d <- sapply(1:K, function(v) (
    sapply(1:nrow(X),function(u) sum((X[u,]-centers[v,])^2))
    ))
  C1 <- apply(d,1,function(u) which(u==min(u)))
  cat('WCSS =',
      sum(sapply(1:nrow(d),function(u) length(C1==C1[u])*d[u,C1[u]]^2)),
    '\n')
  # if we clusters C and C1 are the same
  if(sum(C!=C1)>0) {
    plot(X,cex=.5)
    C <- C1  
    points(X,col=col[C],pch=21,cex=.5)
    centers <- t(sapply(1:K, function(u) apply(X[C==u,],2,mean)))
    points(centers,pch=17,col=col[1:K],cex=1.5)
  } else {
    cat("Done")
    break
  }
}
abline(v=.5)
abline(h=.5)
plot(X,cex=.5)

# how to read an excel file
wiki <- readxl::read_excel('HW5_wiki.xlsx')
wiki <- na.omit(wiki)

# how to read a .csv file
banknote <- read.csv('HW5_banknote.csv')
