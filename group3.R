


n <- 150 
p <- 2 
sigma <- 1 
meanpos <- 0 
meanneg <- 3 
npos <- round(n/2) 
nneg <- n-npos
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)
y <- matrix(c(rep(1,npos),rep(-1,nneg)))
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

ntrain <- round(n*0.8) 
tindex <- sample(n,ntrain)
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))

library(e1071)
library(rpart)
data(Ozone, package= "mlbench")
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

data(iris)
attach(iris)

model <- svm(Species ~ ., data = iris)

x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

pred <- predict(model, x)
pred <- fitted(model)

table(pred, y)

pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

m   <- svm(x, y)
new <- predict(m, x)

plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

m <- svm(X, gamma = 0.1)

m <- svm(~., data = X, gamma = 0.1)
m <- svm(~ a + b, gamma = 0.1)

newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)

data(promotergene)

ind <- sample(1:dim(promotergene)[1],20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]

gene <-  ksvm(Class~.,data=genetrain,kernel="rbfdot",\
              kpar=list(sigma=0.015),C=70,cross=4,prob.model=TRUE)

genetype <- predict(gene,genetest,type="probabilities")

library(e1071) 
m1 <- matrix( c( 
  0,    0,    0,    1,    1,    2,     1, 2,    3,    2,    3, 3, 0, 1,2,3, 
  0, 1, 2, 3, 
  1,    2,    3,    2,    3,    3,     0, 0,    0,    1, 1, 2, 4, 4,4,4,    0, 
  1, 2, 3, 
  1,    1,    1,    1,    1,    1,    -1,-1,  -1,-1,-1,-1, 1 ,1,1,1,     1, 
  1,-1,-1 
), ncol = 3 ) 

Y = m1[,3] 
X = m1[,1:2] 

df = data.frame( X , Y ) 

par(mfcol=c(4,2)) 
for( cost in c( 1e-3 ,1e-2 ,1e-1, 1e0,  1e+1, 1e+2 ,1e+3)) { 
  model.svm <- svm( Y ~ . , data = df ,  type = "C-classification" , kernel = 
                      "linear", cost = cost, 
                    scale =FALSE ) 
  #print(model.svm$SV) 
  
  plot(x=0,ylim=c(0,5), xlim=c(0,3),main= paste( "cost: ",cost, "#SV: ", 
                                                 nrow(model.svm$SV) )) 
  points(m1[m1[,3]>0,1], m1[m1[,3]>0,2], pch=3, col="green") 
  points(m1[m1[,3]<0,1], m1[m1[,3]<0,2], pch=4, col="blue") 
  points(model.svm$SV[,1],model.svm$SV[,2], pch=18 , col = "red") 
}

data(spam)

index <- sample(1:dim(spam)[1])
spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]

filter <- ksvm(type~.,data=spamtrain,kernel="rbfdot",
               kpar=list(sigma=0.05),C=5,cross=3)
filter

mailtype <- predict(filter,spamtest[,-58])

table(mailtype,spamtest[,58])

data(iris)

install.packages("rbfdot")
rbf <- rbfdot(sigma=0.1)
rbf

irismodel <- ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10,prob.model=TRUE)

irismodel

fitted(irismodel)

predict(irismodel, iris[,-5], type="probabilities")

x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60),rep(-1,60)))

svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)


K <- as.kernelMatrix(crossprod(t(x)))

svp2 <- ksvm(K, y, type="C-svc")

svp2

xtest <- rbind(matrix(rnorm(20),,2),matrix(rnorm(20,mean=3),,2))

Ktest <- as.kernelMatrix(crossprod(t(xtest),t(x[SVindex(svp2), ])))

predict(svp2, Ktest)


k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene


data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv


x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)

regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")

k <- function(x,y) {(sum(x*y) +1)*exp(-0.001*sum((x-y)^2))}
class(k) <- "kernel"

data(promotergene)

gene <- ksvm(Class~.,data=promotergene[c(1:20, 80:100),],kernel=k,
             C=5,cross=5)

gene

library(mlbench)
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)
BC.bagging <- bagging(Class ~., data=BreastCancer[,-1], mfinal=20, control=rpart.control(maxdepth=3))
BC.bagging.pred <-predict.bagging( BC.bagging, newdata=BreastCancer[-sub,-1])
BC.bagging.pred$confusion

library(ipred)
data(Ozone)
l <- length(Ozone[,1])
sub <- sample(1:l,2*l/3)
OZ.bagging <- bagging(V4 ~., data=Ozone[,-1], mfinal=30, control=rpart.control(maxdepth=5))
OZ.bagging.pred <-predict.bagging( OZ.bagging, newdata=Ozone[-sub,-4])
OZ.bagging.pred$confusion

library(ipred)
library("MASS")
library("survival")

data("BreastCancer", package = "mlbench")

mod <- bagging(Class ~ Cl.thickness + Cell.size
               + Cell.shape + Marg.adhesion   
               + Epith.c.size + Bare.nuclei   
               + Bl.cromatin + Normal.nucleoli
               + Mitoses, data=BreastCancer, coob=TRUE)
print(mod)

data("Ionosphere", package = "mlbench")
Ionosphere$V2 <- NULL # constant within groups

bagging(Class ~ ., data=Ionosphere, coob=TRUE)

comb.lda <- list(list(model=lda, predict=function(obj, newdata)
  predict(obj, newdata)$x))

mod <- bagging(Class ~ ., data=Ionosphere, comb=comb.lda) 

predict(mod, Ionosphere[1:10,])

data("BostonHousing", package = "mlbench")

mod <- bagging(medv ~ ., data=BostonHousing, coob=TRUE)
print(mod)

library("mlbench")
learn <- as.data.frame(mlbench.friedman1(200))

mod <- bagging(y ~ ., data=learn, coob=TRUE)
print(mod)

data("DLBCL", package = "ipred")
mod <- bagging(Surv(time,cens) ~ MGEc.1 + MGEc.2 + MGEc.3 + MGEc.4 + MGEc.5 +
                 MGEc.6 + MGEc.7 + MGEc.8 + MGEc.9 +
                 MGEc.10 + IPI, data=DLBCL, coob=TRUE)

print(mod)

library(fpc)
cbi1<-clusterboot(iris[,-5],B=100, distances=(class(iris[,-5])=="dist"),
                  bootmethod="boot",
                  bscompare=TRUE, 
                  multipleboot=FALSE,
                  jittertuning=0.05, noisetuning=c(0.05,4),
                  subtuning=floor(nrow(iris)/2),
                  clustermethod=kmeansCBI,noisemethod=FALSE,count=TRUE,
                  showplots=FALSE,dissolution=0.5, krange=5,
                  recover=0.75,seed=NULL)

print(cbi1) 

plot(cbi1)

require(ggplot2)        
data(diamonds)
head(diamonds)          
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)
ggplot(diamonds, aes(clarity)) + geom_freqpoly(aes(group = cut, colour = cut))

diamonds$Expensive <- ifelse(diamonds$price >= 12000,1,0)
head(diamonds)

diamonds$price<-NULL
require(glmnet)         # or load package first
x<-model.matrix(~., diamonds[,-ncol(diamonds)])
y<-as.matrix(diamonds$Expensive)
mglmnet<-glmnet(x=x,y=y,family="binomial")
plot(mglmnet)

set.seed(51559)
sample(1:10)
require(rpart)
mTree<-rpart(Expensive~.,data=diamonds)
plot(mTree)
text(mTree)

require(boot)
mean(diamonds$carat)
ds(diamonds$carat)
boot.mean<-function(x,i)
{
  mean(x[i])
}
boot(data=diamonds$carat, statistic=boot.mean,R=120)

library( MASS )

data( birthwt )
data( VA )
data( iris )
data( fgl )
data( cpus )
data( housing )

set.seed( 20090417 )


bMod <- gbm( low ~ ., data=birthwt,
             n.tree=1000, shrinkage=.01, cv.folds=5,
             verbose = FALSE, n.cores=1)
bMod

bwt <- birthwt
bwt <- bwt[ sample( 1:nrow( bwt ) ),]
aMod <- gbm( low ~ ., data=bwt, distribution="adaboost",
             n.trees=1000, shrinkage=.01, cv.folds=10,
             train.fraction=.9, verbose = FALSE , n.cores=1)
aMod

cMod <- gbm( Surv( stime, status ) ~ treat + age + Karn + diag.time + cell + prior,
             data = VA, n.tree = 1000, shrinkage=.1, cv.folds = 5,
             verbose = FALSE, n.cores=1)
cMod

kMod <- gbm( Species ~ . , data=iris , n.tree=1000, shrinkage=.1,
             cv.folds=5, train.fraction=.9, n.cores=1 )
kMod

kMod2 <- gbm( type ~ ., data=fgl, n.tree=1000, shrinkage=.01,
              cv.folds=5, n.cores=1 )
kMod2

mycpus <- cpus
mycpus <- mycpus[, -1 ]
gMod <- gbm( log( perf ) ~ ., data = mycpus, distribution="gaussian",
             cv.folds=5, n.trees=1000, shrinkage=.01,
             verbose = FALSE, n.cores=1)
gMod

biMod <- gbm( log(perf) ~ ., data=mycpus,
              cv.folds=5, n.trees=1000, shrinkage=.01, n.cores=1 )
biMod

tMod <- gbm( log(perf) ~ ., data=mycpus, distribution="tdist",
             cv.folds=5, n.trees=1000, shrinkage=.01,
             interaction.depth= 3, n.cores=1)
tMod

lMod <- gbm( log(perf) ~ ., data=mycpus, distribution="laplace",
             cv.folds=5, n.trees=1000, shrinkage=.01,
             interaction.depth= 3, n.cores=1)
lMod

qMod <- gbm( log(perf) ~ ., data=mycpus,
             distribution=list(name="quantile", alpha=.7 ),
             cv.folds=5, n.trees=1000, shrinkage=.01,
             interaction.depth= 3, verbose = FALSE, n.cores=1)
qMod

pMod <- gbm( Freq ~ ., data=housing , distribution="poisson",
             n.trees=1000, cv.folds=5 , shrinkage=.01,
             interaction.depth = 3, n.cores=1)
pMod

N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N) 
mu <- c(-1,0,1,2)[as.numeric(X3)]

SNR <- 10 
Y <- X1**1.5 + 2 * (X2**.5) + mu
sigma <- sqrt(var(Y)/SNR)
Y <- Y + rnorm(N,0,sigma)

X1[sample(1:N,size=500)] <- NA
X4[sample(1:N,size=300)] <- NA

data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

gbm1 <-
  gbm(Y~X1+X2+X3+X4+X5+X6,         
      data=data,                   
      var.monotone=c(0,0,0,0,0,0), 
      distribution="gaussian",     
      n.trees=1000,                
      shrinkage=0.05,              
      interaction.depth=3,        
      bag.fraction = 0.5,          
      train.fraction = 0.5,       
      n.minobsinnode = 10,         
      cv.folds = 3,                
      keep.data=TRUE,              
      verbose=FALSE,               
      n.cores=1)                   

best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)

best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)

best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)

summary(gbm1,n.trees=1)         
summary(gbm1,n.trees=best.iter) 

print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))

N <- 1000
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE))
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N) 
mu <- c(-1,0,1,2)[as.numeric(X3)]

Y <- X1**1.5 + 2 * (X2**.5) + mu + rnorm(N,0,sigma)

data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

f.predict <- predict(gbm1,data2,best.iter)

print(sum((data2$Y-f.predict)^2))

par(mfrow=c(1,3))
plot(gbm1,1,best.iter)
plot(gbm1,2,best.iter)
plot(gbm1,3,best.iter)
par(mfrow=c(1,1))
plot(gbm1,1:2,best.iter)
plot(gbm1,2:3,best.iter)
plot(gbm1,3:4,best.iter)

plot(gbm1,c(1,2,6),best.iter,cont=20)
plot(gbm1,1:3,best.iter)
plot(gbm1,2:4,best.iter)
plot(gbm1,3:5,best.iter)

gbm2 <- gbm.more(gbm1,100,
                 verbose=FALSE) # stop printing detailed progress


install.packages("MetaPCA")
library(MetaPCA)

data(Spellman) 
pc <- list(alpha=prcomp(t(Spellman$alpha))$x, cdc15=prcomp(t(Spellman$cdc15))$x,
           cdc28=prcomp(t(Spellman$cdc28))$x, elu=prcomp(t(Spellman$elu))$x)
metaPC <- MetaPCA(Spellman, method="Eigen", doPreprocess=FALSE)
plot
metaPC <- MetaPCA(Spellman, method="RobustAngle", doPreprocess=FALSE)
metaPC <- MetaPCA(Spellman, method="SparseAngle", doPreprocess=FALSE)

par(mfrow=c(2,4), cex=1, mar=c(0.2,0.2,0.2,0.2))
for(i in 1:4) {
  plot(pc[[i]][,1], pc[[i]][,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n")
  text(pc[[i]][,1], pc[[i]][,2], 1:nrow(pc[[i]]), cex=1.5)
  lines(pc[[i]][,1], pc[[i]][,2])
}
for(i in 1:4) {
  plot(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n")
  text(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2], 1:nrow(metaPC$x[[i]]$coord), cex=1.5)
  lines(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2])
}

data(prostate)
metaPC <- MetaPCA(prostate, method="Eigen", doPreprocess=FALSE, .scale=TRUE)
metaPC <- MetaPCA(prostate, method="Angle", doPreprocess=FALSE)
metaPC <- MetaPCA(prostate, method="RobustAngle", doPreprocess=FALSE)
metaPC <- MetaPCA(prostate, method="SparseAngle", doPreprocess=FALSE,iter=100)
coord <- foreach(dd=iter(metaPC$x), .combine=rbind) %do% dd$coord
PlotPC2D(coord[,1:2], drawEllipse=F, dataset.name="Prostate", .class.order=c("Metastasis","Primary","Normal"), 
         .class.color=c('red','#838383','blue'), .annotation=T, newPlot=T,
         .class2=rep(names(metaPC$x), times=sapply(metaPC$x,function(x)nrow(x$coord))), 
         .class2.order=names(metaPC$x), .points.size=1)

metaPC$v[order(abs(metaPC$v[,1]), decreasing=TRUE),1][1:20] 

install.packages("EDR")
library(EDR)

demo(edr_ex1)
demo(edr_ex2)
demo(edr_ex3)
demo(edr_ex4)

library(dr)
data(ais)
s0 <- dr(LBM~log(SSF)+log(Wt)+log(Hg)+log(Ht)+log(WCC)+log(RCC)+
           log(Hc)+log(Ferr),data=ais) 
summary(s1 <- update(s0,slice.function=dr.slices.arc))
summary(s2<-update(s1,nslices=10,method="save"))
 
summary(s3<- update(s1,method="phdres"))
summary(s4 <- update(s1,method="ire"))
s5 <- update(s4,group=~Sex)



data(iqitems)

data(ability)
ability.irt <- irt.fa(ability)
ability.scores <- score.irt(ability.irt,ability)

data(attitude)
cor(attitude)
pfa.eigen<-eigen(cor(attitude))
pfa.eigen$values
factors<-2
pfa.eigen$vectors [ , 1:factors ]  %*% 
  + diag ( sqrt (pfa.eigen$values [ 1:factors ] ),factors,factors )

v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) 
factanal(m1, factors = 3, rotation = "promax")
prcomp(m1) 

factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores

data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))
scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary) 

set.seed(1.234)
N <- 200                             
P <- 6                               
Q <- 2                              

Lambda <- matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),
                 nrow=P, ncol=Q, byrow=TRUE)

library(mvtnorm)                     
FF  <- rmvnorm(N, mean=c(5, 15), sigma=diag(Q))    
E   <- rmvnorm(N, rep(0, P), diag(P))
X   <- FF %*% t(Lambda) + E          
Xdf <- data.frame(X)                  


library(psych) 
fa(X, nfactors=2, rotate="varimax")$loadings     

Xdi    <- lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf  <- do.call("data.frame", Xdi) 
XdiNum <- data.matrix(Xdidf)        

library(polycor)                    
pc <- hetcor(Xdidf, ML=TRUE)         

faPC <- fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings

faPCdirect <- fa.poly(XdiNum, nfactors=2, rotate="varimax")    
faPCdirect$fa$loadings        

factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)

fa.parallel.poly(XdiNum)      
vss(pc$correlations, n.obs=N, rotate="varimax")   
library(random.polychor.pa)    
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=0.99)

library(mboost)
library(TH.data)
?mboost_fit
data(bodyfat)
head(bodyfat)
tail(bodyfat)
summary(bodyfat)
boxplot(bodyfat)

lm1 <- lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
coef(lm1)
glm1 <- glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat)
coef(glm1, off2int=TRUE) 
glm2 <- glmboost(DEXfat ~ ., data = bodyfat)
preds <- names(bodyfat[, names(bodyfat) != "DEXfat"]) 
fm <- as.formula(paste("DEXfat ~", paste(preds, collapse = "+"))) 
fm

coef(glm2, which = "") 
plot(glm2, off2int = TRUE)
plot(glm2, ylim = range(coef(glm2, which = preds)))

boost_control(mstop = 200, nu = 0.05, trace = TRUE) ## print status information? Default: FALSE

z <- factor(1:3)
extract(bols(z))

gam1 <- gamboost(DEXfat ~ bbs(hipcirc) + bbs(kneebreadth) + bbs(anthro3a),data = bodyfat)
par(mfrow = c(1,3)) 
plot(gam1) 

gam2 <- gamboost(DEXfat ~ ., baselearner = "bbs", data = bodyfat,control = boost_control(trace = TRUE))
set.seed(123)
cvm <- cvrisk(gam2) 
cvm
par(mfrow = c(1,1))
plot(cvm)

mstop(cvm) 
gam2[ mstop(cvm) ] 
names(coef(gam2)) 
gam2[1000, return = FALSE] 

names(coef(gam2)) 

glm3 <- glmboost(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = bodyfat,family = QuantReg(tau = 0.5), control = boost_control(mstop = 500))
coef(glm3, off2int = TRUE)

library(mboost)
data(cars)
cars.gb <- gamboost(dist ~ speed, data = cars, dfbase = 4, control = boost_control(mstop = 50))
cars.gb
AIC(cars.gb, method = "corrected")

plot(dist ~ speed, data = cars)
tmp <- sapply(1:mstop(AIC(cars.gb)), function(i)
  lines(cars$speed, predict(cars.gb[i]), col = "red"))
lines(cars$speed, predict(smooth.spline(cars$speed, cars$dist), cars$speed)$y, col = "green")

x <- sort(runif(100)) * 10
y <- sin(x) + rnorm(length(x), sd = 0.25)
plot(x, y)
lines(x, fitted(lm(y ~ sin(x) - 1)), col = "red")
lines(x, fitted(gamboost(y ~ x, control = boost_control(mstop = 500))), col = "green")


install.packages("GAMBoost")
library(GAMBoost)
x <- matrix(runif(100*8,min=-1,max=1),100,8)
eta <- -0.5 + 2*x[,1] + 2*x[,3]^2
y <- rbinom(100,1,binomial()$linkinv(eta))
gb1 <- GAMBoost(x,y,penalty=400,stepno=100,trace=TRUE,family=binomial())
gb1.crit <- cv.GAMBoost(x,y,penalty=400,maxstepno=100,trace=TRUE, family=binomial(), K=10,type="error",just.criterion=TRUE)
which.min(gb1$AIC)
which.min(gb1.crit$criterion)


dist.au <- read.csv("http://rosetta.reltech.org/TC/v15/Mapping/data/dist-Aus.csv")
row.names(dist.au) <- dist.au[, 1]
dist.au <- dist.au[, -1]
dist.au
fit <- cmdscale(dist.au, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
city.names <- c("Adelaide", "Alice Springs", "Brisbane", "Darwin", "Hobart", 
                "Melbourne", "Perth", "Sydney")
text(x, y, pos = 4, labels = city.names)

library(igraph)
g <- graph.full(nrow(dist.au))
V(g)$label <- city.names
layout <- layout.mds(g, dist = as.matrix(dist.au))
plot(g, layout = layout, vertex.size = 3)

require(graphics)

loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2] 
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), cex = 0.6)


