################################################
## GROUP 2
################################################

################################################
## CTREE 1 & 2
################################################


require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) 
text(Swiss_rpart) 

install.packages("party")
require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))

install.packages("tree")
library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)

fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)

plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

##########################################################################
## Naive Bayes 1, 2, 3, 4
##########################################################################

install.packages("mlbench")
require(mlbench)
library(e1071)
data(HouseVotes84)
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")

pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)

data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic)[,1:3])

data(iris)
m <- naiveBayes(Species ~ ., data = iris)
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])

data(HouseVotes84)
install.packages("klaR")
library(klaR)
model <- NaiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])

pred <- predict(model, HouseVotes84[,-1])
table(pred$class, HouseVotes84$Class)


install.packages('ElemStatLearn')
library('ElemStatLearn')
library("klaR") 
library("caret")
data(spam, package="ElemStatLearn")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

prop.table(table(predict(model$finalModel,xTest)$class,yTest))

train.ind <- sample(1:nrow(spam), ceiling(nrow(spam)*2/3), replace=FALSE)

nb.res <- NaiveBayes(spam ~ ., data=spam[train.ind,])

nb.pred <- predict(nb.res, spam[-train.ind,])

head(predict(nb.res))

###################################################################
## KNN labs 
#########################################################

install.packages("kknn")
require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")[(iris.valid$Species != fit)+1])

library(kknn)

spam.kknn <- kknn(spam~., train, test, distance = 1,
                  kernel = "triangular")
summary(spam.kknn)

require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,] 	# train
iris.valid <- iris[val,]	# test
iris.kknn <- train.kknn(Species~., iris.learn, distance = 1, kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal") )
summary(iris.kknn)
table(predict(iris.kknn,iris.valid),iris.valid$Species)

head(iris.kknn$W)
head(iris.kknn$D)
head(iris.kknn$C)
head(iris.kknn$fitted.values)

require(kknn)
data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)


data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
              prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
                  kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
      [(iris.valid$Species != fit)+1])

require(kknn)
data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)

data(swiss)

pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

##################################################
## rpart 1, 2, 3
##################################################

require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) 
text(Swiss_rpart) 

require(rpart)
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fitM) 
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM) 
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)
pfitM<- prune(fitM, cp=0.01160389) # from cptable??? adjust this to see the effect
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)
help(post)
post(pfitM, file = "ptree2.ps", title. = "Pruned Regression Tree for Mileage")

library(e1071)
library(rpart)
data(Glass, package="mlbench")
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

rsq.rpart(rpart.model)
print(rpart.model)

plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)

fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) 
plotcp(fitK) 
summary(fitK) 
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis")

pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
post(pfitK, file = "ptree.ps", title = "Pruned Classification Tree for Kyphosis")

########################################################
## gpairs
###################################################

allexamples <- FALSE
install.packages("gpairs")
library(gpairs)

y <- data.frame(A=c(rep("red", 100), rep("blue", 100)),
                B=c(rnorm(100),round(rnorm(100,5,1),1)), C=runif(200),
                D=c(rep("big", 150), rep("small", 50)),
                E=rnorm(200))
gpairs(y)

data(iris)
gpairs(iris)
if (allexamples) {
  gpairs(iris, upper.pars = list(scatter = 'stats'),
         scatter.pars = list(pch = substr(as.character(iris$Species), 1, 1),
                             col = as.numeric(iris$Species)),
         stat.pars = list(verbose = FALSE))
  gpairs(iris, lower.pars = list(scatter = 'corrgram'),
         upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
         scatter.pars = list(pch = 20))
}

data(Leaves)
gpairs(Leaves[1:10], lower.pars = list(scatter = 'loess'))
if (allexamples) {
  gpairs(Leaves[1:10], upper.pars = list(scatter = 'stats'),
         lower.pars = list(scatter = 'corrgram'),
         stat.pars = list(verbose = FALSE), gap = 0)
  corrgram(Leaves[,-33])
}

runexample <- FALSE
if (runexample) {
  data(NewHavenResidential)
  gpairs(NewHavenResidential)
}

#######################################
## pairs
#######################################

install.packages("pairs")
library(pairs)
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

pairs(iris[-5], log = "xy") # plot all variables on log scale
pairs(iris, log = 1:4, # log the first four
      main = "Lengths and Widths in [log]", line.main=1.5, oma=c(2,2,3,2))

pairs(~ Fertility + Education + Catholic, data = swiss,
      subset = Education < 20, main = "Swiss data, Education < 20")

pairs(USJudgeRatings)
pairs(USJudgeRatings, text.panel = NULL, upper.panel = NULL)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(USJudgeRatings[1:5], panel = panel.smooth,
      cex = 1.5, pch = 24, bg = "light blue",
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor)

###############################################
## swiss
###############################################

data(swiss)
sclass <- kmeans(swiss[2:7], 3) 
table(sclass$cluster, swiss[,2])    
# 
library(e1071)
m <- naiveBayes(swiss[2:7], swiss[,2])    
table(predict(m, iswiss[2:7], swiss[,2])

#########################################
## mosaic
#########################################

data(HairEyeColor)
mosaicplot(HairEyeColor)
margin.table(HairEyeColor,3)
margin.table(HairEyeColor,c(1,3))

install.packages("splom")
library(splom)

#####################################
## spm
##################################

install.packages("car")
require(car)
scatterplotMatrix(iris)
scatterplotMatrix(swiss)

