#######################################################
## group 1
########################################################

#########################
## knn
########################

nyt1<-read.csv("nyt1.csv")
nyt1<-nyt1[which(nyt1$Impressions>0 & nyt1$Clicks>0 & nyt1$Age>0),]
nnyt1<-dim(nyt1)[1]
sampling.rate=0.9
num.test.set.labels=nnyt1*(1.-sampling.rate)
training <-sample(1:nnyt1,sampling.rate*nnyt1, replace=FALSE)
train<-subset(nyt1[training,],select=c(Age,Impressions))
testing<-setdiff(1:nnyt1,training)
test<-subset(nyt1[testing,],select=c(Age,Impressions))
cg<-nyt1$Gender[training]
true.labels<-nyt1$Gender[testing]
classif<-knn(train,test,cg,k=5)
classif
attributes(.Last.value)

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

#############################################
## k-means
#############################################

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

set.seed(123)
sim.xy <- function(n, mean, sd) cbind(rnorm(n, mean[1], sd[1]),rnorm(n, mean[2],sd[2]))
xy <- rbind(sim.xy(100, c(0,0), c(.2,.2)),sim.xy(100, c(2.5,0), c(.4,.2)),sim.xy(100, c(1.25,.5), c(.3,.2)))
xy[1,] <- c(0,2)     
km3 <- kmeans(xy, 3) 
plot(xy, col=km3$cluster)
cex=2.0
points(km3$centers, pch=3)
km4 <- kmeans(xy, 4) 
cex=1.0
plot(xy, col=km4$cluster)
cex=2.0
points(km4$centers, pch=3)

data("iris")
iris.dist <- dist(iris[, -5])
iris.mds <- cmdscale(iris.dist)
c.chars <- c("*", "o", "+")[as.integer(iris$Species)]

a.cols <- rainbow(3)[KMEANSRESULT$cluster]

plot(iris.mds, col = a.cols, pch = c.chars, xlab = "X", ylab = "Y")
plot of chunk unnamed-chunk-5

corr <- KMEANSRESULT$cluster == 4 - as.integer(iris$Species)
correct <- c("o", "x")[2 - corr]

plot(iris.mds, col = a.cols, pch = correct, xlab = "X", ylab = "Y")

#############################################
## filter
#############################################

EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

############################################
## data
############################################


EPI_data <- read.csv("<path>/2010EPI_data.csv")
view(EPI_data)
attach(EPI_data) 	
fix(EPI_data) 
EPI 			
tf <- is.na(EPI)
E <- EPI[!tf] 

GRUMP_data <- read.csv("<path>/GPW3_GRUMP_SummaryInformation_2010.csv")

###########################################
## distributions
###########################################

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty = "s") 
qqnorm(EPI); qqline(EPI)
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#############################################
## summary
#############################################


help(distributions)

summary(EPI) 	
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)		
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.))
help(rug)
rug(EPI) 
