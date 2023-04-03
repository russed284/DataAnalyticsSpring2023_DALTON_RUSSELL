library(ISLR)
library(MASS)
library(boot)
library(ggplot2)
library(dplyr)
library(fastDummies)

data <- read.csv("nyccacs.csv", header = TRUE)

df <- subset(data, BOROUGH == "QUEENS", select = c(BOROUGH, NEIGHBORHOOD, BUILDING_CLASS_CATEGORY,	TAX_CLASS_AS_OF_FINAL_ROLL,	BLOCK,	LOT,	EASE_MENT,	BUILDING_CLASS_AS_OF_FINAL_ROLL,	ADDRESS, APARTMENT_NUMBER,	ZIP_CODE,	RESIDENTIAL_UNITS,	COMMERCIAL_UNITS,	TOTAL_UNITS, LAND_SQUARE_FEET, GROSS_SQUARE_FEET,	YEAR_BUILT,	TAX_CLASS_AT_TIME_OF_SALE,	BUILDING_CLASS_AT_TIME_OF_SALE,	SALE_PRICE,	SALE_DATE, Latitude, Longitude,	Community_Board,	Council_District, Census_Tract,	BIN, BBL, NTA
))
df
print(unique(df[c("SALE_PRICE")]))

attach(df)

boxplot(as.factor(df$SALE_PRICE), xlab = "sale price")

is.na(df)

df1 <- df
df1 <- replace(df, is.na(df), 0)
is.na(df1)

df1$TAX_CLASS_AS_OF_FINAL_ROLL <- as.numeric(df1$TAX_CLASS_AS_OF_FINAL_ROLL)
df1$APARTMENT_NUMBER <- as.numeric(df1$APARTMENT_NUMBER)
df1$RESIDENTIAL_UNITS <- as.numeric(df1$RESIDENTIAL_UNITS)
df1$COMMERCIAL_UNITS <- as.numeric(df1$COMMERCIAL_UNITS)
df1$TOTAL_UNITS <- as.numeric(df1$TOTAL_UNITS)
df1$LAND_SQUARE_FEET <- as.numeric(df1$LAND_SQUARE_FEET)
df1$GROSS_SQUARE_FEET <- as.numeric(df1$GROSS_SQUARE_FEET)
df1$YEAR_BUILT <- as.numeric(df1$YEAR_BUILT)
df1$SALE_PRICE <- as.numeric(gsub(",","",df1$SALE_PRICE))

df1 <- replace(df1, is.na(df1), 0)


attach(df1)

lm11 <- lm(SALE_PRICE ~ TAX_CLASS_AS_OF_FINAL_ROLL + RESIDENTIAL_UNITS + COMMERCIAL_UNITS)
clm11 <- coef(lm11)
clm11

lm22 <- lm(SALE_PRICE ~ TOTAL_UNITS + LAND_SQUARE_FEET + GROSS_SQUARE_FEET)
clm22 <- coef(lm22)
clm22

pred22 <- predict(clm22, df1$SALE_PRICE)

CD <- cooks.distance(lm22)
sort(CD)

plot(CD)

plot(lm22, pch = 18, col = 'blue', which = c(5))

print(sort(unique(df1[c("SALE_PRICE")])))


condf <- subset(df1, (SALE_PRICE > 90000) & (SALE_PRICE < 1000000), select = c(BOROUGH, NEIGHBORHOOD, BUILDING_CLASS_CATEGORY,	TAX_CLASS_AS_OF_FINAL_ROLL,	BLOCK,	LOT,	BUILDING_CLASS_AS_OF_FINAL_ROLL,	ADDRESS, APARTMENT_NUMBER,	ZIP_CODE,	RESIDENTIAL_UNITS,	COMMERCIAL_UNITS,	TOTAL_UNITS, LAND_SQUARE_FEET, GROSS_SQUARE_FEET,	YEAR_BUILT,	TAX_CLASS_AT_TIME_OF_SALE,	BUILDING_CLASS_AT_TIME_OF_SALE,	SALE_PRICE,	SALE_DATE))
str(condf)

##condf2 <- subset(condf, SALE_PRICE != 10, select = c(BOROUGH, NEIGHBORHOOD, BUILDING_CLASS_CATEGORY,	TAX_CLASS_AS_OF_FINAL_ROLL,	BLOCK,	LOT,	BUILDING_CLASS_AS_OF_FINAL_ROLL,	ADDRESS, APARTMENT_NUMBER,	ZIP_CODE,	RESIDENTIAL_UNITS,	COMMERCIAL_UNITS,	TOTAL_UNITS, LAND_SQUARE_FEET, GROSS_SQUARE_FEET,	YEAR_BUILT,	TAX_CLASS_AT_TIME_OF_SALE,	BUILDING_CLASS_AT_TIME_OF_SALE,	SALE_PRICE,	SALE_DATE))

prepdf <- condf[complete.cases(condf),]
prepdf
str(prepdf)

boxplot(as.factor(prepdf$SALE_PRICE), xlab = "sale price")

hist(prepdf$BLOCK)

print(summary(as.factor(prepdf$SALE_PRICE)))

print(summary(data$SALE_PRICE))

print(unique(prepdf[c("NEIGHBORHOOD")]))
print(unique(prepdf[c("BUILDING_CLASS_CATEGORY")]))
print(unique(prepdf[c("BUILDING_CLASS_AS_OF_FINAL_ROLL")]))
print(unique(prepdf[c("ADDRESS")]))
print(unique(prepdf[c("BUILDING_CLASS_AT_TIME_OF_SALE")]))

##multivariate regression
##where I'm having problems

print(summary(as.factor(prepdf$BUILDING_CLASS_AT_TIME_OF_SALE)))

prep1 <- prepdf

prep1$SALE_PRICE <- as.numeric(gsub(",","",prep1$SALE_PRICE))

prep1$SALE_PRICE

prepdf$SALE_PRICE

prep2 <- prepdf

pr1 <- prepdf

pr1

prep2$BUILDING_CLASS_AS_OF_FINAL_ROLL <- dummy_cols(prep2$BUILDING_CLASS_AS_OF_FINAL_ROLL)
prep2$BUILDING_CLASS_AT_TIME_OF_SALE <- dummy_cols(prep2$BUILDING_CLASS_AT_TIME_OF_SALE)
prep2$TAX_CLASS_AS_OF_FINAL_ROLL <- as.numeric(prep2$TAX_CLASS_AS_OF_FINAL_ROLL)
prep2$APARTMENT_NUMBER <- as.numeric(prep2$APARTMENT_NUMBER)
prep2$RESIDENTIAL_UNITS <- as.numeric(prep2$RESIDENTIAL_UNITS)
prep2$COMMERCIAL_UNITS <- as.numeric(prep2$COMMERCIAL_UNITS)
prep2$TOTAL_UNITS <- as.numeric(prep2$TOTAL_UNITS)
prep2$LAND_SQUARE_FEET <- as.numeric(prep2$LAND_SQUARE_FEET)
prep2$GROSS_SQUARE_FEET <- as.numeric(prep2$GROSS_SQUARE_FEET)
prep2$YEAR_BUILT <- as.numeric(prep2$YEAR_BUILT)
prep2$SALE_PRICE <- as.numeric(gsub(",","",prep2$SALE_PRICE))
str(prep2)
########################################################################
########################################################################
########################################################################
pr1$TAX_CLASS_AS_OF_FINAL_ROLL <- as.numeric(pr1$TAX_CLASS_AS_OF_FINAL_ROLL)
pr1$APARTMENT_NUMBER <- as.numeric(pr1$APARTMENT_NUMBER)
pr1$RESIDENTIAL_UNITS <- as.numeric(pr1$RESIDENTIAL_UNITS)
pr1$COMMERCIAL_UNITS <- as.numeric(pr1$COMMERCIAL_UNITS)
pr1$TOTAL_UNITS <- as.numeric(pr1$TOTAL_UNITS)
pr1$LAND_SQUARE_FEET <- as.numeric(pr1$LAND_SQUARE_FEET)
pr1$GROSS_SQUARE_FEET <- as.numeric(pr1$GROSS_SQUARE_FEET)
pr1$YEAR_BUILT <- as.numeric(pr1$YEAR_BUILT)
pr1$SALE_PRICE <- as.numeric(gsub(",","",pr1$SALE_PRICE))

pr1

pr2 <- data.frame(pr1$BOROUGH, pr1$NEIGHBORHOOD, pr1$BUILDING_CLASS_CATEGORY, pr1$TAX_CLASS_AS_OF_FINAL_ROLL, pr1$APARTMENT_NUMBER, pr1$RESIDENTIAL_UNITS, pr1$COMMERCIAL_UNITS, pr1$TOTAL_UNITS, pr1$LAND_SQUARE_FEET, pr1$GROSS_SQUARE_FEET, pr1$YEAR_BUILT, pr1$SALE_PRICE)

pr3 <- data.frame(pr1$TAX_CLASS_AS_OF_FINAL_ROLL, pr1$RESIDENTIAL_UNITS, pr1$COMMERCIAL_UNITS, pr1$TOTAL_UNITS, pr1$LAND_SQUARE_FEET, pr1$GROSS_SQUARE_FEET)

pr4 <- data.frame(pr1$BLOCK, pr1$LOT, pr1$ZIP_CODE, pr1$TAX_CLASS_AS_OF_FINAL_ROLL, pr1$RESIDENTIAL_UNITS, pr1$COMMERCIAL_UNITS, pr1$TOTAL_UNITS, pr1$LAND_SQUARE_FEET, pr1$GROSS_SQUARE_FEET, pr1$SALE_PRICE)

pr4

attach(pr4)

lm1 <- lm(pr1.SALE_PRICE ~ pr1.TAX_CLASS_AS_OF_FINAL_ROLL + pr1.RESIDENTIAL_UNITS + pr1.COMMERCIAL_UNITS)
clm1 <- coef(lm1)
clm1

lm2 <- lm(pr1.SALE_PRICE ~ pr1.TOTAL_UNITS + pr1.LAND_SQUARE_FEET + pr1.GROSS_SQUARE_FEET)
clm2 <- coef(lm2)
clm2

px <- data.frame(pr1$RESIDENTIAL_UNITS, pr1$COMMERCIAL_UNITS, pr1$TOTAL_UNITS)
py <- data.frame(pr1$LAND_SQUARE_FEET, pr1$GROSS_SQUARE_FEET)

x <- as.matrix(px)
y <- as.matrix(py)

multivar_reg <- t(cov(y, x) %*% solve(cov(x)))

multivar_reg
pr3

prep2

prep3 <- data.frame(prep2$BLOCK, prep2$LOT, prep2$ZIP_CODE, prep2$TAX_CLASS_AT_TIME_OF_SALE, prep2$SALE_PRICE)

prep4 <- data.frame(prep2$BUILDING_CLASS_AS_OF_FINAL_ROLL)
prep5 <- data.frame(prep2$BUILDING_CLASS_AT_TIME_OF_SALE)

prep6 <- na.omit(prep3)
str(prep6)
length(prep6$prep2.BLOCK)
length(prep6$prep2.LOT)
length(prep6$prep2.ZIP_CODE)
length(prep6$prep2.TAX_CLASS_AT_TIME_OF_SALE)


set.seed(100)
train <- sample(nrow(prep4), 0.7*nrow(prep4), replace = FALSE)
TrainSet <- prep4[train,]
ValidSet <- prep4[-train,]
dim(TrainSet)
dim(ValidSet)
summary(TrainSet)
summary(ValidSet)

library(randomForest)

model1 <- randomForest(prep6$prep2.SALE_PRICE ~ ., data = TrainSet, importance = TRUE)
model1

str(model1)

model2 <- randomForest(pr1.SALE_PRICE ~ ., data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)
model2.fit


str(prep2)

prep3

prep4 <- data.frame(prep3$BOROUGH, prep3$NEIGHBORHOOD)

prep4

x <- as.matrix(prep_x)
y <- as.matrix(prep_y)

multivar_reg <- t(cov(y, x) %*% solve(cov(x)))


## This section

library(randomForest)

pr5 <- pr4[complete.cases(pr4),]
str(pr5)

attach(pr5)


set.seed(100)
train <- sample(nrow(pr5), 0.7*nrow(pr5), replace = FALSE)
TrainSet <- pr5[train,]
ValidSet <- pr5[-train,]
dim(TrainSet)
dim(ValidSet)
summary(TrainSet)
summary(ValidSet)

library(randomForest)

model1 <- randomForest(pr1.SALE_PRICE ~ ., data = TrainSet, importance = TRUE)
model1

str(model1)

model2 <- randomForest(pr1.SALE_PRICE ~ ., data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)
model2

predTrain <- predict(model2, TrainSet, type = "response")

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(model2))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

predTrain1 <- as.numeric(predTrain)

sp1 <- as.data.frame(pr5$pr1.SALE_PRICE)
samp_price <- dplyr::sample_n(sp1, (0.7*(nrow(sp1))))

TrainSet$pred <- predTrain1
TrainSet$act <- samp_price$`pr5$pr1.SALE_PRICE`

print(str(TrainSet))

str(samp_price)
str(predTrain)
length(pr5$pr1.SALE_PRICE)

confusionMatrix(predTrain, pr5$pr1.SALE_PRICE)

table(TrainSet$pred, TrainSet$act)

mean(((TrainSet$pred - TrainSet$act)^2)^1/2)

print(TrainSet$pred)
print(TrainSet$act)

str(predTrain)
str(model2)

length(predTrain)
dim(TrainSet)
dim()
length(pr5$pr1.SALE_PRICE)

print(length(predTrain)/length(pr5$pr1.SALE_PRICE))

###
## randomly select 70% of sale_price column for comparison so that 
## it equals the same amount of rows in train set
##
set.seed(100)
samp_price <- sample(nrow(pr1.SALE_PRICE), 0.7*nrow(pr1.SALE_PRICE), replace = FALSE)

sp <- table(pr5$pr1.SALE_PRICE)
sp

sp1 <- as.data.frame(pr5$pr1.SALE_PRICE)x

pr6 <- data.frame(pr1$BLOCK, pr1$LOT, pr1$ZIP_CODE, pr1$TAX_CLASS_AS_OF_FINAL_ROLL, pr1$LAND_SQUARE_FEET, pr1$SALE_PRICE)
pr7 <- pr6[complete.cases(pr6),]

attach(pr7)
model3 <- randomForest(pr1.SALE_PRICE ~ ., data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)
model3

dim(sp1)

samp_price <- dplyr::sample_n(sp1, (0.7*(nrow(sp1))))

samp_price

len(samp_price)

dim(samp_price)

table(predTrain, pr4$pr1.SALE_PRICE)
str(predTrain)

pr4$diff <- (predTrain - samp_price)

length(pr4$pr1.SALE_PRICE)

table(pt, samp_price)


class(predTrain)

pt <- as.data.frame(predTrain)

dim(pt)

class(samp_price)

sp2 <- as.numeric(samp_price)

len(sp2)

predValid <- predict(model2, ValidSet, type = "class")

table(predValid, pr5$pr1.SALE_PRICE)

importance(model2)
varImpPlot(model2)
