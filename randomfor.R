install.packages("randomForest")
library(randomForest)

data1 <- read.csv("car.csv", header = TRUE)
head(data1)

colnames(data1) <- c("BuyingPrice", "Maintenance", "NumDoors", "NumPersons", "BootSpace", "Safety", "Condition")

head(data1)
str(data1)

levels(data1$Condition)
summary(data1)
