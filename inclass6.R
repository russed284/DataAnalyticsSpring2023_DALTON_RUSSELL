multivariate <- read.csv("multivariate.csv")
head(multivariate)
attach(multivariate)
tf <- is.na(Immigrants)
tf
tf2 <- is.na(Homeowners)
tf2
mm3 <- lm(Homeowners~Immigrants)
mm3
summary(mm3)$coef
plot(Homeowners~Immigrants)
abline(mm3)
abline(mm3, col=2, lwd=3)

newimmigrantdata <- data.frame(Immigrants =c(0,20))
mm3 %>% predict(newimmigrantdata)
Homeowners

library(ggplot2)

qplot(multivariate$Homeowners, multivariate$Immigrants)
qplot(Immigrants, Homeowners, data=multivariate)
ggplot(multivariate, aes(x=Immigrants, y=Homeowners))+geom_point()
plot(multivariate$Immigrats, multivariate$Income, type = "l")
points(multivariate$City, multivariate$Income)

multivariate$Homeowners
multivariate$Immigrants
