library(ISLR)
library(MASS)
library(boot)
attach(Auto)
set.seed(17) 
help("rep") 
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1] 
  }
cv.error.10 
