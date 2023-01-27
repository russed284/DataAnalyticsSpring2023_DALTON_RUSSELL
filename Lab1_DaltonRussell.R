EPI_data <- read.csv("EPI_2010Data.csv")
attach(EPI_data)
fix(EPI_data)
EPI
tf <- is.na(EPI)
tf
print(tf)
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI, na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI, na.rm=TRUE, bw=SJ.))
rug(EPI)
help(stem)
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
summary(Landarea)
tf2 <- is.na(Landarea)
tf2
print(tf2)
E2 <- Landarea[!tf2]
E2
summary(Landarea)
fivenum(Landarea, na.rm=TRUE)
stem(Landarea)
hist(Landarea)
hist(Landarea, seq(0.0000000, 0.0000002, 0.0000001), prob = TRUE)
lines(density(Landarea, na.rm=TRUE, bw=1.))
rug(Landarea)
plot(ecdf(Landarea), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(Landarea); qqline(Landarea)
x <- seq(0,1,0.01)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

summary(EPI_data)

tf3 <- is.na(ENVHEALTH)
tf3
print(tf3)
E3 <- ENVHEALTH[!tf3]
E3
summary(ENVHEALTH)
fivenum(ENVHEALTH, na.rm=TRUE)
stem(ENVHEALTH)
hist(ENVHEALTH)
hist(ENVHEALTH, seq(0., 96., 1.0), prob = TRUE)
lines(density(ENVHEALTH, na.rm=TRUE, bw=1.))
rug(ENVHEALTH)
plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ENVHEALTH); qqline(ENVHEALTH)
x <- seq(0,96,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

boxplot(ENVHEALTH, Landarea)

plot(ecdf(ENVHEALTH, Landarea), do.points=TRUE, verticals=TRUE)

qqnorm(ENVHEALTH, Landarea); qqline(ENVHEALTH, Landarea)
x <- seq(0,96,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

boxplot(EPI, ENVHEALTH, ECOSYSTEM, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
stem(EPI, ENVHEALTH, ECOSYSTEM, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
hist(EPI, ENVHEALTH, ECOSYSTEM, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY)
plot(ecdf(EPI, ENVHEALTH, ECOSYSTEM, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY), do.points=TRUE, verticals = TRUE)

stem(EPI)
stem(ENVHEALTH)
stem(ECOSYSTEM)
stem(AIR_H)
plot(ecdf(AIR_E, AIR_H), do.points=TRUE, verticals=TRUE)
hist(AIR_E, WATER_E)
qqnorm(AIR_E, WATER_E); qqline(AIR_E, WATER_E)
x <- seq(0,100,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

hist(AIR_E, WATER_E)
qqnorm(BIODIVERSITY, ENVHEALTH); qqline(BIODIVERSITY, ENVHEALTH)
x <- seq(0,100,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

hist(BIODIVERSITY, ENVHEALTH)
EPILAND <- EPI[!Landlock]
Eland <- EPILAND[!is.na(EPILAND)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

summary(No_surface_water)
par(pty="s")
qqnorm(No_surface_water); qqline(No_surface_water)
x <- seq(0,1,0.01)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
plot(ecdf(No_surface_water), do.points=TRUE, verticals=TRUE)


summary(Desert)
par(pty="s")
qqnorm(Desert); qqline(Desert)
x <- seq(0,1,0.01)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
plot(ecdf(Desert), do.points=TRUE, verticals=TRUE)


summary(High_Population_Density)
par(pty="s")
qqnorm(High_Population_Density); qqline(High_Population_Density)
x <- seq(0,1,0.01)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
plot(ecdf(High_Population_Density), do.points=TRUE, verticals=TRUE)

print(EPI_regions)
print(GEO_subregion)

summary(EPI_data)
print(Country)

filter1 <- EPI[Country]
filter2 <- EPI_regions[!filter1]
filter2
filter3 <- EPI_regions[filter1]
filter3

filter4 <- EPI[GEO_subregion]
filter5 <- Country[!filter4]
filter5

filter6 <- Country[!=filter5]
filter7 <- Country != filter4
filter7

filter8 <- Country != GEO_subregion
filter8
