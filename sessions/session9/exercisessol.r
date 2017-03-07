## load the nhanes data
load("nhanesdata.rdata")

## how many observations, how many variables?
nrow(nhanes)
ncol(nhanes)
dim(nhanes)

## how old are the participants (summary statistics, mean, sd)
summary(nhanes$ridageyr)
sd(nhanes$ridageyr)


## plot waistcf vs age
require(ggplot2)
ggplot(nhanes,aes(x=ridageyr,y=bmxwaist)) +
    geom_point() +
    geom_smooth(method="lm")


## model the respective data in a linear model,
## extract and interpret the coefficients.
## Extract also the confidence intervals.
m <- lm(bmxwaist ~ ridageyr, data = nhanes)
summary(m)

coef(m)
confint(m)

## add sex as a covariate. interpret. 
m2 <- lm(bmxwaist ~ ridageyr + riagendr, data = nhanes)
summary(m2)

coef(m2)
confint(m2)

## add sex as a covariate. interpret. II
m3 <- lm(bmxwaist ~ ridageyr * riagendr, data = nhanes)
summary(m3)

coef(m3)
confint(m3)


nhanes$agec <- nhanes$ridageyr - 18
m3 <- lm(bmxwaist ~ agec * riagendr, data = nhanes)
summary(m3)

coef(m3)
confint(m3)
