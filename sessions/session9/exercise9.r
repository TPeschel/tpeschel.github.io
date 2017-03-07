## load the birth weight data
load()

## Metric Response, Numeric explanatory variable
m <- lm(bweight ~ gestwks, data=births)

## extract coefficients
coef(m)

## get information of summary
summary(m)


## get confidence interval of the coefficients
confint(m)



## Explanatory Variable is a Factor
m <- lm(bweight ~ hyp, data=births)


## extract coefficients


## omitting intercept
m <- lm(bweight ~ -1 + hyp, data=births)


## A Multivariable Model
m <- lm(bweight ~ hyp + gestwks, data=births)


library(effects)
allEffects(m)


## include interactions

m <- lm(bweight ~ hyp + gestwks + hyp:gestwks, data=births)
m <- lm(bweight ~ hyp * gestwks, data=births)


library(ggplot2)
ggplot(births,aes(x = gestwks, y = bweight, colour = hyp)) +
  geom_smooth(method = "lm", formula = y~x)


## center variable
births$gwsc <-
  
m <- lm(bweight ~ hyp * gwsc, data=births)


## How much is explained? - aov
m <- lm(bweight ~ gestwks, data=births)
anova(m)

sum(anova(m)$Sum)
anova(m)$Sum[1]/sum(anova(m)$Sum)

summary(m)$r.squared





## load the nhanes data
load()





## how many observations, how many variables?
## how many males/females
## nrow(), ncol(), table(), dim()



## how old are the participants (summary statistics, mean, sd)




## plot waistcf vs age
require(ggplot)





## model the respective data in a linear model,
## extract and interpret the coefficients.
## Extract also the confidence intervals.
## lm(), coef(), confint()





## add sex as a covariate. interpret. 






## add sex as a covariate. interpret. (including interaction)






