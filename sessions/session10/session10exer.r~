load("birthsweights.rdata")

## Exercises
## Remember: We used hypertension of the mom to explain variation
## in the birth weight of the kid. Without looking in the material
## of the last session, try to redo the model.





require(effects)




res <- allEffects(m.hyp, se = T)
summary(res)

invlogit <- function(x){
    exp(x)/(1+exp(x))
}


## What is the relationship between the coefficients of the model
## (from the model summary) and the effects?




## binomial/logistic regression
m.wks <- glm(lowbw ~ gestwks, family=binomial, data=births)
summary(m.wks)


## interpreting the coefficients
invlogit(coef(m.wks)[1])

exp(coef(m.wks)[2])


## effects
Effect("gestwks",m.wks)


## Exercise
Effect("gestwks",m.wks,xlevels = list(gestwks = c(20,30,40)))


## visualiziation
require(ggplot2)
ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = F,size = 2) +
    geom_point(shape="|")


## Exercises

## try to change the axis titles (xlab() and ylab())
## add a title (ggtitle())






## change the colour of the function to black
## change the colour of the points to red for the low birth weight
## and to green for the one with normal birth weight







## change the position of the legend;
## place it somewhere near the upper right corner
## inside the plotting area







## the famous Challenger DAta

require(faraway)
data(orings)

## model
m.oring <- glm(cbind(damage,6-damage) ~ temp,family=binomial, orings)
summary(m.oring)

## coefficients
invlogit(coef(m.oring)[1])
invlogit(coef(m.oring)[2])

exp(coef(m.oring)[2])

Effect("temp",m.oring,
       xlevels = list(temp = c(50:60)),
       transformation = list(link = logit,inverse = exp))




require(scales)
demo(plotmath)

orings$trials <- 6
ggplot(orings,aes(x=temp,y=damage/trials)) +
    geom_point() +
    geom_smooth(method = "glm", family = "binomial", aes(weight = trials)) +
    xlab(expression(paste("temperature (",degree,"F)"))) +
    ylab("probability of damage") +
    scale_y_continuous(labels = percent)

ggsave("img/challenger.png")

ggplot(orings,aes(x=temp,y=damage/trials)) +
    geom_point() +
    xlim(20,90) +
    geom_smooth(method = "glm", family = "binomial",
                aes(weight = trials), fullrange =T) +
    xlab(expression(paste("temperature (",degree,"F)"))) +
    ylab("probability of damage") +
    scale_y_continuous(labels = percent)


ggsave("img/challenger2.png")


## Infection Example

load("infection.rdata")
summary(infection)

m.inf <- glm(infected~age*sex,family=binomial,
                               data=infection)

summary(m.inf)

## coefficients
### intercepts
invlogit(coef(m.inf)[1])
invlogit(coef(m.inf)[1] + coef(m.inf)[3])

### slopes
exp(coef(m.inf)[2])
exp(coef(m.inf)[2] + coef(m.inf)[4])

exp(30 * coef(m.inf)[2])
exp(30 * (coef(m.inf)[2] + coef(m.inf)[4]))

solve(0.015657,3.000513)
solve(0.02670685,2.883849)

## all effects

allEffects(m.inf)

allEffects(m.inf,
           xlevels = list(age = seq(0,200,by = 50)))

















## comparing different link functions
ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_smooth(method = "glm", family = binomial(link = "probit"),se = T,size = 2, colour = "red") +
    geom_smooth(method = "glm", family = binomial(link = "cloglog"),se = T,size = 2, colour = "green") +
    geom_point(shape="|")
