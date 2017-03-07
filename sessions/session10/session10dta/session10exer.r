## Bowlingdateien
library(readxl)
x2013 <- read_excel("Bowling2013_r.xls","punkte_strikes")
x2014 <- read_excel("Bowling2014_r.xls","punkte_strikes")
x2015 <- read_excel("Bowling2015_r.xls","punkte_strikes")

x <- rbind(x2013,x2014,x2015)
x <- x[!is.na(x$spieler),]

table(x$spieler)

library(lubridate)
x$Jahr <- year(x$tag)
x$Monat <- month(x$tag,label = T)

library(dplyr)

x.sum <- x %>% group_by(Jahr, spieler) %>% summarise(
  n.spiele = n(),
  punkte.ges = sum(punkte),
  min.jahr = min(Jahr)
)


library(ggplot2)


ggplot(x.sum, aes(x = spieler, y = punkte.ges/n.spiele, fill = punkte.ges/n.spiele)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Jahr,nrow = 3) +
  scale_fill_gradient(low = "red",high = "green")


ggplot(x.sum, aes(x = spieler, y = punkte.ges/n.spiele, fill = n.spiele)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Jahr,nrow = 3) +
  scale_fill_gradient(low = "red",high = "green")



str(births)


## from lm() to glm()
m1 <- lm(bweight ~ hyp, data=births)
m2 <- glm(bweight ~ hyp, family=gaussian, data=births)

## binary response
y <- sample(0:1,100, replace = T)

plot(y)
abline(h=mean(y))

p <- seq(0,1,by=0.05)

log(p/(1-p))

## inverse logit function

invlogit <- function(x){
    exp(x)/(1+exp(x))
}

## install and load the arm package, there is also a invlogit function
## low birth weight model
m <- glm(lowbw ~ hyp, family=binomial, data=births)

invlogit(coef(m)[1])

table(births$lowbw,births$hyp)
40/(388+40)

## proportion test
prop.test(c(20,40),c(72,428))

chisq.test(table(births$lowbw,births$hyp))

## effects plot
require(effects)
plot(Effect("hyp",m))

## effects
Effect("hyp",m)


## controlling for sex
m2 <- glm(lowbw ~ hyp+sex, family=binomial, data=births)
summary(m2)


m3 <- glm(lowbw ~ hyp*sex, family=binomial, data=births)
summary(m3)



Effect("hyp",m2)
Effect("sex",m2)

plot(Effect("hyp",m2))
plot(Effect("sex",m2))

Effect(c("hyp","sex"),m2)
Effect(c("sex","hyp"),m2)

plot(Effect(c("hyp","sex"),m2))
plot(Effect(c("sex","hyp"),m2))



## Exercise
## the estimated probability for moms with hypertension to get
## a baby with low birth weight for all three models





## is their a difference in effects between boys and girls?
## Which model can answer this question?




## stratified model

m4 <- glm(lowbw ~ sex + sex:hyp, family=binomial, data=births)
summary(m4)

m4 <- glm(lowbw ~ sex/hyp, family=binomial, data=births)
m4

## Exercise



## understanding the coefficients
ftable(births$hyp,
       births$sex,
       births$lowbw)

## male/normal bp
15/(206+15)
## female/normal bp
25/(25+182)
## male/high bp
12/(12+31)
## female/high bp
8/(8+21)

prop.table(table(births$lowbw,
                 births$hyp,
                 births$sex),c(2,3))


## logistic regression
m5 <- glm(lowbw ~ gestwks, family = binomial, data= births)
summary(m5)

## understanding the effects
### intercept
invlogit(coef(m5)[1])

### slope
exp(coef(m5)[2])

## exercise

Effect("gestwks",m5)
Effect("gestwks",m5,xlevels = list(gestwks = c(20,30,40)))

## use the command to gain the estimated probability of
## low birth weight for a gestational age of 27 and 36 weeks




## example for calculating the odds
Effect("gestwks",m5,xlevels = list(gestwks = c(27,28)))
Effect("gestwks",m5,xlevels = list(gestwks = c(39,40)))

p.from.odds <- function(odds) odds/(1+odds)
odds.from.p <- function(p) p/(1-p)

odds.from.p(0.01779725)/odds.from.p(0.04252149)


require(ggplot2)
ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_point(shape="|")

ggsave("img/glmggplot.png")


## try to change the axis titles (xlab() and ylab())
## add a title (ggtitle())

ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_point(shape="|")





## change the colour of the function to black
## change the colour of the points to red for the low birth weight
## and to green for the one with normal birth weight

ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_point(shape="|")





## change the position of the legend;
## place it somewhere near the upper right corner
## inside the plotting area


ggplot(births,aes(x = gestwks, y = as.numeric(lowbw)-1)) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_point(shape="|")



load("birthsweights.rdata")

## Exercises
## Remember: We used hypertension of the mom to explain variation
## in the birth weight of the kid. Without looking in the material
## of the last session, try to redo the model.

m.hyp <- lm()



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
