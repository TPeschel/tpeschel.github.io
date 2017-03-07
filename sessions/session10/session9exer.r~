## Exercise
## The excel file data.xlsx contains the worksheets mother and child
## containing respective parts of the births data set. Use the
## read_excel() command to read both data sets and






## use merge() to join them
names(mdata)
names(cdata)

intersect(names(mdata),names(cdata))



str(births)

summary(births$hyp)
births$hyp <- factor(births$hyp,levels = c("normal","hyper"))
summary(births$hyp)

summary(births$lowbw)
births$lowbw <- factor(births$lowbw,levels = c("normal","low"))
summary(births$lowbw)

summary(births$sex)
births$sex <- factor(births$sex,levels = c("M","F"))
summary(births$sex)

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
