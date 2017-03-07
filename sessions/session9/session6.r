## Birth data
load("birthsraw.rdata")
load("births.rdata")


births <- transform(births,
      lowbw = factor(lowbw, labels=c("normal","low")),
      preterm = factor(preterm, labels=c("normal","preterm")),
      hyp = factor(hyp, labels=c("normal","hyper")),
      sex = factor(sex, labels=c("M","F")),
      gest4 = cut(gestwks, breaks=c(20,35,37,39,45),
      right=F))


## relationship bweight ~ gestwks
m <- lm(bweight ~ gestwks, data=births)

summary(m)
coef(m)
confint(m)


## relationship bweight ~ hyp
m <- lm(bweight ~ hyp, data=births)
summary(m)
coef(m)
confint(m)



## relationship bweight ~ hyp + gestwks
m <- lm(bweight ~ hyp + gestwks, data=births)
summary(m)
coef(m)
confint(m)


## relationship bweight ~ hyp * gestwks
m <- lm(bweight ~ hyp * gestwks, data=births)
summary(m)
coef(m)
confint(m)

## center variable to get meaningful intercept
births$gwsc <- births$gestwks-40
m <- lm(bweight ~ hyp * gwsc, data=births)
summary(m)
coef(m)
confint(m)


