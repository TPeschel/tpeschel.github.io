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


require(ggplot2)
require(scales)

ggplot(infection,aes(y = as.numeric(infected=="infected"), x = age, color=sex, shape=sex))+
  geom_smooth(method = "glm", family = "binomial")+
  geom_jitter(position = position_jitter(width = 0, height= 0.08),size=5,face="bold")+
  scale_color_manual( values = c("male"="midnightblue", "female"= "deeppink"))+
  scale_shape_manual(values = c("male" = "\u2642","female" = "\u2640"))+
  labs(title="Infection", x = "age in days", y= "infected in percent") +
  scale_y_continuous(breaks=seq(0,1,by=0.25),labels = percent)
  











## different link functions

ggplot(births,aes(x = gestwks, y = (1-as.numeric(lowbw)-1))) +
    geom_smooth(method = "glm", family = "binomial",se = T,size = 2) +
    geom_smooth(method = "glm", family = binomial(link = "probit"),se = T,size = 2, colour = "red") +
    geom_smooth(method = "glm", family = binomial(link = "cloglog"),se = T,size = 2, colour = "green") +
    geom_point(shape="|")


## multiple linear regression
sc.race <- read.table("ScotsRaces.dat",header = T)
m1 <- lm(time ~ climb + distance, data = sc.race)
m1



resid(m1)

m.climb <- lm(time ~ distance, data = sc.race)
m.dist <- lm(climb ~ distance, data = sc.race)

m.res <- lm(resid(m.climb) ~ resid(m.dist))


## av.plots()
require(car)
avPlots(m1)

avPlot(m1,"climb")

##
head(cbind(resid(m.dist),
           avPlot(m1,"climb"),
           resid(m.climb)))
      
## summarizing the fit of a linaer model
require(psych)
describe(sc.race[,-1])

M <- cor(sc.race[,-1])
M

pairs(sc.race[,-1])

pairs(sc.race[,-1],panel = panel.smooth)

require(corrplot)
corrplot.mixed(M)

### partial correlation
require(ppcor)
pcor.test(sc.race$distance,sc.race$climb,sc.race$time)
