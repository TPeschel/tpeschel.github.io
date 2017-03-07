## library(Epi) used

x <- runif(30,min=0,max=30)
y <- x + rnorm(30)
df <- data.frame(x=x,y=y)
df$z <- as.numeric(rownames(df))

png("nullmodel.png",width=1000,height=800)
par(cex.lab=2,cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(df$y,pch=18,ylab="response variable",cex=2,main="Total Sum of Squares")
abline(h=mean(df$y),col="red",lwd=2)
segments(x0=df$z,y0=df$y,x1=df$z,y1=mean(df$y),lwd=2)
dev.off()

png("minimalmodel.png",width=1000,height=800)
par(cex.lab=2,cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(df$x,df$y,pch=18,ylab="response variable",xlab="x",cex=2,main="Total Sum of Squares")
abline(h=mean(df$y),col="red",lwd=2)
segments(x0=df$x,y0=df$y,x1=df$x,y1=mean(df$y),lwd=2)
dev.off()

png("minimalmodel2.png",width=1000,height=800)
par(cex.lab=2,cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(df$x,df$y,pch=18,ylab="response variable",xlab="x",cex=2,main="Residual Sum of Squares")
abline(lm(y~x,data=df),col="red",lwd=2)
segments(x0=df$x,y0=df$y,x1=df$x,y1=predict(lm(y~x,data=df)),lwd=2)
dev.off()

## sum of squares
y

mean(y)

y-mean(y)

(y-mean(y))**2

sum((y-mean(y))**2)

deviance(lm(y~1),data=df)

### total
aov(lm(y~1,data=df))
### residual + explained
aov(lm(y~x,data=df))

## Examples
x <- runif(30,min=0,max=30) ## numerical
y <- x + rnorm(30) ## numerical
z <- rnorm(30,mean=x**2)
df <- data.frame(x=x,y=y,z=z) ## create data frame
df$sex <- factor(sample(1:2,30,replace=T), ## two level factor
                 levels=1:2,
                 labels=c("male","female")) 
df$genotype <- factor(sample(1:3,30,replace=T), ## three level factor
                      levels=1:3,
                      labels=c("wildtype","type1","type2"))

## Null Model:
lm0 <- lm(y~1,data=df)
lm0

mean(df$y)

plot(y)

abline(lm0)

abline(h=mean(y),col="red")

summary(lm0)
summary.aov(lm0)

plot(lm0,which=1)  ## Residuals vs. Fitted
plot(lm0,which=2)  ## qq plot of the standardized residuals
plot(lm0,which=3)  ## ## Residuals (standardized + sqrt) vs. Fitted
plot(lm0,which=4)  ## cooks distance
# hat values (leverages) are all = 0.03333333
# and there are no factor predictors; no plot no. 5
# plot(lm0,which=6) makes also no sense here as good as

## simple regression
lm1 <- lm(y~x,data=df)

sum(residuals(lm1)**2) ## residual sum of squares
sqrt(sum(residuals(lm1)**2)/28)  ## residual standard error

summary.aov(lm1)[[1]]$Df
summary.aov(lm1)[[1]]$Sum
summary.aov(lm1)[[1]]$Mean
summary.aov(lm1)[[1]]$F
summary.aov(lm1)[[1]]$Pr

plot(x,y)
abline(lm1)

plot(lm1, which=1)
plot(lm1, which=3)

plot(lm1, which=2)
plot(lm1, which=4)

shapiro.test(residuals(lm1))

## simple regression 2
lm2 <- lm(z~x,data=df)

plot(x,z)
abline(lm2)

plot(lm2,which=1)
shapiro.test(residuals(lm2))


lm2 <- lm(z~I(x**2),data=df)

plot(lm2, which=1)

## change scale for plotting
plot(x,z**(1/2))
abline(lm2)

## or
plot(x,z)

lines(sort(x),fitted(lm2)[order(x)])
predict(lm2,newdata=data.frame(x=0:30))

### 2a
lm2a <- lm(z~poly(x,2),data=df)

####

lm3 <- lm(y ~ sex,data=df)  
summary(lm3)

summary.aov(lm3)

lm4 <- lm(y ~ sex - 1,data=df)  
summary(lm4)


summary.aov(lm4)


## two way anova
lm5 <- lm(y ~ sex + genotype,data=df)  
summary(lm5)

lm5 <- lm(y ~ sex * genotype,data=df)  
summary(lm5)

library(ggplot2)
po <- ggplot(df,aes(x=sex,y=y))
po + geom_boxplot() + facet_wrap(~ genotype,ncol=3)

## analysis of covariance
lm6 <- lm(y~x+sex,data=df)

coef(lm6)

summary(lm6)

with(df,plot(x,y,col=sex))
abline(coef(lm6)[[1]],coef(lm6)[[2]])
abline(coef(lm6)[[1]] + coef(lm6)[[3]],coef(lm6)[[2]],col=2)

## analysis of covariance with interaction
lm7 <- lm(y~x*sex,data=df)

coef(lm7)

summary(lm6)

with(df,plot(x,y,col=sex))
abline(coef(lm7)[[1]],coef(lm6)[[2]])
abline(coef(lm7)[[1]] + coef(lm7)[[3]],coef(lm7)[[2]] + coef(lm7)[[4]],col=2)


po <- ggplot(df,aes(x=x,y=y,group=sex,color=sex))
po + geom_point() + geom_smooth() + facet_wrap(~sex)

po + geom_point() + geom_smooth(method="lm") + facet_wrap(~sex)

po <- ggplot(df,aes(x=x,y=y,group=sex,color=sex))
po + geom_point() + geom_smooth(method="lm")


### linear models

library(Epi)
data(births)
births <- transform(births,
                    lowbw = factor(lowbw, labels=c("normal","low")),
                    preterm = factor(preterm, labels=c("normal","preterm")),
                    hyp = factor(hyp, labels=c("normal","hyper")),
                    sex = factor(sex, labels=c("M","F")),
                    gest4 = cut(gestwks, breaks=c(20,35,37,39,45),
                    right=F)
)

dput(births,"births")

# slide metric response...
m <- lm(bweight ~ gestwks, data=births)
coef(m)

# slide extractor funcions
summary(m)

coef(m)
confint(m)

## other useful functions

## illustrate meaning of fitted and residuals
btmp <- births[complete.cases(births[,c("bweight","gestwks")]),]
tmpdf <- data.frame(gestwks=btmp$gestwks, bweight=btmp$bweight , fitted=fitted(m), residuals=resid(m))

## illustrate meaning of predict
ga <- 10:60
pred.birth.weight <- predict(m,newdata=data.frame(gestwks=ga))
tmpdf <- data.frame(gestwks=ga,bweight=pred.birth.weight)

## illustrate meaning of deviance
m0 <- lm(bweight~1,data=births)
deviance(m0) ## total sum of sqares
deviance(m) ## error sum of squares of our model

(deviance(m0)-deviance(m))/deviance(m0) ## r-squared: slightly different because there are some missing values

m0 <- lm(bweight~1,data=btmp) ## repeat with this model (missing values excluded) you get the appropriate r-squared

## ANOVA
## slide explanatory v is factor
m1 <- lm(bweight ~ hyp, data=births)
coef(m1) ## gives a mean and a difference of means

by(births$bweight,births$hyp,mean) ## gives the two means

m2 <- lm(bweight ~ -1 + hyp, data=births) ## gives also the two means
coef(m2)

## if you look at the p-values of m1 and m2 they differ (i.e. the second) - because in m2 the p-vals are the probability that the values are different from 0
## in m1 the first p-value means the same (diff. from 0) but the second p-val is the probability that the difference is different from 0

## slide a model with both gestwks and hyp
m1 <- lm(bweight ~ hyp + gestwks, data=births)
summary(m1)

coef(m1)
confint(m1)

m2 <- lm(bweight ~ hyp + gestwks - 1, data=births)
summary(m2)

coef(m2)
confint(m2)


### jackknife

leverage <- function(x){1/length(x)+(x-mean(x))**2/sum((x-mean(x))**2)}
jtmp <- numeric()
for(i in 1:nrow(births)){
  m <- lm(bweight ~ hyp + gestwks, data=births[-i,])
  jtmp[i] <- summary(m)$r.squared
}

m1a <- lm(bweight ~ hyp + gestwks, data=births[-1,])


png("model1.png",width=1000,height=600)
par(cex.lab=2,cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(births$gestwks,births$bweight,col=births$hyp,cex=2,pch=".",xlim=c(30,43),ylim=c(1500,4200))
abline(coef(m)[c(1,3)],lwd=2)
abline(coef(m)[1]+coef(m)[2],coef(m)[3],col="red",lwd=2)
text(x=30,y=1600,"A",cex=2)
text(x=32,y=1600,"B",col="red",cex=2)
text(36.5,2300,"hyp=hyper",col="red",cex=2)
text(34,2600,"hyp=normal",cex=2)
dev.off()

## slide a multivariable model
m <- lm(bweight ~ hyp + gestwks, data=births)
summary(m)

## slide interaction between
m <- lm(bweight ~ hyp * gestwks, data=births)

png("model2.png",width=1000,height=800)
par(cex.lab=2,cex.axis=1.5, mar=c(5,5,4,2)+0.1)
plot(births$gestwks,births$bweight,col=births$hyp,cex=2,pch=".",xlim=c(30,43),ylim=c(1500,4200))
abline(coef(m)[c(1,3)],lwd=2)
abline(coef(m)[1]+coef(m)[2],coef(m)[3]+coef(m)[4],col="red",lwd=2)
text(x=30,y=1680,"A",cex=2)
text(x=32,y=1500,"B",col="red",cex=2)
text(36.5,2300,"hyp=hyper",col="red",cex=2)
text(34,2600,"hyp=normal",cex=2)
dev.off()

## slide interaction models, scaled
m <- lm(bweight ~ hyp * gestwks, data=births)
births$gwsc <- births$gestwks-40
m <- lm(bweight ~ hyp * gwsc, data=births)

summary(m)

## aov
m <- lm(bweight ~ gestwks, data=births)
anova(m)

sum(anova(m)$Sum)

anova(m)$Sum[1]/sum(anova(m)$Sum)

summary(m)
summary(m)$r.squared


#### generalized linear models
m <- lm(bweight ~ hyp, data=births)
m <- glm(bweight ~ hyp, family=gaussian, data=births)

## slide Predicting Low Birth Weight
m <- glm(lowbw ~ hyp, family=binomial, data=births)
ci.lin(m)[,5:7]

table(births$hyp, births$lowbw)
20/52 ## low
40/388 ## normal
(20/52) / (40/388) ## ratio


## slide controlling
m <- glm(lowbw ~ hyp+sex, family=binomial, data=births)
ci.lin(m,Exp=T)

## slide interaction (effect modification)
m <- glm(lowbw ~ hyp + sex + hyp:sex, family=binomial, data=births)
ci.lin(m,Exp=T)[,5:7]

m <- glm(lowbw ~ hyp*sex, family=binomial, data=births)

## slide testing for interaction
m1 <- glm(lowbw ~ hyp+sex, family=binomial, data=births)
m2 <- glm(lowbw ~ hyp*sex, family=binomial, data=births)
anova(m1,m2,test="Chisq")


## slide stratified effects
m <- glm(lowbw ~ sex + sex:hyp, family=binomial, data=births)
ci.lin(m,Exp=T)[,5:7]

par(cex.lab=1,cex.axis=1, mar=c(5,4,4,2)+0.1)

