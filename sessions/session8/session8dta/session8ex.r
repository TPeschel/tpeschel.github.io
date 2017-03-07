## set working directorysetwd("/media/mandy/Volume/transcend/life/2016kurs/session8/session8dta/")
load("aerzte.rdata")



library(ggplot2)
library(gridExtra)
p.fa <- ggplot(fa,aes(x = as.numeric(age.group), 
              y = mean.val, 
              linetype = sex)) +
  scale_linetype_manual(values = c(1,3,4)) +
  geom_line(size = 1.5) +
  theme_bw() +
  theme(
    legend.position = c(0.2,0.9),
    legend.key.width = unit(2,"cm"),
    legend.background = element_blank()
  )

p.fa

p.ha <- p.fa %+% ha

p.ha

p.ha + labs(x = "Altersgruppen", 
            y = "Mittelwert Hausarztbesuch")

### combine two data frames using rbind()

fa$physician <- "specialist"
ha$physician <- "general"
phys <- rbind(fa,ha)

## short - if plot already exists
p.fa %+% phys + facet_wrap(~physician,nrow = 2)


## define plot from the scratch
p.phy <- 
  
  ggplot(phys,aes(x = as.numeric(age.group), 
                         y = mean.val, 
                         linetype = sex)) +
  scale_linetype_manual(values = c(1,3,4)) +
  geom_line(size = 1.5) +
  facet_wrap(~physician,nrow =2,scales = "free_y") +
  theme_bw() +
  theme(
    legend.position = c(0.2,0.9),
    legend.key.width = unit(2,"cm"),
    legend.background = element_blank()
  )


p.phy

###################################################
####################  dplyr   #####################
###################################################

## first we load the data from the file "kromeyer.rdata" - 
## it is a `rdata` file - so we use the `load()` function

load("kromeyer.rdata")

## then we group the data frame by the `GRP` column 
## (scientific group) and calculate the mean of `AGE`
library(dplyr)

kh %>% group_by(GRP) %>%
  summarise(
    mean.age = mean(AGE)
  )


kh.sum <- kh %>% group_by(GRP) %>%
  summarise(
    mean.age = mean(AGE),
    sd.age = sd(AGE),
    mean.bmi = mean(BMI_SDS,na.rm = T)
  )


## Exercises

## - add the calculation of the mean and the standard deviation of 
##     - `HEIGHT_SDS`
##     - `WEIGHT_SDS`
##     - `BMI_SDS`


kh.sum <- kh %>% group_by() %>%
  summarise(
    mean.age = mean(AGE),
    sd.age = sd(AGE),
    mean.bmi = ,
    sd.bmi = ,
    mean.height = ,
    sd.height = ,
    mean.bmi = ,
    sd.bmi = 
  )    
    

head(kh.sum)

###################################################
################  data time   #####################
###################################################

library(lubridate)

kh$year <- year(kh$EDAT)

## Exercise
### Create a column `month` containing the month from the `EDAT` 
### column (the respective command is not surprisingly `month()`)!
  
kh$month <- year(kh$month)

### Now use these two variables `month` and `year` and `dplyr` for 
### counting the number of anthro measurements per month. 
### Create a new data frame containing the summarised information.

kh.sum.time <- kh %>% group_by(month,year) %>%
  summarise(
    n.obs = n()
  )



### Create a plot showing the months of the year on the x-axis and 
### number of anthro entries on the y-axis; draw one line per year.

ggplot(kh.sum.time,aes(x = month, y = n.obs, colour = factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.name)


ggplot(kh.sum.time,aes(x = month, y = n.obs, colour = factor(year))) +
  geom_point(size = 3) +
  geom_line() + 
  scale_x_continuous(breaks = 1:12, labels = month.name)


ggplot(kh.sum.time,aes(x = month, y = n.obs, colour = factor(year))) +
  geom_bar(stat = "identity",position = position_dodge())



### do an anova of BMI_SDS dependenton year. Are there differences?

m1 <- lm(BMI_SDS ~ factor(year),data = kh)
summary(m1)
