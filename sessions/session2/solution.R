### use R as calculator 

### Solution

### 0. let our small data set be: 2 5 4 10 8






### 1. enter these data into a data vector v
  
(
    v <- c( 2, 5, 4, 10, 8 ) 
)





### 2. find the square of each number.

(
    v.sqr <- v * v
)





### 3. substract 6 from each number

(
    v.minus.6 <- v - 6
)





### 4. substract 9 from each number and then square the answer

(
    v.minus.9.squared <- ( v - 9 ) ^ 2
)





### 5. load the ChickWeight data set into the workspace using data()

data( "ChickWeight" )

head( ChickWeight )





### 6. how many observations and how many variables? How did you get the information?

dim( ChickWeight )
nrow( ChickWeight )
ncol( ChickWeight )





### 7. what is the mean, min and max of weight and Time

min( ChickWeight$weight )
mean( ChickWeight$weight )
max( ChickWeight$weight )

min( ChickWeight$Time )
mean( ChickWeight$Time )
max( ChickWeight$Time )

summary( ChickWeight$weight )
summary( ChickWeight$time )
summary( ChickWeight )

ChickWeight[ order( ChickWeight$Diet ), ]



### 8. use plot() to produce a boxplot of weight dependent on Diet

plot( ChickWeight$weight ~ ChickWeight$Diet )





### 9. check your working directory

getwd( )    






### 10. set the working directory to today s sesssion folder if not already done

setwd( "~/Dokumente/RLearning/MandysKurs/20161128/2016kurs/session2/")

getwd( )    





## 11. package installation

install.packages( "ggplot2" )
