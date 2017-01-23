library( "ggplot2" )
library( "dplyr" )

education <- read.csv( "http://www.barrolee.com/data/BL_v2.1/BL2013_MF1599_v2.1.csv" )

summary( education )

max( education$lp )

plot( education$lp, education$lpc, col=education$country )

plot(
  education$lpc ~ education$lp,
  col  = education$country,
  main = "education",
  xlab = "precentage which had primary school completed",
  ylab = "precentage of population with primary school as highest level" 
)

abline( 0, 1 )
