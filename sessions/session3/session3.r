#############
## RECAP 1 ##
#############




















## check working directory
getwd( )

## set working directory to today's folder
setwd( "~/Dokumente/RLearning/MandysKurs/20161128/2016kurs/session3/" )
##setwd("/media/mandy/Volume/transcend/life/2016kurs/session3/")

## load bird data
birds <- read.table( "data/bird.dat", header = F )   #header = F seems to be default

head( birds )

























#############
## RECAP 2 ##
#############

## load mz data
library( Hmisc )
library( "stringr" )

mz <- spss.get( "../session2/data/mz2010_cf.sav" )

## 1
ggplot( mz,
    aes( x = EF49 ) ) +
    geom_bar( )

ggplot( mz,
    aes( x = EF49, fill = EF46 ) ) +
    geom_bar( )

ggplot( mz,
    aes( x = EF49, fill = EF46 ) ) +
    geom_bar( position = position_fill( ) )

ggplot( mz,
    aes( x = EF49, fill = EF46 ) ) +
    geom_bar( position = position_fill( ) ) +
    scale_x_discrete( labels = function( x ) str_wrap( x, width = 25 ) ) +
    coord_flip( )






















# 2

ggplot( mz,
    aes( x = EF44, y = EF20 ) ) +
    geom_point( )

ggplot( mz,
    aes( x = EF44, y = EF20, col = EF46 ) ) +
    geom_point( )

ggplot( mz,
    aes( x = EF44, y = EF20, col = EF46 ) ) +
    geom_jitter( )

ggplot( mz,
    aes( x = EF44, y = EF20, col = EF46 ) ) +
    geom_jitter( alpha = .5 )
























## Load the ALLBUS data
allbus <- spss.get( "data/ZA5240_v2-0-0.sav" )

nrow( allbus )
ncol( allbus )
dim( allbus )


## V81 geschlecht
## V417 nettoeinkommen (offen und Liste)
## V118 arbeitsstunden

ggplot( allbus, aes( x = V81, y = V417 ) ) +
    geom_boxplot( ) +

ggplot( allbus, aes( x = V81, y = V417 ) ) +
    geom_boxplot( ) +
    scale_y_continuous( limits = c( 0, 10000 ) )

ggplot( allbus, aes( x = V81, y = V417 ) ) +
    geom_boxplot( ) +
    scale_y_log10( )

table( allbus$V81 )

ggplot( allbus, aes( x = V81, y = V417 ) ) +
    geom_boxplot( ) +
    geom_jitter( alpha = 0.2 ) +
    scale_y_log10( )

min( allbus$V417, na.rm = T )
max( allbus$V417, na.rm = T )
IQR( allbus$V417, na.rm = T )
mean( allbus$V417, na.rm = T )
median( allbus$V417, na.rm = T )
summary( allbus$V417, na.rm = TRUE )
fivenum( allbus$V417, na.rm = TRUE )
quantile( allbus$V417, seq( 0, 1, by = .25 ), na.rm = TRUE )







###############
## Excercise ##
###############


df1 <- data.frame( 
    value = c( rnorm( 100, mean = 0, sd = 0.1 ), rnorm( 100, mean = 1, sd = 0.1 ) ),
    group = rep( c( "A", "B" ), each = 100 ) )

ggplot( df1, aes( group, value ) ) +
    geom_boxplot( ) +
    stat_summary( geom = "point", fun.y = "mean", colour = "red", size = 3 )


df2 <- data.frame(
    value = c( rnorm( 100, mean = 0, sd = 5 ), rnorm( 100, mean = 1, sd = 5 ) ),
    group = rep( c( "A", "B" ), each = 100 ) )

ggplot( df2, aes( group, value ) ) +
    geom_boxplot( ) +
    stat_summary( geom = "point", fun.y = "mean", colour = "red", size = 3 ) +
    scale_y_continuous( limits = c( -10, 10 ) )


ggplot( df1, aes( group, value ) ) +
    geom_boxplot( ) +
    stat_summary( geom = "point", fun.y = "mean", colour = "red", size = 3 ) +
    scale_y_continuous( limits = c( -10, 10 ) )


t.test( df1$value ~ df1$group )

t.test( df2$value ~ df2$group )
