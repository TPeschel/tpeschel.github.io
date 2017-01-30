## lade Paket zum Einlesen von Excel-Tabellen
library( "readxl" )

## lade Paket zum Plotten
library( "ggplot2" )

## setze das aktuelle Arbeitsverzeichnis auf das session6/r
setwd( "~/Dokumente/RLearning/githubRepos/tpeschel.github.io/sessions/session6/r/" )

## lade babies.xlsx und weise die Tabelle der Variablen babies zu
babies <- read_excel( "../data/babies.xlsx" )

## zeige die ersten 6 Zeilen
head( babies )

## 1 Unze sind 28.34952 Gramm oder .02834952 Kilogramm
kg.per.oz <- 28.34952e-3

## fuege neue Spalte hinzu, die die Gebutsgewichte in kg enthaelt
babies$bwt <- round( babies$wt * kg.per.oz, 3 )

## zeige, wie die Daten aussehen
unique( babies$race )
unique( babies$smoke )
unique( babies$number )

## tabelliere Daten um Vorstellung der Groessenordnungen zu gewinnen
( tbl.bwt    <- table( round( babies$bwt, 1 ) ) )
( tbl.race   <- table( babies$race ) )
( tbl.smoke  <- table( babies$smoke ) )
( tbl.number <- table( babies$number ) )

## zeige Verhältnisse
round( prop.table( tbl.bwt ) * 100, 1 )
round( prop.table( tbl.race ) * 100, 1 )
round( prop.table( tbl.smoke ) * 100, 1 )
round( prop.table( tbl.number ) * 100, 1 )

summary( babies$bwt )

## erzeuge und sortiere Faktoren fuer auf- oder absteigende Barplots
babies$race.sorted   <- factor( babies$race, levels = names( sort( tbl.race, decreasing = T ) ) )
babies$smoke.sorted  <- factor( babies$smoke, levels = names( sort( tbl.smoke, decreasing = T ) ) )
babies$number.sorted <- factor( babies$number, levels = names( sort( tbl.number, decreasing = T ) ) )

ggplot ( babies ) + theme_bw( ) +
    geom_bar( aes( race.sorted ) )

ggplot ( babies ) + theme_bw( ) +
    geom_bar( aes( smoke.sorted ) )

ggplot ( babies ) + theme_bw( ) +
    geom_bar( aes( number.sorted ) )

ggplot ( babies ) + theme_bw( ) +
    geom_bar( aes( number, fill = number ) ) +
    facet_grid( race.sorted ~ smoke.sorted )



## 
( lm.race   <- lm( babies$bwt ~ babies$race ) )
( lm.smoke  <- lm( babies$bwt ~ babies$smoke ) )
( lm.number <- lm( babies$bwt ~ babies$number ) )

## Zusammenfassung der Ergebnisse der Fits
summary( lm.race )
summary( lm.smoke )
summary( lm.number )

anova( lm.race )
1 - pf( 11.473, 1, 1218 )

anova( lm.smoke )
1 - pf( 19.224, 1, 1231 )

anova( lm.number )
1 - pf( 5.9919, 1, 1225 )

summary( lm.race )$r.squared
summary( lm.smoke )$r.squared
summary( lm.number )$r.squared

summary.lm( lm.race )
summary.lm( lm.smoke )
summary.lm( lm.number )

summary.aov( lm.race )
summary.aov( lm.smoke )
summary.aov( lm.number )



aov( babies$bwt ~ babies$race )
aov( babies$bwt ~ babies$smoke )
aov( babies$bwt ~ babies$number )




1 - pf( 19.22, 1, 1235 )

TukeyHSD( lm.race )




ggplot( babies, aes( x = race.sorted, y = bwt ) ) +
  geom_boxplot( ) +
  stat_summary( geom = "point", fun.y = "mean" )

ggplot( babies, aes( x = smoke.sorted, y = bwt ) ) +
  geom_boxplot( ) +
  stat_summary( geom = "point", fun.y = "mean" )

ggplot( babies, aes( x = number.sorted, y = bwt ) ) +
  geom_boxplot( ) +
  stat_summary( geom = "point", fun.y = "mean" )


coef( summary( lm.race ) )
coef( summary( lm.smoke ) )
coef( summary( lm.number ) )



## install.packages("granovaGG")
library(granovaGG)

granovagg.1w( babies$bwt, babies$race )

granovagg.1w( babies$bwt, babies$smoke )

granovagg.1w( babies$bwt, babies$number )








table( babies$sex )
class( babies$sex )

babies$sex.mf <- factor( babies$sex, 
                         levels = c( 0, 1 ),
                         labels = c( "male", "female" ) )
class( babies$sex.mf )
table( babies$sex.mf )


table(babies$marital)
babies$marital.text <- factor( babies$marital, 
                         levels = c( 1 : 5 ),
                         labels = c( "married", "legally separated", "divorced", "widowed", "never married" ) )

table(babies$inc)
babies$inc.text <- factor( babies$inc,
                        levels = c( 0 : 9, 98, 99 ),
                        labels = c("<2500",
                                paste( 
                                    seq( 2500, 20000, by = 2500 ),
                                    seq( 4999, 22500, by = 2500 ),
                                    sep = "-" ),
                                    "25000+", "unknown", "not asked" ) )
table( babies$inc.text )
addmargins( prop.table( table( babies$inc.text, babies$marital.text ), 2 ) )

babies$sex.jm <- factor( babies$sex.mf, 
                    levels = c( "male", "female" ),
                    labels = c( "Junge", "Maedchen" ) )
table( babies$sex.jm )



library(car)
babies$drace <- recode( babies$drace,
                       '0:5="white";6="mex";7="black";8="asian";c(9,10)="mixed";99=NA')

prop.table( table( babies$race == babies$drace ) )
