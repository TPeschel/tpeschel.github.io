## lade Paket zum Einlesen von Excel-Tabellen
library( "readxl" )

## lade Paket zum Plotten
library( "ggplot2" )

## setze das aktuelle Arbeitsverzeichnis auf das session6/r/
## hier: bleibe im aktuellen Verzeichnis. Eigentlich sinnlos.
## hier bitte in das Verzeichnis "sessions/session6/r/" wechseln
setwd( 'HIER PFAD ZU EUREM "sessions/session6/r"-VERZEICHNIS EINGEBEN!!!' )

## lade babies.xlsx und weise die Tabelle der Variablen babies zu
babies <- read_excel( "../data/babies.xlsx" )

## zeige die ersten 6 Zeilen
head( babies )

## 1 Unze sind 28.34952 Gramm oder .02834952 Kilogramm
kg.per.oz <- 28.34952e-3

## fuege neue Spalte hinzu, die die Gebutsgewichte in kg enthaelt
## multipliziere dazu Spalte "babies$wt" mit der Zahl in der 
## Variablen "kg.per.oz"
babies$bwt <- babies$wt * kg.per.oz

## zeige, wie die uns interessierenden Spalten aussehen
## sprich: welche unterscheidbaren Werte enthaelt die jeweilige Spalte
unique( babies$race )
unique( babies$smoke )
unique( babies$number )

## tabelliere Daten, um Vorstellung der Groessenordnungen zu gewinnen
## hier runden wir die Geburtsgewichte auf 100g, 
## da es sonst zu viele unterscheidbare 
## Geburtsgewichte gibt
## ABER: Daten nur zum Veranschaulichen runden, 
##   niemals, wenn man mit den eigentlichen Werten weiter rechnen moechte
( tbl.bwt    <- table( round( babies$bwt, 1 ) ) ) 

( tbl.race   <- table( babies$race ) )
( tbl.smoke  <- table( babies$smoke ) )
( tbl.number <- table( babies$number ) )

## zeige Verhältnisse
prop.table( tbl.bwt )
prop.table( tbl.race )
prop.table( tbl.smoke )
prop.table( tbl.number )

## dasselbe nochmal, nun aber in Prozent und auf 1 Kommastelle gerundet
## dazu multipliziert man einfach die Tabelle ( also deren Werte ) mit 100
## und dieses Ergebnis rundet man mit dem Befehl: round( ZAHL[EN], KOMMASTELL[EN] )
round( prop.table( tbl.bwt ) * 100, 1 )
round( prop.table( tbl.race ) * 100, 1 )
round( prop.table( tbl.smoke ) * 100, 1 )
round( prop.table( tbl.number ) * 100, 1 )

## zeige max 1Quantile Median Mean 3Quantile Max
## der Geburtsgewichte in kg an
summary( babies$bwt )

## erzeuge und sortiere Faktoren fuer auf- oder absteigende Barplots
babies$race.sorted   <- factor( ## erzeuge neue Spalte rece.sorted, 
  babies$race, ## die "race" als Faktoren enthaelt
  levels = names( ## die Levels sind die
    sort( ## abseigend sortierten Haeufigkeiten der Rassen in der
      tbl.race, ## Tabelle tbl.race
      decreasing = T ) ) )
## das gleiche fuer den Rauchertyp
babies$smoke.sorted  <- factor( babies$smoke, levels = names( sort( tbl.smoke, decreasing = T ) ) )

## das gleiche fuer die Anzahl der Zigaretten pro Tag
babies$number.sorted <- factor( babies$number, levels = names( sort( tbl.number, decreasing = T ) ) )

## erzeuge einen bar-plot der Haefigkeiten der Rassen
ggplot ( babies ) +
  theme_bw( ) + ## nutze das theme black/white
  geom_bar( aes( race.sorted ) ) ## nutze geom_bar, uebergieb als aesthetics die sortierten Rassen

## erzeuge einen bar-plot der Haefigkeiten der Rauchertypen
ggplot ( babies ) + 
  theme_bw( ) +
  geom_bar( aes( smoke.sorted ) )

## erzeuge einen bar-plot der Haefigkeiten der Zigaretten pro Tag
## mal als Einzeiler
ggplot ( babies ) + theme_bw( ) + geom_bar( aes( number.sorted ) )

## erzeuge Uebersichtsplot 
ggplot ( babies ) + 
  theme_bw( ) +
  geom_bar( aes( number.sorted, fill = number.sorted ) ) + ## Zigaretten pro Tag als Balkenhoehe, Rauchertyp als Füllfarbe
  facet_grid( race.sorted ~ smoke.sorted ) + ## erzeuge plot-grid nach Spalten und Zeilen sortierte Rassen und Rauchertypen  
  coord_flip( ) ## tausche Koordinaten, kippe Bild auf die Seite


## fuehre ANOVA mit dem Befehl "lm" durch
## und zeige die Ergebnisse sofort an ( deshalb die Klammern (...) um die Zuweisungen )
## zeige danach sofoert eine Zusammenfassung der Ergebnisse der Analyse mittels "summary( )" an
( lm.race   <- lm( babies$bwt ~ babies$race ) )
summary( lm.race )

( lm.smoke  <- lm( babies$bwt ~ babies$smoke ) )
summary( lm.smoke )

( lm.number <- lm( babies$bwt ~ babies$number ) )
summary( lm.number )

## zeige nur die Quotienten aus den Summen der quadrierten Residuen der einzelnen Gruppen
## und derder Gesamtstichprobe
## das Ergebnis ist das Verhaeltnis aus Gruppenvarianz und Stichprobenvarianz
## und liegt zwischen 0 und 1
summary( lm.race )$r.squared
summary( lm.smoke )$r.squared
summary( lm.number )$r.squared

summary.lm( lm.race )
summary.lm( lm.smoke )
summary.lm( lm.number )

## aov fuehrt auch eine ANOVA durch
aov( babies$bwt ~ babies$race )
## man vergleiche jeweils die beiden Ausgaben 
summary.aov( lm.race )

aov( babies$bwt ~ babies$smoke )
summary.aov( lm.smoke )

aov( babies$bwt ~ babies$number )
summary.aov( lm.number )

## erzeuge ein paar uebersichtliche Box-Plots
## die Rasse auf die x-Achse, das Geburtsgewicht auf die y-Achse
## berechne dann fuer jede Rasse (x-Achse) den Mittelwert (y-Achse)
## der Geburtsgewichte ( fun.y = "mean" ) und
## zeichne einen Punkt ein ( geom = "point" )
ggplot( babies, aes( x = race.sorted, y = bwt ) ) +
  geom_boxplot( ) + ## nutze geom_boxplot
  stat_summary( geom = "point", fun.y = "mean" ) 

## dasselbe fuer den Rauchertypen
ggplot( babies, aes( x = smoke.sorted, y = bwt ) ) +
  geom_boxplot( ) +
  stat_summary( geom = "point", fun.y = "mean" )

## dasselbe fuer den Rauchertypen
ggplot( babies, aes( x = number.sorted, y = bwt ) ) +
  geom_boxplot( ) +
  stat_summary( geom = "point", fun.y = "mean" )

## zeige nochmal alle Gruppenmittelwerte, Standardfehler t-Wert und 
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
