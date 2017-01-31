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
babies$race.sorted   <- factor( ## erzeuge neue Spalte race.sorted, 
  babies$race, ## die "race" als Faktoren enthaelt
  levels = names( ## die Levels sind die
    sort( ## abseigend sortierten Haeufigkeiten der Rassen in der
      tbl.race, ## Tabelle tbl.race
      decreasing = T ) ) )

## das gleiche fuer den Rauchertyp
babies$smoke.sorted  <- factor( babies$smoke, levels = names( sort( tbl.smoke, decreasing = T ) ) )

## das gleiche fuer die Anzahl der Zigaretten pro Tag
babies$number.sorted <- factor( babies$number, levels = names( sort( tbl.number, decreasing = T ) ) )

## erzeuge ein Balkendiagramm der Haefigkeiten der Rassen
ggplot ( babies ) +
  theme_bw( ) + ## nutze das theme black/white
  geom_bar( aes( race.sorted ) ) ## nutze geom_bar, uebergib als aesthetics die sortierten Rassen

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
## zeige danach sofort eine Zusammenfassung der Ergebnisse der Analyse mittels "summary( )" an
( lm.race   <- lm( babies$bwt ~ babies$race ) )
summary( lm.race )

( lm.smoke  <- lm( babies$bwt ~ babies$smoke ) )
summary( lm.smoke )

( lm.number <- lm( babies$bwt ~ babies$number ) )
summary( lm.number )

## zeige nur die Quotienten aus den Summen der quadrierten Residuen der einzelnen Gruppen
## und der der Gesamtstichprobe
## das Ergebnis ist das Verhaeltnis aus Gruppenvarianz und Stichprobenvarianz
## und liegt zwischen 0 und 1
summary( lm.race )$r.squared
summary( lm.smoke )$r.squared
summary( lm.number )$r.squared

## summary.lm( ) erzeugt dieselbe Ausgabe wie summary( )
summary.lm( lm.race )
summary.lm( lm.smoke )
summary.lm( lm.number )

## aov fuehrt auch eine ANOVA durch
aov( babies$bwt ~ babies$race )
## man vergleiche jeweils die beiden Ausgaben 
summary.aov( lm.race )
## summary.aov( ) extrahiert aus den von lm( ) erzeugten 
## Daten eine ANOVA-Zusammenfassung

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

## zeige nochmal alle Gruppenmittelwerte, Standardfehler, t-Wert und 
coef( summary( lm.race ) )
coef( summary( lm.smoke ) )
coef( summary( lm.number ) )

## Interpretation der Werte
## ( Intercept ) ist der Referenzmittelwert, der sich aus der in der Tabelle
## fehlenden Rasse "asian" berechnet. Die Zahlen darunter sind Offsets,
## die zum Referenzwert addiert die tatsaechlichen Mittelwerte der einzelnen Rassen angeben.
## Beispiel:
## race white: 3.13068904 + 0.31778568 = 3.448475
## der t-Wert gibt an, wie extrem der Effekt ( der Mittelwert der Haeufigkeiten pro Rasse )
## bezueglich des Standardfehlers ist. Er ergibt sich aus Mittelwert / Std.Error
## Beispiel:
## 3.13068904 / 0.07640119 = 40.9769674
## das gilt auch fuer die Offsets selbst:
## 0.07954554 / 0.08300446  = 0.9583285
## Naja und der p-Wert gibt eben die Wahrscheinlichkeit an,
## dass das Ergebnis unter Annahme der Nullhypothese durch Zufall entstanden sein koennte
## 
##                     Estimate Std. Error    t value      Pr(>|t|)
## (Intercept)        3.13068904 0.07640119 40.9769674 1.993267e-231
## babies$raceblack   0.07954554 0.08300446  0.9583285  3.380873e-01
## babies$racemex     0.38890387 0.11071584  3.5126307  4.599375e-04
## babies$racemixed   0.26558346 0.12692719  2.0924079  3.660861e-02
## babies$raceunknown 0.75319520 0.51251476  1.4696069  1.419265e-01
## babies$racewhite   0.31778568 0.07830934  4.0580812  5.262889e-05



## install.packages("granovaGG")
library( granovaGG )

granovagg.1w( babies$bwt, babies$race )
## Fehler in if (owp$stats$F.statistic > 1) { : 
##   Fehlender Wert, wo TRUE/FALSE nötig ist
## granovagg.1g kann mit NA's nicht umgehen
## also dasselbe nochmal ohne NA's
## finde alle Fehlwerte mit is.na( )
## is.na( ) erzeugt einen Vektor, in dem fuer jede Zeile festgehalten ist,
## ob es in ihr eine Fehlstelle gibt (TRUE) oder nicht (FALSE)
## uns interessieren nur die Nichtfehlwerte
## das "!" vor is.na( ) macht aus FALSE TRUE und umgekehrt
ids.no.missings <- !is.na( babies$race )

## benutze neuen logischen Vektor als Filter
granovagg.1w( babies$bwt[ ids.no.missings ], babies$race[ ids.no.missings ] )


# keine Fehlwerte, keine Probleme!
# vergleiche mit Anova-Tabelle
granovagg.1w( babies$bwt, babies$smoke )
    
##    coef( summary( lm.smoke ) )
##                                       Estimate Std. Error     t value     Pr(>|t|)
##    (Intercept)                     3.480685276 0.02153934 161.5966294 0.000000e+00
##    babies$smokeonce but not now    0.047040916 0.05398412   0.8713844 3.837142e-01
##    babies$smokesmokes now         -0.245735607 0.03139109  -7.8281972 1.060773e-14
##    babies$smokeunknown             0.111198908 0.16031987   0.6936065 4.880598e-01
##    babies$smokeuntil current preg  0.008693012 0.05586259   0.1556142 8.763627e-01

## nochmal fuer die Zigarettenanzahl pro Tag
granovagg.1w( babies$bwt, babies$number )





##  RECODING


## das Geschlecht ist 0/1-kodiert also numerisch
table( babies$sex )
class( babies$sex )

## moechte man den Sex statt mit 0 und 1
## "male", "female" kodieren, kann man das mit dem Befehl factor( )
## 1. Argument: die Spalte, deren Kodierung geaendert werden soll
## 2. Argument: die Levels der alten Spalte
## 3. Argument: die neuen Labels zu den Levels
babies$sex.mf <- factor( babies$sex, 
                         levels = c( 0, 1 ),
                         labels = c( "male", "female" ) )
class( babies$sex.mf )
table( babies$sex.mf )
## die Klasse ist nun  ein Faktor 

table( babies$marital )
babies$marital.text <- factor( babies$marital, 
                         levels = c( 1 : 5 ),
                         labels = c( "married", "legally separated", "divorced", "widowed", "never married" ) )
table( babies$marital.text )

## das Einkommen ist als Kode hinterlegt
table( babies$inc )

## um die Geldmenge, die hinter den Kodes steht, zu sehen
## weist man jedem Eintrag (Kode) den entsprechenden Geldbetrag zu
babies$inc.text <- factor( babies$inc,
                        levels = c( 0 : 9, 98, 99 ),
                        labels = c("<2500",
                                paste( 
                                    seq( 2500, 20000, by = 2500 ),
                                    seq( 4999, 22500, by = 2500 ),
                                    sep = "-" ),
                                    "25000+", "unknown", "not asked" ) )
table( babies$inc.text )

## addmargins fuehrt der prop.table eine Spalte und eine Zeile hinzu, die die
## entsprechende Zeilensumme bzw. Spaltensumme angibt
addmargins( prop.table( table( babies$inc.text, babies$marital.text ) ) )

## translate into german
babies$sex.jm <- factor( babies$sex.mf, 
                    levels = c( "male", "female" ),
                    labels = c( "Junge", "Maedchen" ) )

table( babies$sex.jm )


## Paket "car" enthaelt den Befehl recode( )
library( car )

babies$drace <- recode( babies$drace,
                       '0:5="white";6="mex";7="black";8="asian";c(9,10)="mixed";99=NA')


## babies$race == babies$drace gibt einen logischen Vektor [ TRUE, FALSE, FALSE, ... ]
## zurueck, der fuer jede Zeile des Datensatzes angibt, ob die Rassen uebereinstimmen
## tabelliere diesen Vektor und bilde die beiden Verhaeltnisse
## weise das Ergebnis same.race zu und zeige es sofort an, deshalb die aeusseren ( )
( same.race <- prop.table( table( babies$race == babies$drace ) ) )

## augenfreundlichere Variante
paste0( round( same.race * 100, 2 ), '%' )
