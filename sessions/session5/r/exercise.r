library( ggplot2 )

library( UsingR )

t.test( babies$age, babies$dage )  

t.test( babies$age, babies$dage, alternative = 'less' )  

t.test( babies$age, babies$dage, var.equal = T )  

t.test( babies$age, babies$dage, alternative = 'less', var.equal = T )  






gardens <- read.table( "~/Dokumente/RLearning/githubRepos/tpeschel.github.io/sessions/session5/session5dat/gardens2.txt", header = T )
head( gardens )

ggplot( gardens, aes( index, ozone ) ) + theme_bw( ) + geom_point( )


garden.a <- gardens[ gardens$garden == 'a', ]
garden.b <- gardens[ gardens$garden == 'b', ]

( ozone.mean <- mean( gardens$ozone ) )
( ozone.var <- var( gardens$ozone ) )

( ozone.mean.a <- mean( garden.a$ozone ) )
( ozone.var.a <- var( garden.a$ozone ) )

( ozone.mean.b <- mean( garden.b$ozone ) )
( ozone.var.b <- var( garden.b$ozone ) )

( SSY <- sum( ( gardens$ozone-ozone.mean ) ^ 2 ) )

( SSE <- ( sum( ( garden.a$ozone-ozone.mean.a ) ^ 2 ) ) + ( sum( ( garden.b$ozone-ozone.mean.b ) ^ 2 ) ) )

( SSA <- SSY - SSE )


mm <- lm( gardens$ozone ~ gardens$garden )
mm
summary( mm )

am <- anova( mm )

ggplot( gardens ) + geom_boxplot( aes( garden, ozone, group = garden) )

