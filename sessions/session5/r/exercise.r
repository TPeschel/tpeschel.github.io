library( ggplot2 )

gardens <- read.table( "~/Dokumente/RLearning/githubRepos/tpeschel.github.io/sessions/session5/session5dat/gardens2.txt", header = T )
head( gardens )

ggplot( gardens, aes( index, ozone ) ) + theme_bw( ) + geom_point( )


ozone.mean <- mean( gardens$ozone )

SSY <- sum( ( gardens$ozone-ozone.mean ) ^ 2 )
SSY

mm <- lm( gardens$ozone ~ gardens$garden )
mm
summary( mm )

anova( mm )
