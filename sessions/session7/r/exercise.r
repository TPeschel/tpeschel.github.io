## Session 7

## 

library( readxl )

setwd( "~/Dokumente/RLearning/githubRepos/tpeschel.github.io/sessions/session7/r/" )

babies <- read_excel( "../../session6/data/babies.xlsx", 1 )

babies$sex.mf <- factor( babies$sex, levels = c( 0, 1, 2 ), labels = c( "male", "female", "transsexual" ) )
babies$sex.mf
