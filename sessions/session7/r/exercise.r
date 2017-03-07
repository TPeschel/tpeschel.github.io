## Session 7

## 

library( readxl )

setwd( "~/Dokumente/RLearning/githubRepos/tpeschel.github.io/sessions/session7/r/" )

babies <- read_excel( "../../session6/data/babies.xlsx", 1 )

babies$sex.mf <- factor( babies$sex, levels = c( 0, 1, 2 ), labels = c( "male", "female", "transsexual" ) )

table( babies$sex.mf )

babies$inc <- factor(babies$inc,
                     levels = c(0:9,98,99),
                     labels = c("<2500",
                                paste(seq(2500,20000,by = 2500),
                                      seq(4999,22500,by = 2500),sep = "-"),
                                "25000+","unknown","not asked"))