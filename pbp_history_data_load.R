### Load Season Data and save as CSV.
library(dplyr)
library(nflfastR)
seasons<- c(2018:2021)

pbp<- nflfastR::load_pbp(seasons)

##Test phase

##Create test dataframe  hito simulate historic data
#pbph<- data.frame(season = c(2018:2021),
#         game = c(1:4)
#         )
##Create Test Dataframe to simulate current data
#pbpc <- data.frame( season = 2022,
#                    game = 1
#                )
#pbp<-union(pbph,pbpc)

#rm(pbpc,pbph)
###Write history to CSV file
write.csv(pbp,"pbp_history.csv", row.names=FALSE)

###Remove Environment
rm(pbp,seasons)

dataload<- read.csv("pbp_history.csv")


