### Load Season Data and save as CSV.
library(dplyr)
library(nflfastR)
seasons<- c(2018:2023)

###Download Historical Season and Player Data
pbp<- nflfastR::load_pbp(seasons)
players<-fast_scraper_roster(seasons)


###Write history to CSV file
write.csv(pbp,"data/pbp_history.csv", row.names=FALSE)
write.csv(players,"data/player_history.csv",row.names=FALSE)

###Remove Environment
rm(pbp,players,seasons)



