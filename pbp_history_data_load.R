### Load Season Data and save as CSV.
library(dplyr)
library(nflfastR)
seasons<- c(2018:2021)

###Download Historical Season and Player Data
pbp<- nflfastR::load_pbp(seasons)
players<-fast_scraper_roster(seasons)


###Write history to CSV file
write.csv(pbp,"pbp_history.csv", row.names=FALSE)
write.csv(players,"player_history.csv",row.names=FALSE)

###Remove Environment
rm(pbp,players,seasons)



