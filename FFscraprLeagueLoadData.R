
###This R file serves to analyze the 2022 fantasy league to determine current
# roster keeper values. 
#Currently testing against last year's office league

require(ffscrapr)
require(tidyverse)


##Connect to ESPN account, S2 and SWID values are stored in R environment, 
#used process from https://ffscrapr.ffverse.com/articles/espn_authentication.html

#Store Connection values for multiple leagues
witty<- espn_connect(
  season=2022,
  league_id = 680444613,
  espn_s2 = Sys.getenv("TAN_ESPN_S2"),
  swid = Sys.getenv("TAN_SWID")
)

#office test league
office<- espn_connect(
  season=2021,
  league_id = 41375692,
  espn_s2 = Sys.getenv("TAN_ESPN_S2"),
  swid = Sys.getenv("TAN_SWID")
)


##Set which league connection to pull data from
league<-witty

###Get list of Suspended Players (for later calculation of keepr values)


susp_players<-espn_players(witty)%>%
  filter(
    (str_detect(player_name,"Hopkins")& team=="ARI") |
    ( str_detect(player_name,"Watson") & team=="CLE")
  )%>%
  mutate(suspension= case_when(team=="ARI" ~ 6,
                               team=="CLE" ~ 11)
  )


#Get general League info
league_info<-ff_league(league)
#get transaction history, draft details, and rosters from league, Add trans_date to remove time
trans<-ff_transactions(league)
  trans$trans_date<-as.Date(trans$timestamp)
draft<-ff_draft(league)
  draft$trans_date<-as.Date(draft$complete_date)
roster<-ff_rosters(league)
  roster$trans_date<-as.Date(roster$acquisition_date)

  
##Write season draft as CSV file.Only done after draft, commenting out to save historic information
#write.csv(wit.draft, file="data/2022draft.csv", row.names = TRUE) 


##Get values for rostered players currently drafted
roster_drafted <- roster%>%
  filter(acquisition_type=="DRAFT")%>%
  left_join(select(draft,
                   franchise_id,
                   player_id,
                   bid_amount),
            by= c("franchise_id","player_id"))%>%
  rename("player_value"="bid_amount")

##Get values for players added off waivers
roster_waiver <- roster%>%
  filter(acquisition_type=="ADD")%>%
  left_join(select(trans,
                   franchise_id,
                   player_id,
                   trans_date,
                   bbid_spent),
            by= c("franchise_id","player_id","trans_date"))%>%
  rename("player_value"="bbid_spent")


##Get Values for traded players
#make master transaction List
master_trans<- 
  right_join( 
    #Transactions do not include Draft Data. Combining draft and Transaction in Union(), 
    #inserting 'Draft' as type column and renaming bid to columns to common name and 
    #then joining to transaction table
    trans,
    union(
      draft%>%
    select(franchise_id,player_id,trans_date,bid_amount)%>%
      mutate(type="DRAFT")%>%
    rename("player_value"="bid_amount"),
    trans%>%
      select(franchise_id,player_id,trans_date,bbid_spent,type)%>%
      rename("player_value"="bbid_spent"))
  )

##Get Trade List
trades_master<- master_trans%>%
  filter(type=="TRADE"|is.na(type)==TRUE)

##For certain trades with more than one player on each side of the transaction we need to manually define which players values align
trade_manual_id<- c(
  12483,   #Matthew Stafford
  3916430, #Nyeem Heins
  3918298, #Josh Allen
  3925347, #Damian Harris
  4039050, #Devin Duvarnay
  4360438 #Brandon Aiyk

  )
trade_manual_index<-c(1,2,1,2,3,3)
trade_manual<-data.frame(trade_manual_index,trade_manual_id)
rm(trade_manual_id,trade_manual_index)

###To get the base value of traded players, we first need to get the values of players before the traded transaction
##for now I am doing this by filtering the master transaction dataset to drop all trade and 'dropped' line items and then take the max transactiond date
##this will break when someone trades a player that they recieved on a trade in the same season and will need to be redone or manually passed in


trade_value<-
  left_join(
    master_trans%>% ##Master trans most recent transaction for each player and franchise group
      filter(type!="TRADE")%>%
      filter(type!="DROP") %>%
      filter(type_desc!="dropped" | is.na(type_desc==TRUE))%>%
      select(player_id,franchise_id,trans_date)%>%
      group_by(player_id)%>%
      slice_max(trans_date),
    master_trans%>%  ##Pull in details for each group from base dataset
      filter(type!="TRADE")%>%
      filter(type!="DROP") %>%
      filter(type_desc!="dropped" | is.na(type_desc==TRUE)),
    by= c("player_id","franchise_id","trans_date")) 


##Create a frame with all the players a given team recieved and add grouping index to align player values
trade_given<- master_trans%>%
  filter(type=="TRADE")%>%
  rename("player_id_given"="player_id","player_name_given"="player_name","team_given"="team","pos_given"="pos")%>%
  mutate(trade_id= paste(trans_date,"-",franchise_id))%>%
  select(-c(timestamp,type_desc,bbid_spent,player_value))%>%
  group_by(trade_id)%>%
  mutate(counter=row_number(trade_id))%>%
  ungroup(trade_id)%>%
  select(-c(trade_id,counter))%>%
  left_join(trade_manual,by=c("player_id_given"="trade_manual_id"))%>%
  left_join(trade_value%>%
              select(player_id,franchise_id,player_value),
            by=c("player_id_given"="player_id","franchise_id"="franchise_id"))


##Create a frame with all the players a team gave up and add grouping index to align player values  
trade_recieved<-master_trans%>%
  filter(is.na(type)==TRUE)%>%
  rename("player_id_recieved"="player_id","player_name_recieved"="player_name","team_recieved"="team","pos_recieved"="pos")%>%
  mutate(trade_id= paste(trans_date,"-",franchise_id))%>%
  select(-c(timestamp,type_desc,bbid_spent,player_value,type))%>%
  group_by(trade_id)%>%
  mutate(counter=row_number(trade_id))%>%
  ungroup(trade_id)%>%
  select(-c(trade_id,franchise_name,trade_partner,counter))%>%
  left_join(trade_manual,by=c("player_id_recieved"="trade_manual_id"))

##Players without a defined index recieve an index of 1
trade_given[is.na(trade_given)] <- 1
trade_recieved[is.na(trade_recieved)] <- 1

##Join Given and Received frames
trade<- left_join(
  trade_recieved,trade_given,
                  by=c("franchise_id"="franchise_id", "trans_date"="trans_date","trade_manual_index"))

###This section accounts for trades where each side of the group index has an uneven amount of players

##First look group the given side of the transaction and split player amount by number of players received when given player is duplicated
trade_uneven_given<- trade%>%
  group_by(franchise_id,player_id_given,trade_manual_index)%>%
  summarize(players_given = 1,
            players_recieved = n(),
            player_value_given = first(player_value),
            player_value_split = ceiling(player_value_given/players_recieved))

##then group the received side and sum all given values when recieved player is duplicated
trade_uneven_recieved<-trade%>%
  group_by(franchise_id,player_id_recieved,trade_manual_index)%>%
  summarize(players_recieved = 1,
            players_given=n(),
            player_value_total = sum(player_value))

##join datasets and group again by player recieved, then take total for all rows unless recieved more players than gave, then take split value.
trade_final<-left_join(trade_uneven_recieved%>%
                          select(-players_recieved),
                        trade_uneven_given%>%
                          select(-players_given),
                        by=c("franchise_id","trade_manual_index"))%>%
  group_by(franchise_id,player_id_recieved)%>%
  summarize(given = first(players_given),
            recieved = first(players_recieved),
            value_total = first(player_value_total),
            value_split = first(player_value_split),
            player_value = if_else(given>1,value_total,as.integer(value_split))
              )%>%
  select(-given,-recieved,-value_total,-value_split)%>%
  rename("player_id"="player_id_recieved")

rm(trade_uneven_given,trade_uneven_recieved)              


##rebuild trade dataset with values


rm(trade_value,trade_given,trade_recieved,trade_manual)


roster_trade<- roster%>%
  filter(acquisition_type=="TRADE")%>%
  left_join(tradefinal,
            by=c("player_id","franchise_id"))
rm(trade,tradefinal)
###Join Values to Roster
roster_final<-union_all(roster_drafted,roster_waiver)%>%
  union_all(roster_trade)%>%
  arrange(franchise_id,pos)

###test if initial roster and final build of roster are same size 
{stopifnot(count(roster)==count(roster_final))
roster<-roster_final

###clear environment except roster, trade, and suspension data
rm(list=setdiff(ls(),c("roster", "trades_master","susp_players")))
}

###Calculate Roster Keeper Values


roster<-roster%>%
  left_join((susp_players%>% #Add suspension to player roster
              select(player_id,suspension)),
            by="player_id")%>%
  mutate(suspension = coalesce(suspension,0), #Replace NA suspension with 0
         susp_factor = 1-(suspension/17), #Calculate percent of games played for year
         keeper_base = round(player_value/susp_factor,0), # Gross up player salary to full season value
         keeper_value = if_else(round(keeper_base*1.15,0)<10,  #Calculate keeper value
                                10, 
                                round(keeper_base*1.15,0)) #Greater of 10 or a 15% increase in salary
  )%>%
  select(-c(eligible_pos,acquisition_date))

## Write data to CSV file 
write.csv(roster, file="data/keepervalues.csv", row.names=TRUE)


