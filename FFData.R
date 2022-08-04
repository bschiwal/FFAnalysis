###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggrepel)

###Load play by play data
seasons<- 2021
pbp<- read.csv("pbp_history.csv")%>%
  filter(season==seasons)


colr<-teams_colors_logos%>% select(team_abbr,team_color, team_color2,team_logo_espn)

#Background Analysis####
###Analyse QB Stats
qbstats <-pbp%>% 
  filter(
    play_type=="pass",
    pass_attempt == 1
  )%>%
  group_by(game_id,passer_player_name)%>%
  summarise(
    attempts = sum(pass_attempt),
    completions = sum(complete_pass),
    incompetions = sum(incomplete_pass),
    comp_pct= completions/attempts,
    points = (.4*attempts)+(-1*incompetions),
    points2 = (.6*completions)+(-1*incompetions),
    points3 = (attempts*-1) + (completions*1.65),
    pointsround = round(points2,0)
  )%>%
  filter(attempts>3)

ggplot(qbstats,aes(x=points3,y=comp_pct)) +
  geom_point()+
  geom_hline(yintercept=.6,color="red",linetype="dashed")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  geom_hline(yintercept=mean(qbstats$comp_pct),color="blue",linetype="solid")+
  geom_vline(xintercept=mean(qbstats$points3),color="blue",linetype="solid")

mean(qbstats$points3)               

ggplot(qbstats,aes(x=points3,y=comp_pct)) +
  geom_point()


###Analyze WR Stats
wrstats <-pbp%>% 
  filter(
    play_type=="pass",
    pass_attempt == 1
  )%>%
  group_by(game_id,receiver_player_name)%>%
  summarise(
    target = sum(pass_attempt),
    reception = sum(complete_pass),
    dropped_ball = sum(incomplete_pass),
    points = (-.66*target)+(2*reception),
    comp_pct = reception/target,
    pointsround = round(points,0)
  )%>%
  filter(target>3, receiver_player_name!="NA")

ggplot(wrstats,aes(x=points,y=comp_pct)) +
  geom_point()+
  ##geom_hline(yintercept=.6,color="red",linetype="dashed")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  geom_hline(yintercept=mean(wrstats$comp_pct),color="blue",linetype="solid")+
  geom_vline(xintercept=mean(wrstats$points),color="blue",linetype="solid")

ggplot(wrstats,aes(x=reception,y=comp_pct)) +
  geom_point()+
  ##geom_hline(yintercept=.6,color="red",linetype="dashed")+
  geom_vline(xintercept=0,color="red",linetype="dashed")+
  geom_hline(yintercept=mean(wrstats$comp_pct),color="blue",linetype="solid")+
  geom_vline(xintercept=mean(wrstats$points),color="blue",linetype="solid")


mean(wrstats$comp_pct)


##Analyze RB Stats

rbstats <-pbp%>% 
  filter(
    play_type=="run",
    rush_attempt == 1
  )%>%
  group_by(game_id, rusher_player_id,rusher_player_name)%>%
  summarise(
    attempt = sum(rush_attempt, na.rm=TRUE),
    yards = sum(rushing_yards, na.rm=TRUE),
    td = sum(rush_touchdown),
    ydsperattpt = yards/attempt,
    pts = (yards*.21)+(attempt*-.5),
    pts_adj= (yards*.11)+(attempt*-.5)
  )%>%
  filter(attempt>3, rusher_player_name!="NA")

mean(rbstats$yardsperattpt)
ypa<-sum(rbstats$yards)/sum(rbstats$attempt)
ggplot(rbstats,aes(x=pts_adj,y=pts))+geom_point()+
  geom_hline(yintercept=mean(ypa),color="blue",linetype="solid")+
  geom_vline(xintercept=mean(rbstats$yards),color="blue",linetype="solid")

rm(rbstats,wrstats,qbstats)

###Build Points Tables####
###Develop Passing Points Table Per Game
passing_pts_pg <-pbp%>% 
  filter(
    play_type=="pass",
    pass_attempt == 1,
    season_type=="REG"
  )%>%
  mutate(
    two_pt_rslt_bianary = ifelse(two_point_conv_result %in%"success", 1,0)
  )%>%
  group_by(game_id,passer_player_name, passer_player_id)%>%
  summarise(
    game = 1,
    attempts = sum(pass_attempt),
    completions = sum(complete_pass),
    incompetions = sum(incomplete_pass),
    yards = sum(passing_yards, na.rm=TRUE),
    touchdown = sum(pass_touchdown),
    sack = sum(sack),
    interception = sum(interception),
    conversion = sum(two_pt_rslt_bianary),
    comp_pct= completions/attempts,
    pass.pts_cmppct = (attempts*-1) + (completions*1.65),
    pass.pts_yards = yards*.04,
    pass.pts_td = touchdown*6,
    pass.pts_int = interception*-2,
    pass.pts_two = 0, ##conversion*2,
    pass.pts_sack = sack*-1, 
    pass.pts_passing = pass.pts_cmppct+pass.pts_yards+pass.pts_td+pass.pts_int+pass.pts_two+pass.pts_sack,
    pass.pts_passing_std = pass.pts_passing - pass.pts_cmppct - pass.pts_sack,
    pass.pts_diff = pass.pts_passing-pass.pts_passing_std
  )%>%
  rename(gsis_id=passer_player_id,name=passer_player_name)

###Convert to Passing Points Total Table
passing_pts_tot <-passing_pts_pg%>%
  group_by(gsis_id)%>%
  summarise(
    name = first(name),
    games = sum(game),
    attempts = sum(attempts),
    pass.avg_pts = mean(pass.pts_passing),
    pass.avg_pts_std = mean(pass.pts_passing_std),
    pass.avg_adj = mean(pass.pts_diff),
    pass.pts_tot = sum(pass.pts_passing),
    pass.pts_tot_std = sum(pass.pts_passing_std),
    pass.tot_adj = sum(pass.pts_diff)
  )



###Create Receiving Points Table
recieving_pts_pg <-pbp%>% 
  filter(
    play_type=="pass",
    pass_attempt == 1,
    season_type=="REG"
  )%>%
  group_by(game_id,receiver_player_name, receiver_player_id)%>%
  summarise(
    game=1,
    target = sum(pass_attempt),
    yards = sum(receiving_yards, na.rm=TRUE),
    reception = sum(complete_pass),
    dropped_ball = sum(incomplete_pass),
    td = sum(pass_touchdown),
    points = (-.66*target)+(2*reception),
    comp_pct = reception/target,
    rec.pts_yards = yards*.1,
    rec.pts_ppr = reception*2,
    rec.pts_target = target*-.66,
    rec.pts_td = td*6,
    rec.pts_tot = rec.pts_yards+rec.pts_ppr+rec.pts_target+rec.pts_td,
    rec.pts_std = rec.pts_tot-rec.pts_target-(rec.pts_ppr/2),
    rec.pts_adj = rec.pts_target+(rec.pts_ppr/2)
  )%>%
  filter(receiver_player_name!="NA")%>%
  rename(gsis_id=receiver_player_id,name=receiver_player_name)

### add Receiving Points Total Table
recieving_pts_tot <-recieving_pts_pg %>%
  group_by(gsis_id)%>%
  summarise(
    name = first(name),
    games = sum(game),
    targets = sum(target),
    rec.avg_pts = mean(rec.pts_tot),
    rec.avg_pts_std = mean(rec.pts_std),
    rec.avg_adj = mean(rec.pts_adj),
    rec.tot_pts = sum(rec.pts_tot),
    rec.tot_pts_std = sum(rec.pts_std),
    rec.tot_adj = sum(rec.pts_adj)
  )

###Add Rushing Points Per Game table
rushing_pts_pg <-pbp%>% 
  filter(
    play_type=="run",
    rush_attempt == 1,
    season_type=="REG"
  )%>%
  group_by(game_id, rusher_player_id,rusher_player_name)%>%
  summarise(
    attempt = sum(rush_attempt, na.rm=TRUE),
    yards = sum(rushing_yards, na.rm=TRUE),
    td = sum(rush_touchdown),
    rush.pts_yards = yards*.15,
    rush.pts_attempt = attempt*-.25,
    rush.pts_td = td*6,
    rush.pts_tot = rush.pts_yards+rush.pts_td,
    rush.pts_std = (yards*.1)+rush.pts_td,
    rush.pts_adj = rush.pts_tot-rush.pts_std
  )%>%
  rename(gsis_id=rusher_player_id,name=rusher_player_name)

### add Rushing Points Total Table
rushing_pts_tot <- rushing_pts_pg%>%
  group_by(gsis_id)%>%
  summarise(
    name= first(name),
    rush.avg_pts = mean(rush.pts_tot),
    rush.avg_pts_std = mean(rush.pts_std),
    rush.avg_adj = mean(rush.pts_adj),
    rush.tot_pts = sum(rush.pts_tot),
    rush.tot_pts_std = sum(rush.pts_std),
    rush.tot_adj = sum(rush.pts_adj)
  )
###Build Combined Table#####
###Create Combined Table per Game
players<-fast_scraper_roster(seasons)
passers<-passing_pts_pg%>%
  select(gsis_id,name,game_id)

rushers<-rushing_pts_pg%>%
  select(gsis_id,name, game_id)
receiver<- recieving_pts_pg%>%
  select(gsis_id,name, game_id)

ffplayer<-passers%>%union_all(rushers)%>%
  union_all(receiver)%>%
  group_by(gsis_id, game_id)%>%
  summarise(name=first(name))

join_step1<- left_join(ffplayer,passing_pts_pg,
                       by=c("gsis_id","game_id"),
                       suffix=c("",".p"))

join_step2<-left_join(join_step1,recieving_pts_pg,
                      by=c("gsis_id","game_id"),
                      suffix=c("",".rc"))
join_step3<-left_join(join_step2,rushing_pts_pg,
                      by=c("gsis_id","game_id"),
                      suffix=c("",".rs"))
join_step4<-left_join(join_step3,players,
                      by=c("gsis_id"))
join_step5<-join_step4%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

ffpts_pg<- join_step5%>%
  filter(position=="QB" | position=="WR" | position=="TE" | position=="RB" |position=="FB")%>%
  group_by(game_id,gsis_id,name,position)%>%
  summarise(
    game=1,
    pts_gm= pass.pts_passing+rush.pts_tot+rec.pts_tot,
    pts_gm_std= pass.pts_passing_std+rush.pts_std+rec.pts_std,
    pts_gm_adj= pass.pts_diff+rush.pts_adj+rec.pts_adj
  )

rm(join_step1,join_step2,join_step3,join_step4,join_step5, 
   players,passers,rushers,receiver,ffplayer,
   passing_pts_pg,passing_pts_tot,recieving_pts_pg,recieving_pts_tot,rushing_pts_pg,rushing_pts_tot)

###Condense Final Table to Total Season
ffpts_tot<- ffpts_pg%>%
  group_by(gsis_id)%>%
  summarise(
    name= first(name),
    position=first(position),
    games=sum(game),
    pts_tot= sum(pts_gm),
    pts_avg= mean(pts_gm),
    pts_std_tot = sum(pts_gm_std),
    pts_std_avg = mean(pts_gm_std),
    pts_adj_tot = sum(pts_gm_adj),
    pts_adj_avg = mean(pts_gm_adj)
  )


ggplot(ffpts_tot,aes(y=pts_tot,x=position, fill=position))+geom_point()+
  scale_y_continuous(limits=c(-20,700))
ggplot(ffpts_tot,aes(y=pts_std_tot,x=position, fill=position))+geom_point()+
  scale_y_continuous(limits=c(-20,700))

ggplot(ffpts_tot,aes(y=pts_tot,x=position, fill=position))+geom_boxplot()+
  scale_y_continuous(limits=c(0,600))
ggplot(ffpts_tot,aes(y=pts_std_tot,x=position, fill=position))+geom_boxplot()+
  scale_y_continuous(limits=c(0,600))


####Build Top Position Table####
topqb<-ffpts_tot%>%
  filter(position=="QB", games>8.5)%>%
  arrange(desc(pts_avg))%>%
  slice(1:10)%>%
  mutate(
    fs_pts_est = pts_avg*17,
    pos_rank = rank(-fs_pts_est,ties.method = "first")
  )
topte<-ffpts_tot%>%
  filter(position=="TE", games>8.5)%>%
  arrange(desc(pts_avg))%>%
  slice(1:10)%>%
  mutate(
    fs_pts_est = pts_avg*17,
    pos_rank = rank(-fs_pts_est,ties.method = "first")
  )

toprb<-ffpts_tot%>%
  filter(position=="RB", games>8.5)%>%
  arrange(desc(pts_avg))%>%
  slice(1:25)%>%
  mutate(
    fs_pts_est = pts_avg*17,
    pos_rank = rank(-fs_pts_est,ties.method = "first")
  )
topwr<-ffpts_tot%>%
  filter(position=="WR", games>8.5)%>%
  arrange(desc(pts_avg))%>%
  slice(1:25)%>%
  mutate(
    fs_pts_est = pts_avg*17,
    pos_rank = rank(-fs_pts_est,ties.method = "first")
  )

fftoppts<-topqb%>%
  union_all(topwr)%>%
  union_all(toprb)%>%
  union_all(topte)

rm(topqb,topwr,toprb,topte)

ggplot(fftoppts,aes(y=fs_pts_est,x=position))+
  geom_point(data=fftoppts,aes(fill=position))+
  scale_y_continuous(limits=c(0,700))
ggplot(fftoppts,aes(y=fs_pts_est,x=position))+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,700))

ggplot(fftoppts,aes(y=pts_std_tot,x=position, fill=position))+geom_point()+
  scale_y_continuous(limits=c(0,700))

ggplot(fftoppts, aes(y=pts_avg,x=pts_std_avg, group=position))+
  geom_point(aes(col=position))+
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(0,35))

ggplot(fftoppts, aes(x=position, y=pts_tot))+geom_point()+
  scale_y_continuous(limits=c(0,550))
ggplot(fftoppts, aes(x=position, y=pts_std_tot))+geom_point()+
  scale_y_continuous(limits=c(0,550))

ggplot(ffpts_pg, aes(y=pts_gm,x=pts_gm_adj, group=position))+
  geom_point(aes(col=position))+
  scale_y_continuous(limits=c(-20,70))+
  scale_x_continuous(limits=c(-25,25))

ggplot(fftoppts, aes(y=pts_tot,x=position, group=position))+
  geom_point(aes(col=position))+
  scale_y_continuous(limits=c(100,600))

ggplot(fftoppts, aes(y=pts_std_tot,x=position, group=position))+
  geom_point(aes(col=position))+
  scale_y_continuous(limits=c(100,600))
