###Load Libraries

library(nflfastR)
library(tidyverse)
library(ggrepel)

###Load play by play data
seasons<- 2022
pbp<- read.csv("pbp_history.csv")%>%
  filter(season==seasons)


#colr<-teams_colors_logos%>% select(team_abbr,team_color, team_color2,team_logo_espn)

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
    pointsround = round(points2,0),
    Adjustment= if_else(points3>0,"Positive","Negative"),
    abs_adj=abs(points3)
  )%>%
  filter(attempts>3)


avgqbadj<-round(mean(qbstats$points3),2 )
avgqbcomp<-round(mean(qbstats$comp_pct),2)

ggplot(qbstats,aes(x=points3,y=comp_pct)) +
  geom_point()+
  geom_hline(yintercept=mean(qbstats$comp_pct),color="red",linetype="dashed")+
  geom_vline(xintercept=avgqbadj,color="blue",linetype="dashed")+
  ggtitle("Quarterback Points Adjustment 2022")+
  xlab("Point Adjustment")+
  ylab("Completion Percentage")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       subtitle="per game")+
  geom_text(aes(0,avgqbadj ,label=paste("AVG Adj",avgqbadj) ,vjust=0, hjust=1),check_overlap = TRUE)+
  geom_text(aes(10,avgqbcomp ,label=paste("AVG Comp",avgqbcomp*100,"%"),vjust=-1,hjust=5.5),check_overlap = TRUE)+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18),
        plot.subtitle = element_text(hjust=.5,face="italic",size=10))+
  scale_y_continuous(labels=scales::percent, limits=c(0,1))+
  scale_x_continuous(limits=c(-25,25))
  dev.print(file="charts/QBAjustPerGame.png",device=png, height=600,width=800)
  dev.off()

ggplot(qbstats,aes(x=attempts,y=completions))+
  geom_point(aes(color=Adjustment, size=abs_adj), alpha=.5)+
  stat_smooth(method="lm",col="red")+
  geom_text(aes(20,35,label=paste("Average QB Completions =",avgqbcomp*100,"%")),color="red", check_overlap = TRUE)+
  scale_color_manual(values=c("red3","forestgreen"))+
  scale_alpha(guide=FALSE)+
  ggtitle("Quarterback Points Adjustment 2022")+
  xlab("Pass Attempts")+
  ylab("Completions")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       size="Points")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18))+
  scale_y_continuous( limits=c(0,65))+
  scale_x_continuous(limits=c(0,65))
dev.print(file="charts/QBCompPerGame.png",device=png, height=600,width=800)
dev.off()




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
    adj = (-.66*target)+(1*reception),
    points = (-.66*target)+(2*reception),
    comp_pct = reception/target,
    pointsround = round(points,0),
    Adjustment= if_else(adj>0,"Positive","Negative"),
    abs_adj=abs(adj)
  )%>%
  filter(target>3, receiver_player_name!="NA")

avgwradj<-round(mean(wrstats$adj),2 )
avgwrcomp<-round(mean(wrstats$comp_pct),2)

ggplot(wrstats,aes(x=adj,y=comp_pct)) +
  geom_point()+
  geom_hline(yintercept=mean(wrstats$comp_pct),color="red",linetype="dashed")+
  geom_vline(xintercept=avgwradj,color="blue",linetype="dashed")+
  ggtitle("Reciever Points Adjustment 2022")+
  xlab("Point Adjustment")+
  ylab("Completion Percentage When Targeted")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       subtitle="per game")+
  geom_text(aes(0,avgwradj ,label=paste("AVG Adj",avgwradj) ,vjust=0, hjust=1),check_overlap = TRUE)+
  geom_text(aes(10,avgwrcomp ,label=paste("AVG Comp",avgwrcomp*100,"%"),vjust=-1,hjust=8),check_overlap = TRUE)+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18),
        plot.subtitle = element_text(hjust=.5,face="italic",size=10))+
  scale_y_continuous(labels=scales::percent, limits=c(0,1))+
  scale_x_continuous(limits=c(-8,8))
dev.print(file="charts/WRAjustPerGame.png",device=png, height=600,width=800)
dev.off()



ggplot(wrstats,aes(x=target,y=reception))+
  geom_point(aes(color=Adjustment, size=abs_adj), alpha=.5)+
  stat_smooth(method="lm",col="red")+
  geom_text(aes(5,10,label=paste("Average WR Completions =",avgwrcomp*100,"%")),color="red", check_overlap = TRUE)+
  scale_color_manual(values=c("red3","forestgreen"))+
  scale_alpha(guide=FALSE)+
  ggtitle("Receiver Points Adjustment 2022")+
  xlab("Targets")+
  ylab("Receptions")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       size="Points")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18))+
  scale_y_continuous( limits=c(0,20))+
  scale_x_continuous(limits=c(0,20))
dev.print(file="charts/WRCompPerGame.png",device=png, height=600,width=800)
dev.off()

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
    pts_adj= (yards*.06)+(attempt*-.26),
    Adjustment= if_else(pts_adj>0,"Positive","Negative"),
    abs_adj=abs(pts_adj)
  )%>%
  filter(attempt>3, rusher_player_name!="NA")

rbypa<-round(sum(rbstats$yards)/sum(rbstats$attempt),2)
rbavgadj<-round(mean(rbstats$pts_adj),2)

ggplot(rbstats,aes(x=pts_adj,y=ydsperattpt)) +
  geom_point()+
  geom_hline(yintercept=rbypa,color="red",linetype="dashed")+
  geom_vline(xintercept=rbavgadj,color="blue",linetype="dashed")+
  ggtitle("Rusher Points Adjustment 2022")+
  xlab("Point Adjustment")+
  ylab("Yards per Attempt")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       subtitle="per game")+
  geom_text(aes(0,rbavgadj ,label=paste("AVG Adj",rbavgadj) ,vjust=0, hjust=1),check_overlap = TRUE)+
  geom_text(aes(-3,rbypa ,label=paste("AVG YPA",rbypa),vjust=0,hjust=0),check_overlap = TRUE)+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18),
        plot.subtitle = element_text(hjust=.5,face="italic",size=10))+
  scale_y_continuous(limits=c(-5,25))+
  scale_x_continuous(limits=c(-5,5))
dev.print(file="charts/RBAjustPerGame.png",device=png, height=600,width=800)
dev.off()



ggplot(rbstats,aes(x=attempt,y=yards))+
  geom_point(aes(color=Adjustment, size=abs_adj), alpha=.5)+
  stat_smooth(method="lm",col="red")+
  geom_text(aes(5,10,label=paste("Average RB Completions =",avgwrcomp*100,"%")),color="red", check_overlap = TRUE)+
  scale_color_manual(values=c("red3","forestgreen"))+
  scale_alpha(guide=FALSE)+
  ggtitle("Rusher Points Adjustment 2022")+
  xlab("Attempts")+
  ylab("Yards")+
  labs(caption= "Made by @BSchiwal, Datasource = @NFLFastr",
       size="Points")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18))+
  scale_y_continuous( limits=c(0,200))+
  scale_x_continuous(limits=c(0,40))
dev.print(file="charts/RBCompPerGame.png",device=png, height=600,width=800)
dev.off()


rm(rbstats,wrstats,qbstats,avgqbadj,avgqbcomp,avgwradj,avgwrcomp,rbavgadj,rbypa)

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
    rush.pts_yards = yards*.16,
    rush.pts_attempt = attempt*-.26,
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
rm(pbp)
###Build Combined Table#####
###Create Combined Table per Game
players<-read.csv("player_history.csv")%>%
  filter(season==seasons)
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

rm(seasons,join_step1,join_step2,join_step3,join_step4,join_step5, 
   players,passers,rushers,receiver,ffplayer,
   passing_pts_pg,passing_pts_tot,recieving_pts_pg,
   recieving_pts_tot,rushing_pts_pg,rushing_pts_tot)

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

ggplot(ffpts_pg,aes(y=pts_gm,x=position, fill=position))+geom_point()+
  scale_y_continuous(limits=c(-20,700))

ggplot(ffpts_tot,aes(y=pts_tot,x=pts_avg, fill=position))+
  geom_point(aes(colour=position))+
  scale_y_continuous(limits=c(0,500))+
  scale_x_continuous(limits=c(0,40))+
  ggtitle("2022 Fantasy Points per Player")+
  xlab("Avg Points per Game")+
  ylab("Total Season Points")+
  labs(subtitle="New Scoring Method",
       caption= "Made by @BSchiwal, Datasource = @NFLFastr", 
       position="Position")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18),
        plot.subtitle=element_text(hjust=.5,size=10))
dev.print(file="charts/PlayerScoresNew.png",device=png, height=600,width=800)
dev.off()


ggplot(ffpts_tot,aes(y=pts_std_tot,x=pts_std_avg, fill=position))+
  geom_point(aes(colour=position))+
  scale_y_continuous(limits=c(0,500))+
  scale_x_continuous(limits=c(0,40))+
  ggtitle("2022 Fantasy Points per Player")+
  xlab("Avg Points per Game")+
  ylab("Total Season Points")+
  labs(subtitle="Standard PPR Scoring Method",
       caption= "Made by @BSchiwal, Datasource = @NFLFastr", 
       position="Position")+
  theme(plot.title=element_text(hjust=.5, face="bold", size=18),
        plot.subtitle=element_text(hjust=.5,size=10))
dev.print(file="charts/PlayerScoresStdPPR.png",device=png, height=600,width=800)
dev.off()

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

write.csv(fftoppts,"data/ff_top_players_points.csv", row.names=FALSE)
write.csv(ffpts_tot,"data/ff_allplayer_totalseason_points.csv",row.names=FALSE)
write.csv(ffpts_pg,"data/ff_allplayer_pergame_points.csv",row.names=FALSE)

ggplot(fftoppts,aes(y=pts_tot,x=pts_avg, fill=position))+
  geom_point(aes(colour=position))+
  scale_y_continuous(limits=c(100,515))+
  scale_x_continuous(limits=c(10,31))
ggplot(fftoppts,aes(y=pts_std_tot,x=pts_std_avg, fill=position))+
  geom_point(aes(colour=position))+
  scale_y_continuous(limits=c(100,515))+
  scale_x_continuous(limits=c(10,31))

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
