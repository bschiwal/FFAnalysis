###Load data from ESPN 22 player points projection, top 400 using current scoring method


proj<- read.csv("data/ff_player_22_est.csv")%>%
  mutate(
    games=round(pts_tot/pts_avg,0)
  )
colnames(proj)[1]<-"rank"  


##Separate into Position Groups  
qb <-proj%>% 
  filter(
    position=="QB"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  )
rb <-proj%>% 
  filter(
    position=="RB"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  ) 

wr <-proj%>% 
  filter(
    position=="WR"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  )

te <-proj%>% 
  filter(
    position=="TE"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  )


def <-proj%>% 
  filter(
    position=="D/ST"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  )

k <-proj%>% 
  filter(
    position=="K"
  )%>%
  mutate(
    pos_rank = rank(rank,ties.method = "first")
  )

##Further Separate into Tiers based on 12 team league
qb1<-qb%>%
  slice(1:12)
qb2<-qb%>%
  slice(13:24)

wr1<-wr%>%
  slice(1:12)
wr2<-wr%>%
  slice(13:24)
wr3<-wr%>%
  slice(25:36)
wr4<-wr%>%
  slice(37:48)

rb1<-rb%>%
  slice(1:12)
rb2<-rb%>%
  slice(13:24)
rb3<-rb%>%
  slice(25:36)
rb4<-rb%>%
  slice(37:48)

te1<-te%>%
  slice(1:12)
te2<-te%>%
  slice(13:24)

def1<-def%>%
  slice(1:12)
k1<-k%>%
  slice(1:12)

###Calculate tier means

qb1avg<- mean(qb1$pts_avg)
qb2avg<- mean(qb2$pts_avg)

rb1avg<- mean(rb1$pts_avg)
rb2avg<- mean(rb2$pts_avg)
rb3avg<- mean(rb3$pts_avg)
rb4avg<- mean(rb4$pts_avg)

wr1avg<- mean(wr1$pts_avg)
wr2avg<- mean(wr2$pts_avg)
wr3avg<- mean(wr3$pts_avg)
wr4avg<- mean(wr4$pts_avg)

te1avg<- mean(te1$pts_avg)
te2avg<- mean(te2$pts_avg)

def1avg<- mean(def1$pts_avg)
k1avg<- mean(k1$pts_avg)


###Calculate tier medians
qb1med<- median(qb1$pts_avg)
qb2med<- median(qb2$pts_avg)

rb1med<- median(rb1$pts_avg)
rb2med<- median(rb2$pts_avg)
rb3med<- median(rb3$pts_avg)
rb4med<- median(rb4$pts_avg)

wr1med<- median(wr1$pts_avg)
wr2med<- median(wr2$pts_avg)
wr3med<- median(wr3$pts_avg)
wr4med<- median(wr4$pts_avg)

te1med<- median(te1$pts_avg)
te2med<- median(te2$pts_avg)

def1med<- median(def1$pts_avg)
k1med<- median(k1$pts_avg)


###Max
qb1max<- max(qb1$pts_avg)
qb2max<- max(qb2$pts_avg)

rb1max<- max(rb1$pts_avg)
rb2max<- max(rb2$pts_avg)
rb3max<- max(rb3$pts_avg)
rb4max<- max(rb4$pts_avg)

wr1max<- max(wr1$pts_avg)
wr2max<- max(wr2$pts_avg)
wr3max<- max(wr3$pts_avg)
wr4max<- max(wr4$pts_avg)

te1max<- max(te1$pts_avg)
te2max<- max(te2$pts_avg)

def1max<- max(def1$pts_avg)
k1max<- max(k1$pts_avg)

###Min
qb1min<- min(qb1$pts_avg)
qb2min<- min(qb2$pts_avg)

rb1min<- min(rb1$pts_avg)
rb2min<- min(rb2$pts_avg)
rb3min<- min(rb3$pts_avg)
rb4min<- min(rb4$pts_avg)

wr1min<- min(wr1$pts_avg)
wr2min<- min(wr2$pts_avg)
wr3min<- min(wr3$pts_avg)
wr4min<- min(wr4$pts_avg)

te1min<- min(te1$pts_avg)
te2min<- min(te2$pts_avg)

def1min<- min(def1$pts_avg)
k1min<- min(k1$pts_avg)

##range
qb1range<- qb1max-qb1min
qb2range<- qb2max-qb2min

rb1range<- rb1max-rb1min
rb2range<- rb2max-rb2min
rb3range<- rb3max-rb3min
rb4range<- rb4max-rb4min

wr1range<- wr1max-wr1min
wr2range<- wr2max-wr2min
wr3range<- wr3max-wr3min
wr4range<- wr4max-wr4min

te1range<- te1max-te1min
te2range<- te2max-te2min

def1range<- def1max-def1min
k1range<- k1max-k1min


###add calculated columns on tiered results



qb1<-qb1%>%
  mutate(
    pos = "QB1",
    pos_avg_var = pts_avg-qb1avg,
    pos_med_var = pts_avg-qb1med,
    val_adj_avg=pos_avg_var/qb1avg,
    val_adj_med=pos_med_var/qb1med,
    val1=(1+val_adj_avg)*10,
    val2=(1+val_adj_med)*10,
    val_adj_range= pos_med_var / qb1range,
    val3=(1+val_adj_range)*10,
    avg_val=(val1+val2+val3)/3
  )
qb2<-qb2%>%
  mutate(
    pos = "QB2",
    pos_avg_var = pts_avg-qb2avg,
    pos_med_var = pts_avg-qb2med,
    val_adj_avg=pos_avg_var/qb2avg,
    val_adj_med=pos_med_var/qb2med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / qb2range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

wr1<-wr1%>%
  mutate(
    pos = "wr1",
    pos_avg_var = pts_avg-wr1avg,
    pos_med_var = pts_avg-wr1med,
    val_adj_avg=pos_avg_var/wr1avg,
    val_adj_med=pos_med_var/wr1med,
    val1=(1+val_adj_avg)*55,
    val2=(1+val_adj_med)*55,
    val_adj_range= pos_med_var / wr1range,
    val3=(1+val_adj_range)*55,
    avg_val=(val1+val2+val3)/3
  )
wr2<-wr2%>%
  mutate(
    pos = "wr2",
    pos_avg_var = pts_avg-wr2avg,
    pos_med_var = pts_avg-wr2med,
    val_adj_avg=pos_avg_var/wr2avg,
    val_adj_med=pos_med_var/wr2med,
    val1=(1+val_adj_avg)*24,
    val2=(1+val_adj_med)*24,
    val_adj_range= pos_med_var / wr2range,
    val3=(1+val_adj_range)*24,
    avg_val=(val1+val2+val3)/3
  )

wr3<-wr3%>%
  mutate(
    pos = "wr3",
    pos_avg_var = pts_avg-wr3avg,
    pos_med_var = pts_avg-wr3med,
    val_adj_avg=pos_avg_var/wr3avg,
    val_adj_med=pos_med_var/wr3med,
    val1=(1+val_adj_avg)*10,
    val2=(1+val_adj_med)*10,
    val_adj_range= pos_med_var / wr3range,
    val3=(1+val_adj_range)*10,
    avg_val=(val1+val2+val3)/3
  )
wr4<-wr4%>%
  mutate(
    pos = "wr4",
    pos_avg_var = pts_avg-wr4avg,
    pos_med_var = pts_avg-wr4med,
    val_adj_avg=pos_avg_var/wr4avg,
    val_adj_med=pos_med_var/wr4med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / wr4range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )
rb1<-rb1%>%
  mutate(
    pos = "rb1",
    pos_avg_var = pts_avg-rb1avg,
    pos_med_var = pts_avg-rb1med,
    val_adj_avg=pos_avg_var/rb1avg,
    val_adj_med=pos_med_var/rb1med,
    val1=(1+val_adj_avg)*56,
    val2=(1+val_adj_med)*56,
    val_adj_range= pos_med_var / rb1range,
    val3=(1+val_adj_range)*56,
    avg_val=(val1+val2+val3)/3
  )
rb2<-rb2%>%
  mutate(
    pos = "rb2",
    pos_avg_var = pts_avg-rb2avg,
    pos_med_var = pts_avg-rb2med,
    val_adj_avg=pos_avg_var/rb2avg,
    val_adj_med=pos_med_var/rb2med,
    val1=(1+val_adj_avg)*25,
    val2=(1+val_adj_med)*25,
    val_adj_range= pos_med_var / rb2range,
    val3=(1+val_adj_range)*25,
    avg_val=(val1+val2+val3)/3
  )

rb3<-rb3%>%
  mutate(
    pos = "rb3",
    pos_avg_var = pts_avg-rb3avg,
    pos_med_var = pts_avg-rb3med,
    val_adj_avg=pos_avg_var/rb3avg,
    val_adj_med=pos_med_var/rb3med,
    val1=(1+val_adj_avg)*8,
    val2=(1+val_adj_med)*8,
    val_adj_range= pos_med_var / rb3range,
    val3=(1+val_adj_range)*8,
    avg_val=(val1+val2+val3)/3
  )
rb4<-rb4%>%
  mutate(
    pos = "rb4",
    pos_avg_var = pts_avg-rb4avg,
    pos_med_var = pts_avg-rb4med,
    val_adj_avg=pos_avg_var/rb4avg,
    val_adj_med=pos_med_var/rb4med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / rb4range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

te1<-te1%>%
  mutate(
    pos = "te1",
    pos_avg_var = pts_avg-te1avg,
    pos_med_var = pts_avg-te1med,
    val_adj_avg=pos_avg_var/te1avg,
    val_adj_med=pos_med_var/te1med,
    val1=(1+val_adj_avg)*5,
    val2=(1+val_adj_med)*5,
    val_adj_range= pos_med_var / te1range,
    val3=(1+val_adj_range)*5,
    avg_val=(val1+val2+val3)/3
  )
te2<-te2%>%
  mutate(
    pos = "te2",
    pos_avg_var = pts_avg-te2avg,
    pos_med_var = pts_avg-te2med,
    val_adj_avg=pos_avg_var/te2avg,
    val_adj_med=pos_med_var/te2med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / te2range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

def1<-def1%>%
  mutate(
    pos = "def1",
    pos_avg_var = pts_avg-def1avg,
    pos_med_var = pts_avg-def1med,
    val_adj_avg=pos_avg_var/def1avg,
    val_adj_med=pos_med_var/def1med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / def1range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )
k1<-k1%>%
  mutate(
    pos = "k1",
    pos_avg_var = pts_avg-k1avg,
    pos_med_var = pts_avg-k1med,
    val_adj_avg=pos_avg_var/k1avg,
    val_adj_med=pos_med_var/k1med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / k1range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )


##Join Back into Position Groups
qball<-union(qb1,qb2)

wrall1<-union(wr1,wr2)
wrall2<-union(wrall1,wr3)
wrall<-union(wrall2,wr4)
rm(wrall1,wrall2)

rball1<-union(rb1,rb2)
rball2<-union(rball1,rb3)
rball<-union(rball2,rb4)
rm(rball1,rball2)

teall<-union(te1,te2)

##Join into Player Dataset

player_all<-union(qball,wrall)%>%
  union(rball)%>%
  union(teall)%>%
  union(def1)%>%
  union(k1)%>%
  arrange(rank)

write.csv(player_all,"data/top400calculatedvalues.csv", row.names=FALSE)

##Do some background Statistics, calculate average points per game for median team and percentage of points for each starting position group

totptsmed<-qb1med+wr1med+wr2med+rb1med+rb2med+wr3med+te1med+def1med+k1med
qb1med/totptsmed
rb1med/totptsmed
rb2med/totptsmed  
wr1med/totptsmed
wr2med/totptsmed 
wr3med/totptsmed
te1med/totptsmed 
def1med/totptsmed
k1med/totptsmed 

qb1med-qb2med
wr1med-wr2med
wr2med-wr3med
wr3med-wr4med
rb1med-rb2med
rb2med-rb3med
rb3med-rb4med

te1med-te2med
rm(list=ls())

###Load data from ESPN 22 player points projection, top 400 using current scoring method


proj<- read.csv("data/ff_player_22_est.csv")%>%
  mutate(
    games=round(pts_tot/pts_avg,0)
  )
colnames(proj)[1]<-"rank"  


### Separate into Position Groups  ####
qb <-proj%>% 
  filter(
    position=="QB"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  )
rb <-proj%>% 
  filter(
    position=="RB"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  ) 

wr <-proj%>% 
  filter(
    position=="WR"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  )

te <-proj%>% 
  filter(
    position=="TE"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  )


def <-proj%>% 
  filter(
    position=="D/ST"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  )

k <-proj%>% 
  filter(
    position=="K"
  )%>%
  arrange(desc(pts_tot))%>%
  mutate(
    pos_rank = rank(-pts_tot,ties.method = "first")
  )

##Further Separate into Tiers based on 12 team league
qb1<-qb%>%
  slice(1:12)
qb2<-qb%>%
  slice(13:24)

wr1<-wr%>%
  slice(1:12)
wr2<-wr%>%
  slice(13:24)
wr3<-wr%>%
  slice(25:36)
wr4<-wr%>%
  slice(37:48)

rb1<-rb%>%
  slice(1:12)
rb2<-rb%>%
  slice(13:24)
rb3<-rb%>%
  slice(25:36)
rb4<-rb%>%
  slice(37:48)

te1<-te%>%
  slice(1:12)
te2<-te%>%
  slice(13:24)

def1<-def%>%
  slice(1:12)
k1<-k%>%
  slice(1:12)

###Calculate tier means ####

qb1avg<- mean(qb1$pts_tot)
qb2avg<- mean(qb2$pts_tot)

rb1avg<- mean(rb1$pts_tot)
rb2avg<- mean(rb2$pts_tot)
rb3avg<- mean(rb3$pts_tot)
rb4avg<- mean(rb4$pts_tot)

wr1avg<- mean(wr1$pts_tot)
wr2avg<- mean(wr2$pts_tot)
wr3avg<- mean(wr3$pts_tot)
wr4avg<- mean(wr4$pts_tot)

te1avg<- mean(te1$pts_tot)
te2avg<- mean(te2$pts_tot)

def1avg<- mean(def1$pts_tot)
k1avg<- mean(k1$pts_tot)


###Calculate tier medians
qb1med<- median(qb1$pts_tot)
qb2med<- median(qb2$pts_tot)

rb1med<- median(rb1$pts_tot)
rb2med<- median(rb2$pts_tot)
rb3med<- median(rb3$pts_tot)
rb4med<- median(rb4$pts_tot)

wr1med<- median(wr1$pts_tot)
wr2med<- median(wr2$pts_tot)
wr3med<- median(wr3$pts_tot)
wr4med<- median(wr4$pts_tot)

te1med<- median(te1$pts_tot)
te2med<- median(te2$pts_tot)

def1med<- median(def1$pts_tot)
k1med<- median(k1$pts_tot)


###Max
qb1max<- max(qb1$pts_tot)
qb2max<- max(qb2$pts_tot)

rb1max<- max(rb1$pts_tot)
rb2max<- max(rb2$pts_tot)
rb3max<- max(rb3$pts_tot)
rb4max<- max(rb4$pts_tot)

wr1max<- max(wr1$pts_tot)
wr2max<- max(wr2$pts_tot)
wr3max<- max(wr3$pts_tot)
wr4max<- max(wr4$pts_tot)

te1max<- max(te1$pts_tot)
te2max<- max(te2$pts_tot)

def1max<- max(def1$pts_tot)
k1max<- max(k1$pts_tot)

###Min
qb1min<- min(qb1$pts_tot)
qb2min<- min(qb2$pts_tot)

rb1min<- min(rb1$pts_tot)
rb2min<- min(rb2$pts_tot)
rb3min<- min(rb3$pts_tot)
rb4min<- min(rb4$pts_tot)

wr1min<- min(wr1$pts_tot)
wr2min<- min(wr2$pts_tot)
wr3min<- min(wr3$pts_tot)
wr4min<- min(wr4$pts_tot)

te1min<- min(te1$pts_tot)
te2min<- min(te2$pts_tot)

def1min<- min(def1$pts_tot)
k1min<- min(k1$pts_tot)

##range
qb1range<- qb1max-qb1min
qb2range<- qb2max-qb2min

rb1range<- rb1max-rb1min
rb2range<- rb2max-rb2min
rb3range<- rb3max-rb3min
rb4range<- rb4max-rb4min

wr1range<- wr1max-wr1min
wr2range<- wr2max-wr2min
wr3range<- wr3max-wr3min
wr4range<- wr4max-wr4min

te1range<- te1max-te1min
te2range<- te2max-te2min

def1range<- def1max-def1min
k1range<- k1max-k1min


###add calculated columns on tiered results



qb1<-qb1%>%
  mutate(
    pos = "QB1",
    pos_avg_var = pts_tot-qb1avg,
    pos_med_var = pts_tot-qb1med,
    val_adj_avg=pos_avg_var/qb1avg,
    val_adj_med=pos_med_var/qb1med,
    val1=(1+val_adj_avg)*10,
    val2=(1+val_adj_med)*10,
    val_adj_range= pos_med_var / qb1range,
    val3=(1+val_adj_range)*10,
    avg_val=(val1+val2+val3)/3
  )
qb2<-qb2%>%
  mutate(
    pos = "QB2",
    pos_avg_var = pts_tot-qb2avg,
    pos_med_var = pts_tot-qb2med,
    val_adj_avg=pos_avg_var/qb2avg,
    val_adj_med=pos_med_var/qb2med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / qb2range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

wr1<-wr1%>%
  mutate(
    pos = "wr1",
    pos_avg_var = pts_tot-wr1avg,
    pos_med_var = pts_tot-wr1med,
    val_adj_avg=pos_avg_var/wr1avg,
    val_adj_med=pos_med_var/wr1med,
    val1=(1+val_adj_avg)*55,
    val2=(1+val_adj_med)*55,
    val_adj_range= pos_med_var / wr1range,
    val3=(1+val_adj_range)*55,
    avg_val=(val1+val2+val3)/3
  )
wr2<-wr2%>%
  mutate(
    pos = "wr2",
    pos_avg_var = pts_tot-wr2avg,
    pos_med_var = pts_tot-wr2med,
    val_adj_avg=pos_avg_var/wr2avg,
    val_adj_med=pos_med_var/wr2med,
    val1=(1+val_adj_avg)*24,
    val2=(1+val_adj_med)*24,
    val_adj_range= pos_med_var / wr2range,
    val3=(1+val_adj_range)*24,
    avg_val=(val1+val2+val3)/3
  )

wr3<-wr3%>%
  mutate(
    pos = "wr3",
    pos_avg_var = pts_tot-wr3avg,
    pos_med_var = pts_tot-wr3med,
    val_adj_avg=pos_avg_var/wr3avg,
    val_adj_med=pos_med_var/wr3med,
    val1=(1+val_adj_avg)*10,
    val2=(1+val_adj_med)*10,
    val_adj_range= pos_med_var / wr3range,
    val3=(1+val_adj_range)*10,
    avg_val=(val1+val2+val3)/3
  )
wr4<-wr4%>%
  mutate(
    pos = "wr4",
    pos_avg_var = pts_tot-wr4avg,
    pos_med_var = pts_tot-wr4med,
    val_adj_avg=pos_avg_var/wr4avg,
    val_adj_med=pos_med_var/wr4med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / wr4range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )
rb1<-rb1%>%
  mutate(
    pos = "rb1",
    pos_avg_var = pts_tot-rb1avg,
    pos_med_var = pts_tot-rb1med,
    val_adj_avg=pos_avg_var/rb1avg,
    val_adj_med=pos_med_var/rb1med,
    val1=(1+val_adj_avg)*56,
    val2=(1+val_adj_med)*56,
    val_adj_range= pos_med_var / rb1range,
    val3=(1+val_adj_range)*56,
    avg_val=(val1+val2+val3)/3
  )
rb2<-rb2%>%
  mutate(
    pos = "rb2",
    pos_avg_var = pts_tot-rb2avg,
    pos_med_var = pts_tot-rb2med,
    val_adj_avg=pos_avg_var/rb2avg,
    val_adj_med=pos_med_var/rb2med,
    val1=(1+val_adj_avg)*25,
    val2=(1+val_adj_med)*25,
    val_adj_range= pos_med_var / rb2range,
    val3=(1+val_adj_range)*25,
    avg_val=(val1+val2+val3)/3
  )

rb3<-rb3%>%
  mutate(
    pos = "rb3",
    pos_avg_var = pts_tot-rb3avg,
    pos_med_var = pts_tot-rb3med,
    val_adj_avg=pos_avg_var/rb3avg,
    val_adj_med=pos_med_var/rb3med,
    val1=(1+val_adj_avg)*8,
    val2=(1+val_adj_med)*8,
    val_adj_range= pos_med_var / rb3range,
    val3=(1+val_adj_range)*8,
    avg_val=(val1+val2+val3)/3
  )
rb4<-rb4%>%
  mutate(
    pos = "rb4",
    pos_avg_var = pts_tot-rb4avg,
    pos_med_var = pts_tot-rb4med,
    val_adj_avg=pos_avg_var/rb4avg,
    val_adj_med=pos_med_var/rb4med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / rb4range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

te1<-te1%>%
  mutate(
    pos = "te1",
    pos_avg_var = pts_tot-te1avg,
    pos_med_var = pts_tot-te1med,
    val_adj_avg=pos_avg_var/te1avg,
    val_adj_med=pos_med_var/te1med,
    val1=(1+val_adj_avg)*5,
    val2=(1+val_adj_med)*5,
    val_adj_range= pos_med_var / te1range,
    val3=(1+val_adj_range)*5,
    avg_val=(val1+val2+val3)/3
  )
te2<-te2%>%
  mutate(
    pos = "te2",
    pos_avg_var = pts_tot-te2avg,
    pos_med_var = pts_tot-te2med,
    val_adj_avg=pos_avg_var/te2avg,
    val_adj_med=pos_med_var/te2med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / te2range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )

def1<-def1%>%
  mutate(
    pos = "def1",
    pos_avg_var = pts_tot-def1avg,
    pos_med_var = pts_tot-def1med,
    val_adj_avg=pos_avg_var/def1avg,
    val_adj_med=pos_med_var/def1med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / def1range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )
k1<-k1%>%
  mutate(
    pos = "k1",
    pos_avg_var = pts_tot-k1avg,
    pos_med_var = pts_tot-k1med,
    val_adj_avg=pos_avg_var/k1avg,
    val_adj_med=pos_med_var/k1med,
    val1=(1+val_adj_avg)*1,
    val2=(1+val_adj_med)*1,
    val_adj_range= pos_med_var / k1range,
    val3=(1+val_adj_range)*1,
    avg_val=(val1+val2+val3)/3
  )


##Join Back into Position Groups
qball<-union(qb1,qb2)

wrall1<-union(wr1,wr2)
wrall2<-union(wrall1,wr3)
wrall<-union(wrall2,wr4)
rm(wrall1,wrall2)

rball1<-union(rb1,rb2)
rball2<-union(rball1,rb3)
rball<-union(rball2,rb4)
rm(rball1,rball2)

teall<-union(te1,te2)

##Join into Player Dataset

player_all<-union(qball,wrall)%>%
  union(rball)%>%
  union(teall)%>%
  union(def1)%>%
  union(k1)%>%
  arrange(rank)

write.csv(player_all,"data/toptotcalculatedvalues.csv", row.names=FALSE)

##Do some background Statistics, calculate average points per game for median team and percentage of points for each starting position group

totptsmed<-qb1med+wr1med+wr2med+rb1med+rb2med+wr3med+te1med+def1med+k1med
qb1med/totptsmed
rb1med/totptsmed
rb2med/totptsmed  
wr1med/totptsmed
wr2med/totptsmed 
wr3med/totptsmed
te1med/totptsmed 
def1med/totptsmed
k1med/totptsmed 

qb1med-qb2med
wr1med-wr2med
wr2med-wr3med
wr3med-wr4med
rb1med-rb2med
rb2med-rb3med
rb3med-rb4med

te1med-te2med

rm(list=ls())
