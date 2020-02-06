fl<-read.csv('FlightDelays.csv')
library(dplyr)
tbl_df(fl)
unique(fl$CARRIER)
unique(fl$ORIGIN)
unique(fl$DEST)
unique(fl$DAY_WEEK)

##CARRIER 가변수 처리
fl<-fl%>%
  mutate(CAR.OH=ifelse(fl$CARRIER=='OH',1,0),
         CAR.DH=ifelse(fl$CARRIER=='DH',1,0),
         CAR.DL=ifelse(fl$CARRIER=='DL',1,0),
         CAR.MQ=ifelse(fl$CARRIER=='MQ',1,0),
         CAR.UA=ifelse(fl$CARRIER=='UA',1,0),
         CAR.US=ifelse(fl$CARRIER=='US',1,0),
         CAR.RU=ifelse(fl$CARRIER=='RU',1,0))

fl$CAR.OH<-factor(fl$CAR.OH)
fl$CAR.DH<-factor(fl$CAR.DH)
fl$CAR.DL<-factor(fl$CAR.DL)
fl$CAR.MQ<-factor(fl$CAR.MQ)
fl$CAR.UA<-factor(fl$CAR.UA)
fl$CAR.US<-factor(fl$CAR.US)
fl$CAR.RU<-factor(fl$CAR.RU)
fl
tbl_df(fl)


##ORIGIN 가변수 처리
fl<-fl%>%
  mutate(ORI.BWI=ifelse(fl$ORIGIN=='BWI',1,0),
         ORI.DCA=ifelse(fl$ORIGIN=='DCA',1,0))
fl
fl$ORI.BWI<-factor(fl$ORI.BWI)
fl$ORI.DCA<-factor(fl$ORI.DCA)



##DEST 가변수 처리
fl<-fl%>%
  mutate(DEST.JFK=ifelse(fl$DEST=='JFK',1,0),
         DEST.LGA=ifelse(fl$DEST=='LGA',1,0))
fl$DEST.JFK<-factor(fl$DEST.JFK)
fl$DEST.LGA<-factor(fl$DEST.LGA)


##요일 가변수 처리
unique(fl$DAY_WEEK)

fl<-fl%>%
  mutate(dw.mon=ifelse(fl$DAY_WEEK==1,1,0),
         dw.tue=ifelse(fl$DAY_WEEK==2,1,0),
         dw.wed=ifelse(fl$DAY_WEEK==3,1,0),
         dw.thur=ifelse(fl$DAY_WEEK==4,1,0),
         dw.fri=ifelse(fl$DAY_WEEK==5,1,0),
         dw.sat=ifelse(fl$DAY_WEEK==6,1,0))

weekdays(as.Date('2004-01-06'))
fl[fl$DAY_WEEK==2,6]

fl$dw.mon<-factor(fl$dw.mon)
fl$dw.tue<-factor(fl$dw.tue)
fl$dw.wed<-factor(fl$dw.wed)
fl$dw.thur<-factor(fl$dw.thur)
fl$dw.fri<-factor(fl$dw.fri)
fl$dw.sat<-factor(fl$dw.sat)



## 출발시간 구간화

tbl_df(fl)
summary(fl$DEP_TIME)
sort(fl$DEP_TIME)
cut(fl$DEP_TIME,breaks=seq(min(fl$DEP_TIME),max(fl$DEP_TIME),length.out = 9),include.lowest = T)
fl<-fl%>%
  mutate(DepT1=ifelse(fl$DEP_TIME>=10&fl$DEP_TIME<=300,1,0),
         DepT2=ifelse(fl$DEP_TIME>300&fl$DEP_TIME<=590,1,0),
         DepT3=ifelse(fl$DEP_TIME>590&fl$DEP_TIME<=880,1,0),
         DepT4=ifelse(fl$DEP_TIME>880&fl$DEP_TIME<=1170,1,0),
         DepT5=ifelse(fl$DEP_TIME>1170&fl$DEP_TIME<=1460,1,0),
         DepT6=ifelse(fl$DEP_TIME>1460&fl$DEP_TIME<=1750,1,0),
         DepT7=ifelse(fl$DEP_TIME>1750&fl$DEP_TIME<=2040,1,0))
fl

##예정 출발 시간
summary(fl$CRS_DEP_TIME)
fl<-fl%>%
  mutate(schDT1=ifelse(fl$CRS_DEP_TIME>=600&fl$CRS_DEP_TIME<=800,1,0),
         schDT2=ifelse(fl$CRS_DEP_TIME>800&fl$CRS_DEP_TIME<=1000,1,0),
         schDT3=ifelse(fl$CRS_DEP_TIME>1000&fl$CRS_DEP_TIME<=1200,1,0),
         schDT4=ifelse(fl$CRS_DEP_TIME>1200&fl$CRS_DEP_TIME<=1400,1,0),
         schDT5=ifelse(fl$CRS_DEP_TIME>1400&fl$CRS_DEP_TIME<=1600,1,0),
         schDT6=ifelse(fl$CRS_DEP_TIME>1600&fl$CRS_DEP_TIME<=1800,1,0),
         schDT7=ifelse(fl$CRS_DEP_TIME>1800&fl$CRS_DEP_TIME<=2000,1,0))

fl$schDT1<-factor(fl$schDT1)
fl$schDT2<-factor(fl$schDT2)
fl$schDT3<-factor(fl$schDT3)
fl$schDT4<-factor(fl$schDT4)
fl$schDT5<-factor(fl$schDT5)
fl$schDT6<-factor(fl$schDT6)
fl$schDT7<-factor(fl$schDT7)


##변수 선택

fl.la<-fl%>%
  select(DISTANCE,Weather,Flight.Status,CAR.OH,
         CAR.DH,CAR.DL,CAR.MQ,CAR.UA,CAR.US,CAR.RU,ORI.BWI,ORI.DCA,DEST.JFK,DEST.LGA,dw.mon,dw.tue,
         dw.wed,dw.thur,dw.fri,dw.sat,schDT1,schDT2,schDT3,schDT4,schDT5,schDT6,schDT7)


##데이터 분할
set.seed(1)
trn<-sample(1:nrow(fl),floor(0.6*nrow(fl)))

trn.fl<-fl.la[trn,]
val.fl<-fl.la[-trn,]
tbl_df(trn.fl)
tbl_df(fl)

#모델 적합
library(tree)
a<-tree(Flight.Status~.,data=trn.fl,minsize=2,mindev=0,mincut=0) #완전 성장한 나무
windows()
plot(a);text(a)
plot(prune.tree(a))




aa<-prune.tree(a,best=7) ##나무 가지치기 분할수=6번 리프노드 수 =7
plot(aa); text(aa)


##검증데이터의 오분류율을 이용한 가지치기
val.err<-0:5; trn.err<-0:5
for(i in 2:7){
  m<-prune.tree(aa,best=i)
  p.tr<-predict(m,trn.fl,type='class')
  p.va<-predict(m,val.fl,type='class')
  trn.err[i-1]<-mean(p.tr!=trn.fl$Flight.Status)
  val.err[i-1]<-mean(p.va!=val.fl$Flight.Status)
}
df<-data.frame(2:7,trn.err,val.err) ##리프노드의 수가 2개일 때 검증데이터의 오분류율이 가장 낮다
sqrt((0.1770715*(1-0.1770715))/nrow(val.fl))+0.1770715 ## 가장 작은 검증 데이터에서의 오분류율 + 표준오차 = 0.1899323 보다 작으면서 리프노드가 작은 나무 선택

plot(cv.tree(aa,FUN=prune.misclass))
prune.tree(aa,best=2)

windows()
cv.tree(aa)
plot(cv.tree(aa,FUN=prune.misclass))
plot(cv.tree(a,FUN=prune.misclass))

##나무 깊이가 6인 모델에서 중복 정보.
vc<-fl%>%
  group_by(ORIGIN,DEST,DISTANCE)%>%
  select(ORIGIN,DEST,DISTANCE)
unique(vc)  



##c 

tbl_df(fl)

flfl<-fl%>%
  select(FL_NUM,Weather,Flight.Status,CAR.OH,CAR.DH,CAR.DL,CAR.MQ,CAR.UA,CAR.US,CAR.RU,DEST.JFK,DEST.LGA,ORI.BWI,ORI.DCA)

trn.flfl<-flfl[trn,]
val.flfl<-flfl[-trn,]
windows()
md.c<-tree(Flight.Status~.,data=trn.flfl,minsize=2,mindev=0)
plot(md.c); text(md.c)

plot(prune.tree(md.c))
plot(cv.tree(md.c,FUN=prune.misclass))


##검증데이터의 오분류율을 이용한 가지치기
size<-2:94
val.err<-rep(0,93); trn.err<-rep(0,93)
for(i in 2:94){
  mc<-prune.tree(md.c,best=i)
  p.tr<-predict(mc,trn.flfl,type='class')
  p.va<-predict(mc,val.flfl,type='class')
  trn.err[i-1]<-mean(p.tr!=trn.flfl$Flight.Status)
  val.err[i-1]<-mean(p.va!=val.flfl$Flight.Status)
}
df<-data.frame(size,trn.err,val.err) ##리프노드의 수가 2개일 때 검증데이터의 오분류율이 가장 낮다
sqrt((0.1748*(1-0.1748))/nrow(val.flfl))+0.1748 ## 가장 작은 검증 데이터에서의 오분류율 + 표준오차 = 0.1899323 보다 작으면서 리프노드가 작은 나무 선택
summary(df$val.err)
min(df$val.err)

plot(prune.tree(md.c,best=5)); text(prune.tree(md.c,best=5))