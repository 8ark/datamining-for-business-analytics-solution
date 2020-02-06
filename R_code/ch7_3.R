bos<-read.csv('BostonHousing.csv',header=T)
library(dplyr)
library(class)
library(caret)
tbl_df(bos)
bos$CAT..MEDV<-factor(bos$CAT..MEDV)
set.seed(1)
trn<-sample(1:nrow(bos),floor(0.6*nrow(bos)))
trn.bos<-bos[trn,]
val.bos<-bos[-trn,]
trn.bos.sc<-scale(trn.bos[,-14])
val.bos.sc<-scale(val.bos[,-14])
knn_1<-knn(train = trn.bos[,-14],test=val.bos[,-14],cl=trn.bos[,14],k=1)
confusionMatrix(knn_1,val.bos[,14])
table(knn_1,val.bos[,14])
for(i in 1:5){
  knn_i<-knn(trn.bos.sc,val.bos.sc,trn.bos[,14],k=i)
  print(paste(i,'-nn err = ',sum(table(knn_i,val.bos$CAT..MEDV)[c(2,3)])/sum(table(knn_i,val.bos$CAT..MEDV)),sep=''))
}

# 새로운 데이터를 넣고 스케일하기
new<-c(0.2,0,7,0,0.538,6,62,4.7,4,307,21,10)
trn.bos.b<-trn.bos[,-c(13,14)]
val.bos.b<-val.bos[,-c(13,14)]
val.bos.b<-rbind(val.bos.b,new)
tail(val.bos.b)

trn.bos.b.sc<-scale(trn.bos.b)
val.bos.b.sc<-scale(val.bos.b)
nrow(val.bos.b.sc)
val.bos.b.sc[204,]
trn.bos
install.packages('FNN')
library(FNN)
knn.reg(trn.bos.b.sc,val.bos.b.sc[204,],trn.bos[,13],k=5)

## 학습 데이터의 오류율

for(i in 1:20){
  knn_i<-knn(trn.bos.sc,trn.bos.sc,trn.bos[,14],k=i)
  print(paste(i,'-nn err = ',sum(table(knn_i,trn.bos$CAT..MEDV)[c(2,3)])/sum(table(knn_i,trn.bos$CAT..MEDV)),sep=''))
}


