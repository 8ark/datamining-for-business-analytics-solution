# 2.11

Toyota <- read.csv("ToyotaCorolla.csv", header = T)
str(Toyota)

# (a) 시각화 기능을 이용해 상관성이 큰 변수쌍을 찾으시오.

# 우선 View()를 통해 데이터셋을 봤다.
View(Toyota)
dim(Toyota)

plot(Toyota[,2:21])

Toyota$Model <- as.numeric(Toyota$Model)

##### (b) -i

set.seed(1234)

# 우리가 바꿔야 하는 범주형 변수 Metalic, Fuel_Type

# Metallic은 이미 전환이 끝남

# Fuel_Type 를 바꿔보자
Ch_Fuel_Type <- model.matrix(~0 + Toyota$Fuel_Type, data = Toyota)
Ch_Fuel_Type <- as.data.frame(Ch_Fuel_Type)
str(Ch_Fuel_Type)
t(t(names(Ch_Fuel_Type)))
head(Ch_Fuel_Type)

# Model 변수와 Fuel_Type 변수를 지우고 새로 만든 변수들을 집어넣자

Toyota_NEW <-cbind(Toyota[,-c(2,8)],Ch_Fuel_Type)
str(Toyota_NEW)


#### + 덧붙임. 다르게 만든 거

nnrow<-dim(Toyota)[1]
nnrow
nncol<-dim(Toyota)[2]
nncol

levels(Toyota$Fuel_Type)
# Fuel_Type은 3가지의 변수가 있었다. Diesel, Petrol, CNG
# 이들을 각각의 3변수를 만들어, 그곳에 맞는 값들만 1이 형성되도록
# 만들 거다.


# 우선 더미 변수를 만들어 공간을 확보한다.

dummy_D<-rep(0,nnrow)
dummy_P<-rep(0,nnrow)
dummy_C<-rep(0,nnrow)


D_index <- which(Toyota$Fuel_Type=="Diesel")
P_index <- which(Toyota$Fuel_Type=="Petrol")
C_index <- which(Toyota$Fuel_Type=="CNG")

# 더미 파일은 지금 빈 상태
# index는 몇 번째 해당 단어가 있는지 알려주는 것이므로
# 해당 index 마다 1을 넣는다고 하면

dummy_D[D_index] <-1
dummy_P[P_index] <-1
dummy_C[C_index] <-1

# 얘네들만 다시 만든다.

Fuel <- data.frame(dummy_C,dummy_D, dummy_P)
names(Fuel) <- c("Fuel_CNG","Fuel_Diesel","Fuel_Petrol")
Fuel
str(Fuel)

# Fuel 이라는 데이터 프레임 안에 CNG, Diesel, Petrol 컬럼이 있다.
# 이 데이터를 원 데이터인 Toyota와 섞으면...

# 이왕 없애는 김에 id, Fuel_Type도 없앨까?
# id = 1, Fuel_Type = 8 번째니까

Toyota_New <- cbind(Toyota[,-c(1,8)], Fuel)
str(Toyota_New)

View(Toyota_New)

Toyota_New$Model<-factor(Toyota_New$Model)
class(Toyota_New$Model)
levels(Toyota_New$Model)

#### (b) -ii

### Hmmmm...데이터를 나눠야 한다.

####### 교재용 #######

# 학습데이터(50%)
trn<- sample(rownames(Toyota_New), dim(Toyota_New)[1]*0.5)
trn.data <-Toyota_New[trn,]
# 검증데이터(30%)
val<- sample(setdiff(rownames(Toyota_New), trn),dim(Toyota_New)[1]*0.3)
val.data <-Toyota_New[val,] 
# 평가데이터(20%)
test <-setdiff(rownames(Toyota_New),union(trn,val))
test.data<- Toyota_New[test,]


###이건 따로 찾아본 거###


# 학습데이터(50%)
trn_tyt<-sample(1:nnrow, round(0.5*nnrow))
Toyota_Trn<- Toyota_New[trn_tyt,]
# 검증데이터(30%)
val_tyt<- sample(setdiff(1:nnrow, trn_tyt), round(0.3*nnrow))
Toyota_Val<- Toyota_New[val_tyt,]
#평가데이터
test_tyt<- setdiff(1:nnrow, union(trn_tyt,val_tyt))
Toyota_Test<-Toyota_New[test_tyt,]

