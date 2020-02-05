## DATA MINING

## HW_3장
Laptop <- read.table("LaptopSales.txt",header = T, sep = "\t",fileEncoding="UTF-16LE")

View(Laptop)



## HW_4장

# 4.1
Cereal <- read.csv("Cereals.csv", header = T)
str(Cereal)

# a) 어느 변수들이 양적/수치적 변수인가? 순서형 변수는 어떤 것인가?
#  명목형 변수는 어떤 것인가?

# 양적/수치적 변수
# calories, protein, fat, sodium, fiber, carbo, sugars, potass, weight, cups

# 순서형 변수
# rating, vitamins(0,25,100)

# 명목형 변수
mfr, type, shelf

# b) 각각의 양적변수에 대해서 평균, 중앙값, 최솟값, 최댓값, 그리고 표준편차를
#    계산하시오.

# 하나하나 보고 싶으면...[예시처럼]
C_mean <- sapply(Cereal[,4:11], mean, na.rm=T)
C_median <- sapply(Cereal[,4:11], median, na.rm=T)
C_min <- sapply(Cereal[,4:11], min, na.rm=T)
C_max <- sapply(Cereal[,4:11], max, na.rm=T))
C_sd <- sapply(Cereal[,4:11], sd, na.rm=T)

# 한 번에 보고 싶으면...
summary(Cereal[,4:11])

# c
par(mfcol =c(3,2))

hist(Cereal$calories)
hist(Cereal$protein)
hist(Cereal$fat)
hist(Cereal$sodium)
hist(Cereal$fiber)
hist(Cereal$carbo)
hist(Cereal$sugars)
hist(Cereal$potass)
hist(Cereal$vitamins)

## i 어떤 변수의 변동이 가장 큰가?
# protein, fat

## ii 어떤 변수가 치우쳐 있는가?
# fiber, potass, 

## iii 극단값으로 보이는 값이 있는가?
# vitamins, 


### d)  R을 이용하여 저온용, 고온용 시리얼에 함유된 칼로리를
###     비교하기 위해 박스플롯을 나란히 그리시오. 이를 통해 무엇을 알 수 있는가?

# 우선, 저온용, 고온용이 factor로 
class(Cereal$type)

library(ggplot2)
ggplot(data = Cereal, aes(x = as.factor(Cereal$type),y = Cereal$calories))+
  geom_boxplot()


# e)
ggplot(data = Cereal, aes(x = as.factor(Cereal$shelf),y = Cereal$rating))+
  geom_boxplot()+
  xlab("Shelf")+
  ylab("Cereal_Rating")

# f)
sum(is.na(Cereal[,4:11])) ## NA 몇 개 있는지 확인
cor(Cereal[,4:11], use="complete.obs")

plot(Cereal[,4:11])
 
## i) fiber ~ potass
## ii) 상관관계가 높은 변수만 추출해서 사용가능하다.
## iii) 