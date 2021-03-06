Laptop <- read.csv("LaptopSales.csv", header = T)

install.packages("devtools")
install.packages("rlang")
library(devtools)

install_github('dkahle/ggmap')

library('ggmap')

install.packages('rvest')

library('rvest')

register_google(key='AIzaSyDl300ehnvcIQpZeKSUkztLFDPMkWVSx54')

windows()

dankook_map <- get_map("dankook university",zoom=17,maptype="roadmap")

ggmap(dankook_map)




## 3.4 (a)

library(ggplot2)
library(dplyr)

Laptop.retail <- Laptop[,c(2,5)]
str(laptop.retail)
sum(is.na(Laptop.retail))
Laptop.retail<-na.omit(Laptop.retail)

Lap_Retail <- Laptop.retail %>%
  group_by(Configuration) %>%
  summarise(Price_mean = mean(Retail.Price, na.rm = T),
            Price_sd = sd(Retail.Price, na.rm = T))

Lap_Retail

Lap_Retail <- na.omit(Lap_Retail)

library(showtext)
font_add_google("Nanum Gothic", "nanumgothic")
showtext_auto()


# i. 실제로 노트북은 얼마에 판매되었는가?

ggplot(data = Lap_Retail, aes(x = Lap_Retail$Configuration, y = Lap_Retail$Price_mean))+
  geom_point(aes(fill=Lap_Retail$Price_sd),size = 1.7,shape=21,alpha =0.5)+
  ggtitle("Price~Configuration")+
  labs(x = "Configuration",y = "Price_Mean",fill = "Price_sd")+
  theme(plot.title = element_text(family = "nanumgothic",face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"),legend.title = element_text(face="bold",size = 7.5, colour = "darkblue"), legend.box.background = element_rect(fill = "skyblue"),legend.box.margin = margin(1,1,1,1))


dim(Lap_Retail)

# 위 그림을 보면 configuration ~ Retail.Price 가 Configuration의 증가에 맞게 Retail.Price가 증가하는 비례관계임을 알 수 있다. 이 형태를 좀 더 명확하게 보기 위해 회귀선을 부여했다.

# 회귀선 추가
ggplot(data = Lap_Retail, aes(x = Lap_Retail$Configuration, y = Lap_Retail$Price_mean))+
  geom_point(aes(fill=Lap_Retail$Price_sd),size = 1.7,shape=21,alpha =0.5)+
  geom_smooth()+
  ggtitle("Price~Configuration")+
  labs(x = "Configuration",y = "Price_Mean",fill = "Price_sd")+
  theme(plot.title = element_text(family = "nanumgothic",face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"),legend.title = element_text(face="bold",size = 7.5, colour = "darkblue"), legend.box.background = element_rect(fill = "skyblue"),legend.box.margin = margin(1,1,1,1))

# 그렇다면 노트북 판매가를 어떻게 말해야할까. 그래프를 보면 Configuration에 따라 명확한 집단이 6개 보인다. 해당 구간만큼 집단을 쪼개 그 집단의 평균 판매가를 답하는 것이 가장 맞다고 생각한다.

ggplot(data = Lap_Retail, aes(x = Lap_Retail$Configuration, y = Lap_Retail$Price_mean))+
  geom_point(aes(fill=Lap_Retail$Price_sd),size = 1.7,shape=21,alpha =0.5)+
  geom_smooth()+
  ggtitle("Price~Configuration")+
  labs(x = "Configuration",y = "Price_Mean",fill = "Price_sd")+
  scale_x_continuous(breaks=seq(0,1000,50))+
  theme(plot.title = element_text(family = "nanumgothic",face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"),legend.title = element_text(face="bold",size = 7.5, colour = "darkblue"), legend.box.background = element_rect(fill = "skyblue"),legend.box.margin = margin(1,1,1,1))

# scale_x_continuous 함수를 사용해 집단의 범위를 세밀하게 파악한다. 이후 집단을 X축 기준으로 나누어 6개의 집단으로 묶어준다.

Case1 <-Lap_Retail %>%
  filter(Configuration <= 150)
Case2 <-Lap_Retail %>%
  filter(Configuration > 150 & Configuration <= 300)
Case3 <-Lap_Retail %>%
  filter(Configuration > 300 & Configuration <= 430)
Case4 <-Lap_Retail %>%
  filter(Configuration > 430 & Configuration <= 575)
Case5 <-Lap_Retail %>%
  filter(Configuration > 575 & Configuration <= 725)
Case6 <-Lap_Retail %>%
  filter(Configuration > 725 & Configuration <= 875)

Case <- data.frame(Case1,Case2,Case3,Case4,Case5,Case6)

Result <- data.frame(M_Case1 = mean(Case1$Price_mean,na.rm = T),
          M_Case2 = mean(Case2$Price_mean,na.rm = T),
          M_Case3 = mean(Case3$Price_mean,na.rm = T),
          M_Case4 = mean(Case4$Price_mean,na.rm = T),
          M_Case5 = mean(Case5$Price_mean,na.rm = T),
          M_Case6 = mean(Case6$Price_mean,na.rm = T))

# 6집단으로나눴을 경우 집단 간 평균 가격은 위와 같다.
Result


# ii. 시간에 따라서 판매가격이 변화하였는가?

# 1년을 기준으로 시간 데이터를 다룰 때,
# 월, 요일, 분기를 기준으로 데이터를 구분해야 한다고 파악

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

Lap_time <- Laptop[,c(1,5)]
Lap_time <- na.omit(Lap_time)


# 시간 렬의 데이터를 확인하니 수정해야겠다는 생각이 든다.
head(Lap_time)

Lap_time$Date <- gsub(" ",",",Lap_time$Date)
head(Lap_time)

Lap_time$YMD <-str_sub(Lap_time$Date,1,9)

Lap_time <- Lap_time[,-1]
head(Lap_time)

# lubricate 패키지를 이용해 시간 데이터를 다시 한 번 정리한다.

Lap_time$YMD <- dmy(Lap_time$YMD)
head(Lap_time)

Lap_time$MONTH <- month(Lap_time$YMD, label = T)
Lap_time$WDAY <- wday(Lap_time$YMD, label = T)
Lap_time$QUARTER <- quarter(Lap_time$YMD)

Lap_time <- na.omit(Lap_time)
## 월별로 데이터를 보려면

# Sales ~ Month + Wday
ggplot(data = Lap_time, aes(x = Lap_time$MONTH, y = Lap_time$Retail.Price, fill = Lap_time$WDAY))+
  geom_col()+
  labs(x = "월별", y = "판매량", fill = "요일")+
  ggtitle("월별 판매량")+
  theme(plot.title = element_text(face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"),legend.title = element_text(face="bold",size = 7.5, colour = "darkblue"))

## 분기별로 보려면
ggplot(data = Lap_time, aes(x = Lap_time$QUARTER, y = Lap_time$Retail.Price, fill = Lap_time$WDAY))+
  geom_col()+
  labs(x = "분기별", y = "판매량", fill = "요일")+
  ggtitle("분기별 판매량")+
  theme(plot.title = element_text(face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"),legend.title = element_text(face="bold",size = 7.5, colour = "darkblue"))

## 마지막 요일만
ggplot(data = Lap_time, aes(x = Lap_time$WDAY, y = Lap_time$Retail.Price))+
  geom_col()+
  labs(x = "요일 별", y = "판매량")+
  ggtitle("요일 별 판매량")+
  theme(plot.title = element_text(face = "bold",hjust=0.5,size = 13, colour = "darkblue"),axis.title = element_text(face = "bold", size = 10, colour = "darkblue"))

# 요일은 큰 차이가 없다.


## iii. 판매 가격은 각 매장 별로 일관성이 있는가?

Lap_store <- Laptop[,c(4:5)]
Lap_store <- na.omit(Lap_store)
head(Lap_store)
dim(Lap_store)


ggplot(data = Lap_store, aes(x = reorder(Store.Postcode, Retail.Price), y = Retail.Price))+
  geom_col()


# iv 판매 가격은 컴퓨터 사양에 따라 어떻게 다른가?

Lap_part <- Laptop[,5:12]
Lap_part <- na.omit(Lap_part)
head(Lap_part)
class(Lap_part$Battery.Life..Hours.)


#1) Screen Size
Lap_part$Screen.Size..Inches. <-factor(Lap_part$Screen.Size..Inches.)
#2) Battery Life
Lap_part$Battery.Life..Hours. <- factor(Lap_part$Battery.Life..Hours.)
#3) RAM
Lap_part$RAM..GB. <- factor(Lap_part$RAM..GB.)
#4) Processor
Lap_part$Processor.Speeds..GHz. <- factor(Lap_part$Processor.Speeds..GHz.)
#5) Wireless
Lap_part$Integrated.Wireless. <- factor(Lap_part$Integrated.Wireless.)
#6) HD Size
Lap_part$HD.Size..GB. <- factor(Lap_part$HD.Size..GB.)
#7) Bundled
Lap_part$Bundled.Applications. <- factor(Lap_part$Bundled.Applications.)


par(mfcol = c(1,4))

boxplot(Lap_part$Retail.Price ~ Lap_part$Screen.Size..Inches.,xlab = "Screen Size", ylab = "Retail Price")
boxplot(Lap_part$Retail.Price ~ Lap_part$Battery.Life..Hours., xlab = "Battery Life")
boxplot(Lap_part$Retail.Price ~ Lap_part$RAM..GB.,xlab = "RAM")
boxplot(Lap_part$Retail.Price ~ Lap_part$Processor.Speeds..GHz., xlab = "Processor")
boxplot(Lap_part$Retail.Price ~ Lap_part$Integrated.Wireless., xlab = "Integrated Wireless",ylab = "Retail Price")
boxplot(Lap_part$Retail.Price ~ Lap_part$HD.Size..GB., xlab = "HD Size")
boxplot(Lap_part$Retail.Price ~ Lap_part$Bundled.Applications., xlab = "Bundled Application")

## 3.4 (b)

Lap_map <- Laptop[,c(3:5,13:16)]
Lap_map <- na.omit(Laptop)
head(Lap_map)
str(Lap_map)
# i.

library(ggmap)

register_google(key = "AIzaSyDTMuJi6LLbHLZ0Ks4ZgjCwAOIcB5wc9pc")

position <- geocode(as.character(Laptop$Store.Postcode))

Map <-get_googlemap(location = "United Kingdom",zoom = 5, maptype = "terrain")

Lap_map$customer.X <- substr(Lap_map$customer.X,1,3)
Lap_map$customer.Y <- substr(Lap_map$customer.Y,1,3)
Lap_map$store.X <- substr(Lap_map$store.X,1,3)
Lap_map$store.Y <- substr(Lap_map$store.Y,1,3)

map <- get_googlemap(center=c(lon=mean(Lap_map$store.X),lat=mean(Lap_map$store.Y)),
              zoom=10,maptype='roadmap')


store_map <- ggmap(Map)+
  

where <- geocode(as.character(Laptop$Store.Postcode))


#c.i.
Lap_store_whole <- Laptop[,c(4,5)]
Lap_store_whole <- na.omit(Lap_store_whole)

Lap_store_whole$Store.Postcode <- as.character(Lap_store_whole$Store.Postcode)
install.packages("treemap")


# d. ii
View(Laptop)
heat_Laptop <- Laptop[,c(4,6:12)]
str(heat_Laptop)
window()
heatmap(1*is.na(dataFrame), Rowv = NA, Colv = NA)




library(reshape2)
Heat <- Laptop[,c(2,4)]
Heat <- Heat %>%
  group_by(Store.Postcode)
unique(Heat[Heat$Store.Postcode == 'SW1P 3AU',])
Heat <- melt(Heat,id='Store.Postcode')
Heat <- Heat[order(Heat$Store.Postcode),]
; library(ggplot2)
ggplot(Heat,aes(x=Store.Postcode,value,fill=value)) + geom_tile() + ylab('Configuration')

