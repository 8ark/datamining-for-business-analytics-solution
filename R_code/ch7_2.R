Bank <- read.csv("UniversalBank.csv")

View(Bank)

# 3개 이상 범주형 변수 -> 가변수 변호

Bank$Education <- as.factor(Bank$Education)

edu_type <- model.matrix( ~ 0 + Education, data = Bank)
str(edu_type)

edu_type <- as.data.frame(edu_type)
str(edu_type)

t(t(names(edu_type)))
edu_type

Bank <- cbind(Bank[, -c(1,5,8)],edu_type)
View(Bank)

# 더미변수를 한 개 제거하려 했으나
# 하지 않는다. 198p에 K-NN은 더미변수를 제거하지 않는다.고 써있다.
dim(Bank)
#Bank <- Bank[ ,-14]

# 종속변수 위치 조절
Bank <- Bank[,c(1:6,8:14,7)]

######################################################

# 전처리 끝

train_bank_index <- sample(row.names(Bank),0.6*dim(Bank)[1])
valid_bank_index <- setdiff(row.names(Bank), train_bank_index)

train_df <- Bank[train_bank_index, ]
valid_df <- Bank[valid_bank_index, ]
str(train_df)
new_df <- data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2,Mortgage=0,  Securities.Account=0, CD.Account=0,Online=1, CreditCard=1, Education1=0, Education2=1, Education3=0)

View(train_df)

### a ###

set.seed(1234)

library(caret)
dim(train_df)

train_norm_bk <- train_df
valid_norm_bk <- valid_df
bank_norm_bk <- Bank

norm.value <- preProcess(train_df[,1:13], method = c("center","scale"))

train_norm_bk[, 1:13] <- predict(norm.value, train_df[, 1:13])
valid_norm_bk[, 1:13] <- predict(norm.value, valid_df[, 1:13])
bank_norm_bk[, 1:13] <- predict(norm.value, Bank[, 1:13])
new_norm_df <- predict(norm.value,new_df)

str(train_norm_bk)
library(FNN)
# 2000으로 나중에 돌려보기
accuracy_df <- data.frame(k = seq(1,3000,1),accuracy = rep(0,3000))
for (i in 1:3000) {
  knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                  cl = train_norm_bk[, 14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, valid_norm_bk[ ,14]))$overall[1]
}

knn_pred_new <- knn(bank_norm_bk[,1:13], new_norm_df,
                    cl = bank_norm_bk[,14], k = 3)
row.names(train_df)[attr(nn,"nn.index")]


A <-table(knn_pred_new, valid_norm_bk[, 14])

confusionMatrix(A, positive = "1")




## k=1일때,새로운 학습데이터에 넣은 새로운 값의 결과는?
nn <- knn(train = train_norm_bk[,1:13],test = new_norm_df,
          cl = train_norm_bk[ , 14],k = 1)
row.names(train_df)[attr(nn,"nn.index")]
nn

nn <- knn(train = train_norm_bk[,1:13],test = new_norm_df,
          cl = train_norm_bk[ , 14],k = 5)
nn

## 다양한 k값을 가질 때

accuracy_df <- data.frame(k = seq(1,3000,1), accuracy = rep(0,3000))
str(train_norm_bk)


for (i in 1:3000) {
  knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, valid_norm_bk[,14]),positive="1")$overall[1]
}

View(accuracy_df)

# 자 255 일때 k가 멈췄어. 그 이후는 다 0
# 이유는 k가 203을 넘는 순간부터 valid 자료에 1보다 0이 많기 때문이야.
# 그럼 이때부터 직접 하자.

knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 1)
A <-table(knn_pred, valid_norm_bk[, 14])
confusionMatrix(A, positive = "1")


######## e. train, valid, test (5/3/2)

Bank <- read.csv("UniversalBank.csv")

View(Bank)

# 3개 이상 범주형 변수 -> 가변수 변호

Bank$Education <- as.factor(Bank$Education)

edu_type <- model.matrix( ~ 0 + Education, data = Bank)
str(edu_type)
edu_type <- as.data.frame(edu_type)
str(edu_type)

t(t(names(edu_type)))
edu_type

Bank <- cbind(Bank[, -c(1,5,8)],edu_type)
View(Bank)

# 더미변수를 한 개 제거하려 했으나
# 하지 않는다. 198p에 K-NN은 더미변수를 제거하지 않는다.고 써있다.
dim(Bank)
#Bank <- Bank[ ,-14]

# 종속변수 위치 조절
Bank <- Bank[,c(1:6,8:14,7)]

train_bank_index <- sample(row.names(Bank),0.5*dim(Bank)[1])
valid_bank_index <- sample(setdiff(row.names(Bank),train_bank_index), 0.3*dim(Bank)[1])
test_bank_index <- setdiff(row.names(Bank), union(train_bank_index,valid_bank_index))


train_df <- Bank[train_bank_index, ]
valid_df <- Bank[valid_bank_index, ]
test_df <- Bank[test_bank_index, ]


### 전처리 완료

set.seed(1234)

library(caret)
dim(train_df)

train_norm_bk <- train_df
valid_norm_bk <- valid_df
test_norm_bk <- test_df

norm.value <- preProcess(train_df[,1:13], method = c("center","scale"))

train_norm_bk[, 1:13] <- predict(norm.value, train_df[, 1:13])
valid_norm_bk[, 1:13] <- predict(norm.value, valid_df[, 1:13])
test_norm_bk[, 1:13] <- predict(norm.value, test_df[,1:13])

library(FNN)

## Training ~ Valid

accuracy_df <- data.frame(k = seq(1,2500,1), accuracy = rep(0,2500))
str(train_norm_bk)

for (i in 1:2500) {
  knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, test_norm_bk[,14]),positive="1")$overall[1]
}

View(accuracy_df)

# k = 1, 3, 5, 7

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 1)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 3)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 5)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 7)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

## 근접한 k값만 확인할 때
nn <- knn(train = train_norm_bk[,1:13],test = valid_norm_bk[,1:13],
          cl = train_norm_bk[ , 14],k = 1)

row.names(train_df)[attr(nn,"nn.index")]




## Training ~ Test

accuracy_df <- data.frame(k = seq(1,2500,1), accuracy = rep(0,2500))
str(train_norm_bk)

for (i in 1:2500) {
  knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, test_norm_bk[,14],positive="1"))$overall[1]
}

dim(test_norm_bk)

for (i in 1:2500) {
  knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, test_norm_bk[,14]),positive="1")$overall[1]
}

View(accuracy_df)



## Training ~ Valid

accuracy_df <- data.frame(k = seq(1,2500,1), accuracy = rep(0,2500))
str(train_norm_bk)

for (i in 1:2500) {
  knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, test_norm_bk[,14]),positive="1")$overall[1]
}

View(accuracy_df)

# k = 1, 3, 5, 7

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 1)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 3)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 5)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 7)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

## 근접한 k값만 확인할 때
nn <- knn(train = train_norm_bk[,1:13],test = valid_norm_bk[,1:13],
          cl = train_norm_bk[ , 14],k = 1)

row.names(train_df)[attr(nn,"nn.index")]




##### Validation ~ Test

accuracy_df <- data.frame(k = seq(1,1500,1), accuracy = rep(0,1500))
str(valid_norm_bk)

table(is.na(valid_norm_bk))

for (i in 1:1500) {
  knn_pred <- knn(valid_norm_bk[,1:13], test_norm_bk[,1:13],
                  cl = valid_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, test_norm_bk[,14]),positive = "1")$overall[1]
}

View(accuracy_df)

knn_pred <- knn(valid_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = valid_norm_bk[,14], k = 1)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(valid_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = valid_norm_bk[,14], k = 3)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")

knn_pred <- knn(valid_norm_bk[,1:13], test_norm_bk[,1:13],
                cl = valid_norm_bk[,14], k = 5)
A <-table(knn_pred, test_norm_bk[, 14])
confusionMatrix(A, positive = "1")



## Training ~ Test


accuracy_df <- data.frame(k = seq(1,2500,1), accuracy = rep(0,2500))
str(train_norm_bk)

for (i in 1:2500) {
  knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                  cl = train_norm_bk[,14], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, valid_norm_bk[,14]),positive="1")$overall[1]
}

View(accuracy_df)

knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 1)
B <- table(knn_pred, valid_norm_bk[,14])
confusionMatrix(B, positive = "1")

knn_pred <- knn(train_norm_bk[,1:13], valid_norm_bk[,1:13],
                cl = train_norm_bk[,14], k = 3)
B <- table(knn_pred, valid_norm_bk[,14])
confusionMatrix(B, positive = "1")
