Bank <- read.csv("UniversalBank.csv")

View(Bank)

set.seed(1234)
# 3개 이상 범주형 변수 -> 가변수 변호

Bank$Education <- as.factor(Bank$Education)

edu_type <- model.matrix( ~ 0 + Education, data = Bank)
str(edu_type)
edu_type <- as.data.frame(edu_type)
str(edu_type)

t(t(names(edu_type)))
edu_type

Bank <- cbind(Bank[, -c(1,5)],edu_type)
View(Bank)

# 더미변수를 한 개 제거하려 했으나
# 하지 않는다. 198p에 K-NN은 더미변수를 제거하지 않는다.고 써있다.
dim(Bank)
#Bank <- Bank[ ,-14]

# 종속변수 위치 조절
Bank <- Bank[,c(1:6,8:15,7)]

######################################################
# 전처리 끝

# rownames 는 행에 이름 붙이는 것, 고로 얘는 숫자
train_bank_index <- sample(row.names(Bank),0.6*dim(Bank)[1])
valid_bank_index <- setdiff(row.names(Bank), train_bank_index)

train_df <- Bank[train_bank_index, ]
valid_df <- Bank[valid_bank_index, ]

View(train_df)

### a ###

library(caret)
dim(train_df)

train_norm_bk <- train_df
valid_norm_bk <- valid_df
Bank_norm_bk <- Bank

norm.value <- preProcess(train_df[,1:14], method = c("center","scale"))

train_norm_bk[, 1:14] <- predict(norm.value, train_df[, 1:14])
valid_norm_bk[, 1:14] <- predict(norm.value, valid_df[, 1:14])
Bank_norm_bk[, 1:14] <- predict(norm.value, Bank[,1:14])

library(FNN)

###################### 근접한 k값만 확인할 때
nn <- knn(train = train_norm_bk[,1:13],test = valid_norm_bk[,1:13],
          cl = train_norm_bk[ , 14],k = 3)

row.names(train_df)[attr(nn,"nn.index")]
##########################

## 다양한 k값을 가질 때

accuracy_df <- data.frame(k = seq(1,3000,1), accuracy = rep(0,3000))
str(train_norm_bk)

for (i in 1:3000) {
  knn_pred <- knn(train_norm_bk[,1:14], valid_norm_bk[,1:14],
                  cl = train_norm_bk[,15], k = i)
  accuracy_df[i,2] <- confusionMatrix(table(knn_pred, valid_norm_bk[,15]))$overall[1]
}
table(is.na(knn_pred))

str(knn_pred)
?confusionMatrix

summary(accuracy_df)
View(knn_pred)
