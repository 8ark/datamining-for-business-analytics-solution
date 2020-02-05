eBay <-read.csv("eBayAuctions.csv")
View(eBay)
str(eBay)

eBay$Duration <- as.factor(eBay$Duration)
eBay$Competitive.<-as.factor(eBay$Competitive.)

set.seed(32121391)

train.row<-sample(rownames(eBay), dim(eBay)[1]*0.6)
valid.row<-setdiff(rownames(eBay), train.row) 
train.ebay<-eBay[train.row,]
valid.ebay<-eBay[valid.row,]

# a. 분류나무 minbucket=50, maxdepth=7 
library(rpart)
library(rpart.plot)
class.ebay<-rpart(Competitive. ~ ., data=train.ebay, 
                  control=rpart.control(minbucket = 50, maxdepth = 7), method="class")

prp(class.ebay, type=1, extra=1, varlen=0)

class.ebay.point.valid <- predict(class.ebay,valid.ebay,type = "class")
# generate confusion matrix for training data

out<-table(class.ebay.point.valid,valid.ebay$Competitive.)
# 실제값과 예측값의 분류교차표 작성  

out
(out[1,1]+out[2,2])/sum(out)



#c
cv.ct <-rpart(Competitive.~., data = train.ebay, method = "class",
              cp = 0.00001, minsplit = 50, xval = 5)

control=rpart.control(minbucket = 50, maxdepth = 7)

cv.ct <-rpart(Competitive.~., data = train.ebay, method = "class",
              cp = 0.00001, minsplit = 50, xval = 5, control = rpart.control(minbucket = 50, maxdepth = 7))

printcp(cv.ct)

## 적절한 nsplit = 6

#d
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


cv.ct <-rpart(Competitive.~., data = pruned.ct, method = "class",
              cp = 0.00001, minsplit = 50, xval = 5, control = rpart.control(minbucket = 50, maxdepth = 7))

printcp(cv.ct)




#e

ebay_e <- eBay[,6:8]
library(ggplot2)

ggplot(data = ebay_e, aes(x = ebay_e$OpenPrice, y = ebay_e$ClosePrice, color = ebay_e$Competitive.))+
  geom_point()


graph + geom_hline(aes(yintercept=2.65)) + geom_vline(aes(xintercept=0.8)) + geom_vline(aes(xintercept=1.75)) + geom_vline(aes(xintercept=1.35))

library(party)

tree1 <- tree(Species ~ Sepal.Width + Petal.Width, data = iris)
summary(tree1)
Classification tree:
  tree(formula = Species ~ Sepal.Width + Petal.Width, data = iris)
Number of terminal nodes:  5 
Residual mean deviance:  0.204 = 29.57 / 145 
Misclassification error rate: 0.03333 = 5 / 150
plot(tree1)
text(tree1)

install.packages("tree")
library(tree)
eb<-eBay
plot(eBay$OpenPrice, eBay$ClosePrice, pch=c(19,19), col=as.numeric(eb$Competitive.))
tree1 <- tree(Competitive. ~ OpenPrice + ClosePrice, data = eb)
summary(tree1)
partition.tree(tree1,label="Competetive.",add=TRUE)

