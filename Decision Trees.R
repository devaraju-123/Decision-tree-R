data1 = read.csv('C:/Users/phsivale/Documents/Trainings/dataMerged.csv')
str(data1)
data1 = na.omit(data1)

colsToFactors = c('loan','online','securities','edu','family','cc','cd')
for(i in colsToFactors){
  data1[,i] = as.factor(data1[,i])
}

## train test split
rows = 1:nrow(data1)
train_rows = sample(rows,round(0.8*nrow(data1)))
test_rows = rows[-train_rows]
train_data = data1[train_rows,]
test_data = data1[test_rows,]

str(train_data)

## decsionTree
library(rpart)
dtree1 = rpart(loan ~.,data=train_data,control = c(cp=0.001,maxdepth=2))#control = c(cp=0.01)
dtree1

plot(dtree1,main="Classification Tree for loan Class",
     margin=0.1,uniform=TRUE)
text(dtree1,use.n=T)


library(rpart.plot)
rpart.plot(dtree1)


preds = predict(dtree1,test_data)
preds = as.data.frame(preds)
View(preds)
preds$preds_Class = ifelse(preds$`1` > 0.5,1,0)
table(test_data$loan,preds$preds_Class,dnn=c('actuals','preds'))

### 
library(C50)

dtree2 = C5.0(loan ~.,data=train_data)
plot(dtree2)

preds = predict(dtree2, test_data)
table(test_data$loan,preds,dnn=c('actuals','preds'))

#### Cross validation through Caret
library(caret)
train_control<- trainControl(method="cv", number=10)
model<- train(loan ~.,data=train_data, trControl=train_control, method="C5.0")
preds = predict(model,test_data)
table(preds,test_data$loan)
