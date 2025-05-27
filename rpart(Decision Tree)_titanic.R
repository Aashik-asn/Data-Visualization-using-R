library(rpart.plot)
library(ggplot2)
library(caret)
library(dplyr)

titanic<-read.csv("dataset/titanic.csv")
head(titanic)
tail(titanic)
str(titanic)

titanic=select(titanic,Survived,Pclass,Sex,SibSp,Parch)
titanic=na.omit(titanic)

titanic$Pclass=factor(titanic$Pclass,order=TRUE,levels=c(3,2,1))
titanic$Survived=factor(titanic$Survived)

ggplot(titanic,aes(x=Survived))+
  geom_bar(width=0.5,fill="coral")+
  geom_text(stat='count',aes(label=stat(count)),vjust=-0.5)+
  theme_classic()

train_test_split=function(data,fraction=0.8,train=TRUE){
  tot_rows=nrow(data)
  train_rows=fraction*tot_rows
  sample=1:train_rows
  if(train==TRUE){
    return (data[sample,])
  }
  else{
    return (data[-sample,])
  }
}
train_data=train_test_split(titanic,0.8,train=TRUE)
test_data=train_test_split(titanic,0.8,train=FALSE)
fit=rpart(Survived~.,data=train_data,method="class")
predicted=predict(fit,test_data,type="class")
rpart.plot(fit,extra=106)
table=table(test_data$Survived,predicted)
table
accuracy=sum(diag(table))/sum(table)
print(paste("Accuracy: ",accuracy))
