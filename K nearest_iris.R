install.packages("caTools")
insatll.packages("class")
library(e1071)
library(caTools)
library(class)
library(ggplot2)

iris<-read.csv("dataset/iris.csv")
head(iris)
tail(iris)
str(iris)

split<-sample.split(iris,SplitRatio=0.7)
train_cl=subset(iris,split==TRUE)
test_cl=subset(iris,split==FALSE)

train_scale=scale(train_cl[,1:4])
test_scale=scale(test_cl[,1:4])

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=1)
knn_classifier
cm<-table(test_cl$variety,knn_classifier)
cm          
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=3)
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=5)
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=7)
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=15)
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=19)
misClassError=mean(knn_classifier!=test_cl$variety)
print(paste("Accuracy:",1 - misClassError))

k_values=c(1,3,5,7,15,19)
accuracy<-sapply(k_values,function(k){
  knn_classifier<-knn(train=train_scale,test=test_scale,cl=train_cl$variety,k=k)
  1-mean(knn_classifier!=test_cl$variety)
})
dev.off()#To close current graphical device(use to display the below graph without errors)
accuracy_data=data.frame(k=k_values,accuracy=accuracy)
ggplot(accuracy_data,aes(x=k,y=accuracy))+
         geom_line(color="green",size=1)+
         geom_point(color="blue",size=3)+
         labs(title="K nearest Alg",x="K Values",y="Accuracy")+
         theme_minimal()
       
       
