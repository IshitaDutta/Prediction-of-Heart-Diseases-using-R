#KNN
donees<-read.table(file="heart.txt",dec=" ",header=TRUE)
summary(donees)
arbre.full<-rpart(coeur~ .,data=donees,method="class")
print(arbre.full)

library(rpart)
library(rpart.plot)
plot(arbre.full,margin=0.1,main="Occurece of heart diseases")
text(arbre.full, use.n=TRUE, all=TRUE, cex=.7)
library(rattle)
fancyRpartPlot(arbre.full)
printcp(arbre.full)
plotcp(arbre.full, minline = TRUE)
arbre.full1<-prune(arbre.full,cp= 0.017)
fancyRpartPlot(arbre.full1)
arbre.full1
#Predicting accuracy 
pred<-predict(arbre.full,newdata=donees,type="class")
mc<-table(donees$coeur,pred)
print(mc)
err.resub<-1.0-(mc[1,1]+mc[2,2])/sum(mc)
print(err.resub)
#DECSION TREE
actual<-donees$coeur
head(actual)
actual<-as.numeric(actual)
head(actual)
actual<-ifelse(actual==2,1,0)
head(actual)
predicted<-predict(arbre.full1,type = "class")  #The decision tree model created predicts the Target data
predicted
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)
head(predicted)
#Accuracy by Confusion matrix
confusionMatrix(predicted,actual,positive="1") #87% 

#Accuracy by Area under the curve, ROCR
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr") #compares and gives the performance of actual and predicted
plot(perf,col="magenta", main= "Graph to find accuracy")
abline(0,1, lty = 15, col = "black")
#dev.off()


auc<-performance(pred,"auc")
a<-unlist(auc@y.values) #In lis
paste("The accuracy of the decision tree is", a)
