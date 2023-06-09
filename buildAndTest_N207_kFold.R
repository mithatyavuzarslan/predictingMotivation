######## ANALİZ 207 düzeltme sonrası #####
### 1'den sonra hepsi 1er artacak###

url<-"http://www.cet110.com/finalSetSon_Mor.csv"
Ogrenciler<-as.data.frame(read.csv(file=url,header=TRUE,sep=';',dec=','))
colnames(Ogrenciler)


df <- Ogrenciler[ -c(1,4,(6:11),17,24,(28:30),(33:89))]
colnames(df)

for (i in 1:nrow(df)) {
  if(df[i,"Faculty"]=="Mühendislik")
  {
    df[i,"Faculty"]=1
  }
  else df[i,"Faculty"]=0
  
}
df$Faculty<-as.numeric(df$Faculty)



for (i in 1:nrow(df)) {
  if(df[i,"Gender"]=="Erkek")
  {
    df[i,"Gender"]=0
  }
  else df[i,"Gender"]=1
  
}
df$Gender<-as.numeric(df$Gender)

df<-as.data.frame(df)


############### korelasyon ##########
library(corrplot)
colnames(df)[ncol(df)]<-"motivation"
df2<-df

#kolonIsimleri<-NULL
#for (i in 1:ncol(df2)) {

#  kolonIsimleri<-c(kolonIsimleri,paste("K",i))

#}

library(stringr)
#kolonIsimleri<-str_replace(kolonIsimleri," ","")

summary(df2)
#colnames(df2)<-kolonIsimleri 
#help(cor)

korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon

corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.6)

df2<-df2[,c(-3,-5)]

korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.6)

df<-df2



for (i in 1:nrow(df)) {
  if(df[i,"motivation"]>=4.0)
  {
    df[i,"motivation"]=1
  }
  else df[i,"motivation"]=0
  
}
table(df$motivation)

library(clusterSim)


df[,-ncol(df)]<-data.Normalization(df[,-ncol(df)],type="n4",normalization ="column")

df$motivation<-as.factor(df$motivation)
levels(df$motivation)
table(df$motivation)

levels(df$motivation)
levels(df$motivation) <- c("dusuk", "yuksek")

library(clipr)
######## KNN ###########

## Accuracy 5 ###
library(caret)
set.seed(861)
ctrl_knn1 <- trainControl(method = "cv", number = 5)
set.seed(862)
model_knn1<-train(motivation~.,data=df, method='knn', trControl=ctrl_knn1)
print(model_knn1)
print(model_knn1$results)
print(model_knn1$resample)
write_clip(model_knn1$results)

## Accuracy 10 ##
set.seed(863)
ctrl_knn2 <- trainControl(method = "cv", number = 10)
#returnResamp = 'all'
set.seed(864)
model_knn2<-train(motivation~.,data=df, method='knn', trControl=ctrl_knn2)
# tuneLength ile k sayısı belirlenebilir.
print(model_knn2)
print(model_knn2$results)
print(model_knn2$resample)
write_clip(model_knn2$results)

## ROC 5 ###
set.seed(865)
ctrl_knn3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(866)
model_knn3<-train(motivation~.,data=df, method='knn', trControl=ctrl_knn3, metric="ROC")
print(model_knn3)
print(model_knn3$results)
print(model_knn3$resample)
### clipboarda kopyalar
write_clip(model_knn3$results)

## ROC 10 ##
set.seed(867)
ctrl_knn4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(868)
model_knn4<-train(motivation~.,data=df, method='knn', trControl=ctrl_knn4, metric="ROC")
print(model_knn4)
print(model_knn4$results)
print(model_knn4$resample)
write_clip(model_knn4$results)




########## NAIVE BAYES ###########

## Accuracy 5 ###

set.seed(869)
ctrl_nb1 <- trainControl(method = "cv", number = 5)
set.seed(870)
model_nb1<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_nb1)
print(model_nb1)
print(model_nb1$results)
print(model_nb1$resample)
write_clip(model_nb1$results)


## Accuracy 10 ##
set.seed(871)
control_nb2 <- trainControl(method = "cv", number = 10)
#returnResamp = 'all'
set.seed(872)
model_nb2<-train(motivation~.,data=df, method='naive_bayes', trControl=control_nb2)
# tuneLength ile k sayısı belirlenebilir.
print(model_nb2)
print(model_nb2$results)
print(model_nb2$resample)
write_clip(model_nb2$results)

## ROC 5 ##

set.seed(873)
ctrl_nb3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb3<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_nb3, metric="ROC")
print(model_nb3)
print(model_nb3$results)
print(model_nb3$resample)
### clipboarda kopyalar
write_clip(model_nb3$results)

## ROC 10 ##

set.seed(873)
ctrl_nb4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb4<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_nb4, metric="ROC")
print(model_nb4)
print(model_nb4$results)
print(model_nb4$resample)
### clipboarda kopyalar
write_clip(model_nb4$results)

########### SVM ############

## Accuracy 5 ###

set.seed(875)
ctrl_svm1 <- trainControl(method = "cv", number = 5)
set.seed(876)
model_svm1<-train(motivation~.,data=df, method='svmLinear', trControl=ctrl_svm1)
print(model_svm1)
print(model_svm1$results)
print(model_svm1$resample)
write_clip(model_svm1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(877)
ctrl_svm2 <- trainControl(method = "cv", number = 10)
set.seed(878)
model_svm2<-train(motivation~.,data=df, method='svmLinear', trControl=ctrl_svm2)
print(model_svm2)
print(model_svm2$results)
print(model_svm2$resample)
write_clip(model_svm2$results)


#SVMtahminimiz<-predict(model_svm1, test_Attributes)
#SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=df$motivation,mode="everything",positive = "yuksek")
#SVMtablomuz
#table(SVMtablomuz)
#write_clip(SVMtablomuz)

## ROC 5 ##

set.seed(879)
ctrl_svm3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(880)
model_svm3<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_svm3, metric="ROC")
print(model_svm3)
print(model_svm3$results)
print(model_svm3$resample)
### clipboarda kopyalar
write_clip(model_svm3$results)

## ROC 10 ##

set.seed(881)
ctrl_svm4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(882)
model_svm4<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_svm4, metric="ROC")
print(model_svm4)
print(model_svm4$results)
print(model_svm4$resample)
### clipboarda kopyalar
write_clip(model_svm4$results)


####### c50 #######

## Accuracy 5 ###

set.seed(883)
ctrl_c501 <- trainControl(method = "cv", number = 5)
set.seed(884)
model_c501<-train(motivation~.,data=df, method='C5.0', trControl=ctrl_c501)
print(model_c501)
print(model_c501$results)
print(model_c501$resample)
write_clip(model_c501$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(885)
ctrl_c502 <- trainControl(method = "cv", number = 10)
set.seed(886)
model_c502<-train(motivation~.,data=df, method='C5.0', trControl=ctrl_c502)
print(model_c502)
print(model_c502$results)
print(model_c502$resample)
write_clip(model_c502$results)


#SVMtahminimiz<-predict(model_svm1, test_Attributes)
#SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=df$motivation,mode="everything",positive = "yuksek")
#SVMtablomuz
#table(SVMtablomuz)
#write_clip(SVMtablomuz)

## ROC 5 ##

set.seed(887)
ctrl_c503 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(888)
model_c503<-train(motivation~.,data=df, method='C5.0', trControl=ctrl_c503, metric="ROC")
print(model_c503)
print(model_c503$results)
print(model_c503$resample)
### clipboarda kopyalar
write_clip(model_c503$results)

## ROC 10 ##

set.seed(889)
ctrl_c504 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(890)
model_c504<-train(motivation~.,data=df, method='C5.0', trControl=ctrl_c504, metric="ROC")
print(model_c504)
print(model_c504$results)
print(model_c504$resample)
### clipboarda kopyalar
write_clip(model_c504$results)




############ CART ###############

## Accuracy 5 ###

set.seed(891)
ctrl_CART1 <- trainControl(method = "cv", number = 5)
set.seed(892)
model_CART1<-train(motivation~.,data=df, method='rpart1SE', trControl=ctrl_CART1)
print(model_CART1)
print(model_CART1$results)
print(model_CART1$resample)
write_clip(model_CART1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

rpart.plot(model_CART1$finalModel,type=3,fallen.leaves = TRUE,cex=0.9, roundint=FALSE)

## Accuracy 10 ##
set.seed(893)
ctrl_CART2 <- trainControl(method = "cv", number = 10)
set.seed(894)
model_CART2<-train(motivation~.,data=df, method='rpart1SE', trControl=ctrl_CART2)
print(model_CART2)
print(model_CART2$results)
print(model_CART2$resample)
write_clip(model_CART2$results)


#SVMtahminimiz<-predict(model_svm1, test_Attributes)
#SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=df$motivation,mode="everything",positive = "yuksek")
#SVMtablomuz
#table(SVMtablomuz)
#write_clip(SVMtablomuz)

## ROC 5 ##

set.seed(895)
ctrl_CART3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(896)
model_CART3<-train(motivation~.,data=df, method='rpart1SE', trControl=ctrl_CART3, metric="ROC")
print(model_CART3)
print(model_CART3$results)
print(model_CART3$resample)
### clipboarda kopyalar
write_clip(model_CART3$results)

## ROC 10 ##

set.seed(897)
ctrl_CART4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(898)
model_CART4<-train(motivation~.,data=df, method='rpart1SE', trControl=ctrl_CART4, metric="ROC")
print(model_CART4)
print(model_CART4$results)
print(model_CART4$resample)
### clipboarda kopyalar
write_clip(model_CART4$results)
rpart.plot(model_CART4$finalModel,type=3,fallen.leaves = TRUE,cex=0.9, roundint=FALSE, space=0.1)


########## RANDOM FOREST ###########

## Accuracy 5 ###

set.seed(899)
ctrl_RF1 <- trainControl(method = "cv", number = 5)
set.seed(900)
model_RF1<-train(motivation~.,data=df, method='rf', trControl=ctrl_RF1)
print(model_RF1)
print(model_RF1$results)
print(model_RF1$resample)
write_clip(model_RF1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(901)
ctrl_RF2 <- trainControl(method = "cv", number = 10)
set.seed(902)
model_RF2<-train(motivation~.,data=df, method='rf', trControl=ctrl_RF2)
print(model_RF2)
print(model_RF2$results)
print(model_RF2$resample)
write_clip(model_RF2$results)


#SVMtahminimiz<-predict(model_svm1, test_Attributes)
#SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=df$motivation,mode="everything",positive = "yuksek")
#SVMtablomuz
#table(SVMtablomuz)
#write_clip(SVMtablomuz)

## ROC 5 ##

set.seed(903)
ctrl_RF3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(904)
model_RF3<-train(motivation~.,data=df, method='rf', trControl=ctrl_RF3, metric="ROC")
print(model_RF3)
print(model_RF3$results)
print(model_RF3$resample)
### clipboarda kopyalar
write_clip(model_RF3$results)

## ROC 10 ##

set.seed(905)
ctrl_RF4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(906)
model_RF4<-train(motivation~.,data=df, method='rf', trControl=ctrl_RF4, metric="ROC")
print(model_RF4)
print(model_RF4$results)
print(model_RF4$resample)
### clipboarda kopyalar
write_clip(model_RF4$results)


########## Log. REg. ###########

## Accuracy 5 ###

set.seed(907)
ctrl_LR1 <- trainControl(method = "cv", number = 5)
set.seed(908)
model_LR1<-train(motivation~.,data=df, method='glm', trControl=ctrl_LR1)
print(model_LR1)
print(model_LR1$results)
print(model_LR1$resample)
write_clip(model_LR1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(909)
ctrl_LR2 <- trainControl(method = "cv", number = 10)
set.seed(910)
model_LR2<-train(motivation~.,data=df, method='glm', trControl=ctrl_LR2)
print(model_LR2)
print(model_LR2$results)
print(model_LR2$resample)
write_clip(model_LR2$results)


#SVMtahminimiz<-predict(model_svm1, test_Attributes)
#SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=df$motivation,mode="everything",positive = "yuksek")
#SVMtablomuz
#table(SVMtablomuz)
#write_clip(SVMtablomuz)

## ROC 5 ##

set.seed(911)
ctrl_LR3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(912)
model_LR3<-train(motivation~.,data=df, method='glm', trControl=ctrl_LR3, metric="ROC")
print(model_LR3)
print(model_LR3$results)
print(model_LR3$resample)
### clipboarda kopyalar
write_clip(model_LR3$results)

## ROC 10 ##

set.seed(913)
ctrl_LR4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(914)
model_LR4<-train(motivation~.,data=df, method='glm', trControl=ctrl_LR4, metric="ROC")
print(model_LR4)
print(model_LR4$results)
print(model_LR4$resample)
### clipboarda kopyalar
write_clip(model_LR4$results)












set.seed(873)
ctrl_nb3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb3<-train(motivation~.,data=df, method='naive_bayes', trControl=ctrl_nb3, metric="ROC")
print(model_nb3)
print(model_nb3$results)
print(model_nb3$resample)
### clipboarda kopyalar
write_clip(model_nb3$results)




## ROC 10 ## 










ctrl <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
summary(ctrl)


set.seed(861)
model<-train(motivation~.,data=df, method='rpart',trControl=ctrl, metric="ROC")



set.seed(861)
ctrl <- trainControl(method = "cv", number = 10)

set.seed(862)
model<-train(motivation~.,data=df, method='rpart', trControl=ctrl)

print(model)
model$resample
class(model$finalModel)

ctrl1<- trainControl(method = "cv", number = 5)
model1<-train(motivation~.,data=df, method='knn', trControl=ctrl1)
print(model1)
model1$resample
class(model1$finalModel)

CARTmodelimiz<- rpart(motivation~., train_data,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz<-predict(CARTmodelimiz,test_Attributes,type="class")
CARTtablomuz<-confusionMatrix(data=CARTtahminimiz,reference=test_Target,mode="everything", positive="1")

