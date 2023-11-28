######### motivasyonlu ve UA anketsizler için validation #########

url_m<-"https://raw.githubusercontent.com/mithatyavuzarslan/predictingMotivation/main/finalSetSon_Mor.csv"
Ogrenciler_m<-as.data.frame(read.csv(file=url_m, header=TRUE,sep=';',dec=','))

colnames(Ogrenciler_m)


df_m <- Ogrenciler_m[ -c(1,4,(6:11),17,24,(28:30),(33:89))]

df_m<- df_m[-c(9,10,13,15,16,17)]
colnames(df_m)

for (i in 1:nrow(df_m)) {
  if(df_m[i,"Faculty"]=="Mühendislik")
  {
    df_m[i,"Faculty"]=1
  }
  else df_m[i,"Faculty"]=0
  
}
df_m$Faculty<-as.numeric(df_m$Faculty)

for (i in 1:nrow(df_m)) {
  if(df_m[i,"Gender"]=="Erkek")
  {
    df_m[i,"Gender"]=0
  }
  else df_m[i,"Gender"]=1
  
}
df_m$Gender<-as.numeric(df_m$Gender)

#hist(df_m$ESAS_ORT,labels=TRUE, main='I. SET "Motivation" Özniteliği (N=207)',xlab='Motivasyon_Ortalama', ylim = c(0, 100))
#########################


url_v<-"https://raw.githubusercontent.com/mithatyavuzarslan/predictingMotivation/main/validationSet2_.csv"
Ogrenciler_v<-as.data.frame(read.csv(file=url_v,header=TRUE,sep=';',dec=','))
df_v <- Ogrenciler_v[ -c(1,4,(6:11),17,24,(28:30),(33:89))]
df_v<- df_v[-c(9,10,13,15,16,17)]
#library(dplyr)
#df_v$Faculty<-as.numeric(recode(df_v$Faculty, "Mühendislik" = "1",
#                    "Fen-Edebiyat"="2",
#                    "İktisat"="3",
#                    "Güzel Sanatlar"="4",
#                    "Sağlık Bilimleri"="5",
#                    "Mimarlık"="6",
#                    "İletişim"="7",
#                    "Ticari Bilimler"="8",
#                      "Eğitim"="9",
#                      "Eczacılık"="10"))




#df_v[df_v == ""] <- NA
for (i in 1:nrow(df_v)) {
  if(df_v[i,"Faculty"]=="Mühendislik")
  {
    df_v[i,"Faculty"]=1
  }
  else if(df_v[i,"Faculty"]=="")
  {
    df_v[i,"Faculty"]=""
  } else
    df_v[i,"Faculty"]=0
  
}

df_v$Faculty<-as.numeric(df_v$Faculty)


library(mice)


for (i in 1:nrow(df_v)) {
  if(df_v[i,"Gender"]=="Erkek")
  {
    df_v[i,"Gender"]=0
  }
  else df_v[i,"Gender"]=1
  
}
df_v$Gender<-as.numeric(df_v$Gender)
#df<-as.data.frame(df)
############# KORELASYON #################

library(corrplot)
df2<-df_m

kolonIsimleri<-NULL
for (i in 1:ncol(df2)) {
  
  kolonIsimleri<-c(kolonIsimleri,paste("K",i))
  
}

library(stringr)
#kolonIsimleri<-str_replace(kolonIsimleri," ","")

#summary(df2)
#colnames(df2)<-kolonIsimleri

#df2$K12<-as.numeric(df2$K12)
#help(cor)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number",)

df2<-df2[,c(-3,-5)]

korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number",)

df_m<-df2

colnames(df_m)[ncol(df_m)]<-"motivation"
colnames(df_v)[ncol(df_v)]<-"motivation"
hist(df_m$motivation)

## modelde çıktığı için validationda da çıkmalı!!##

df_v<-df_v[,c(-3,-5)]
###############################

for (i in 1:nrow(df_m)) {
  if(df_m[i,"motivation"]>=4.0)
  {
    df_m[i,"motivation"]=1
  }
  else df_m[i,"motivation"]=0
  
}
table(df_m$motivation)

for (i in 1:nrow(df_v)) {
  if(df_v[i,"motivation"]>=4.0)
  {
    df_v[i,"motivation"]=1
  }
  else df_v[i,"motivation"]=0
  
}
table(df_v$motivation)
#FREKANS GRAFİKLERİ İÇİNNN

# hist(df_m$motivation,labels=TRUE, main='I. SET "Motivation" Özniteliği (N=207)',xlab='Motivasyon_Ortalama', xlim=c(0,1),ylim=c(0,200), breaks=2, xaxt="n")
# axis(side=1, at=c(1, 0))

histicin<-df_v$motivation
levels(histicin)
levels(histicin)<-c("0","1")
table(histicin)
hist(as.numeric(as.character(histicin)),labels=TRUE, main='II. SET (N=200)\n"Motivation"  Heden niteliği\n0:Düşük 1:Yüksek',xlab='Motivasyon', xlim=c(0,1),ylim=c(0,200), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))

library(clusterSim)


df_m[,-ncol(df_m)]<-data.Normalization(df_m[,-ncol(df_m)],type="n4",normalization ="column")
df_m$motivation<-as.factor(df_m$motivation)
levels(df_m$motivation)
table(df_m$motivation)

df_v[,-ncol(df_v)]<-data.Normalization(df_v[,-ncol(df_v)],type="n4",normalization ="column")
df_v$motivation<-as.factor(df_v$motivation)
levels(df_v$motivation)
table(df_v$motivation)

levels(df_m$motivation) <- c("dusuk", "yuksek")
levels(df_v$motivation) <- c("dusuk", "yuksek")

validation_Test<-df_v[,-ncol(df_v)]
validation_TestTarget<-df_v[[ncol(df_v)]]


#set.seed(400)
#df_m<-ovun.sample(motivation~.,data=df_m,method="over",N=286)$data
#table(df_m$motivation)




######## KNN ###########

## Accuracy 5 ###
library(caret)
set.seed(861)
ctrl_knn1 <- trainControl(method = "cv", number = 5)
set.seed(862)
model_knn1<-train(motivation~.,data=df_m, method='knn', trControl=ctrl_knn1)

prediction_knn1<-predict(model_knn1,validation_Test)
knn_table1<-confusionMatrix(data=prediction_knn1,reference=validation_TestTarget,mode="everything",positive="yuksek")

print(model_knn1)
print(model_knn1$results)
print(model_knn1$resample)
write_clip(model_knn1$results)




## Accuracy 10 ##
set.seed(863)
ctrl_knn2 <- trainControl(method = "cv", number = 10)
#returnResamp = 'all'
set.seed(864)
model_knn2<-train(motivation~.,data=df_m, method='knn', trControl=ctrl_knn2)
# tuneLength ile k sayısı belirlenebilir.

prediction_knn2<-predict(model_knn2,validation_Test)
knn_table2<-confusionMatrix(data=prediction_knn2,reference=validation_TestTarget,mode="everything",positive="yuksek")


print(model_knn2)
print(model_knn2$results)
print(model_knn2$resample)
write_clip(model_knn2$results)

## ROC 5 ###
set.seed(865)
ctrl_knn3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(866)
model_knn3<-train(motivation~.,data=df_m, method='knn', trControl=ctrl_knn3, metric="ROC")

prediction_knn3<-predict(model_knn3,validation_Test)
knn_table3<-confusionMatrix(data=prediction_knn3,reference=validation_TestTarget,mode="everything",positive="yuksek")


print(model_knn3)
print(model_knn3$results)
print(model_knn3$resample)
### clipboarda kopyalar
write_clip(model_knn3$results)

## ROC 10 ##
set.seed(867)
ctrl_knn4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(868)
model_knn4<-train(motivation~.,data=df_m, method='knn', trControl=ctrl_knn4, metric="ROC")

prediction_knn4<-predict(model_knn1,validation_Test)
knn_table4<-confusionMatrix(data=prediction_knn4,reference=validation_TestTarget,mode="everything",positive="yuksek")


print(model_knn4)
print(model_knn4$results)
print(model_knn4$resample)
write_clip(model_knn4$results)




########## NAIVE BAYES ###########

## Accuracy 5 ###

set.seed(869)
ctrl_nb1 <- trainControl(method = "cv", number = 5)
set.seed(870)
model_nb1<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_nb1)

prediction_nb1<-predict(model_nb1,validation_Test)
nb_table1<-confusionMatrix(data=prediction_nb1,reference=validation_TestTarget,mode="everything",positive="yuksek")
nb_table1



print(model_nb1)
print(model_nb1$results)
print(model_nb1$resample)
write_clip(model_nb1$results)


## Accuracy 10 ##
set.seed(871)
control_nb2 <- trainControl(method = "cv", number = 10)
#returnResamp = 'all'
set.seed(872)
model_nb2<-train(motivation~.,data=df_m, method='naive_bayes', trControl=control_nb2)

prediction_nb2<-predict(model_nb2,validation_Test)
nb_table2<-confusionMatrix(data=prediction_nb2,reference=validation_TestTarget,mode="everything",positive="yuksek")
nb_table2


# tuneLength ile k sayısı belirlenebilir.
print(model_nb2)
print(model_nb2$results)
print(model_nb2$resample)
write_clip(model_nb2$results)

## ROC 5 ##

set.seed(873)
ctrl_nb3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb3<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_nb3, metric="ROC")

prediction_nb3<-predict(model_nb3,validation_Test)
nb_table3<-confusionMatrix(data=prediction_nb3,reference=validation_TestTarget,mode="everything",positive="yuksek")
nb_table3


print(model_nb3)
print(model_nb3$results)
print(model_nb3$resample)
### clipboarda kopyalar
write_clip(model_nb3$results)

## ROC 10 ##

set.seed(873)
ctrl_nb4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb4<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_nb4, metric="ROC")

prediction_nb4<-predict(model_nb4,validation_Test)
nb_table4<-confusionMatrix(data=prediction_nb4,reference=validation_TestTarget,mode="everything",positive="yuksek")
nb_table4


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
model_svm1<-train(motivation~.,data=df_m, method='svmLinear', trControl=ctrl_svm1)

prediction_svm1<-predict(model_svm1,validation_Test)
svm_table1<-confusionMatrix(data=prediction_svm1,reference=validation_TestTarget,mode="everything",positive="yuksek")
svm_table1


print(model_svm1)
print(model_svm1$results)
print(model_svm1$resample)
write_clip(model_svm1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(877)
ctrl_svm2 <- trainControl(method = "cv", number = 10)
set.seed(878)
model_svm2<-train(motivation~.,data=df_m, method='svmLinear', trControl=ctrl_svm2)

prediction_svm2<-predict(model_svm2,validation_Test)
svm_table2<-confusionMatrix(data=prediction_svm2,reference=validation_TestTarget,mode="everything",positive="yuksek")
svm_table2


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
model_svm3<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_svm3, metric="ROC")

prediction_svm3<-predict(model_svm3,validation_Test)
svm_table3<-confusionMatrix(data=prediction_svm3,reference=validation_TestTarget,mode="everything",positive="yuksek")
svm_table3


print(model_svm3)
print(model_svm3$results)
print(model_svm3$resample)
### clipboarda kopyalar
write_clip(model_svm3$results)

## ROC 10 ##

set.seed(881)
ctrl_svm4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(882)
model_svm4<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_svm4, metric="ROC")

prediction_svm4<-predict(model_svm4,validation_Test)
svm_table4<-confusionMatrix(data=prediction_svm4,reference=validation_TestTarget,mode="everything",positive="yuksek")
svm_table4


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
model_c501<-train(motivation~.,data=df_m, method='C5.0', trControl=ctrl_c501)

prediction_c501<-predict(model_c501,validation_Test)
c50_table1<-confusionMatrix(data=prediction_c501,reference=validation_TestTarget,mode="everything",positive="yuksek")
c50_table1


print(model_c501)
print(model_c501$results)
print(model_c501$resample)
write_clip(model_c501$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(885)
ctrl_c502 <- trainControl(method = "cv", number = 10)
set.seed(886)
model_c502<-train(motivation~.,data=df_m, method='C5.0', trControl=ctrl_c502)

prediction_c502<-predict(model_c502,validation_Test)
c50_table2<-confusionMatrix(data=prediction_c502,reference=validation_TestTarget,mode="everything",positive="yuksek")
c50_table2

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
model_c503<-train(motivation~.,data=df_m, method='C5.0', trControl=ctrl_c503, metric="ROC")

prediction_c503<-predict(model_c503,validation_Test)
c50_table3<-confusionMatrix(data=prediction_c503,reference=validation_TestTarget,mode="everything",positive="yuksek")
c50_table3

print(model_c503)
print(model_c503$results)
print(model_c503$resample)
### clipboarda kopyalar
write_clip(model_c503$results)

## ROC 10 ##

set.seed(889)
ctrl_c504 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(890)
model_c504<-train(motivation~.,data=df_m, method='C5.0', trControl=ctrl_c504, metric="ROC")

prediction_c504<-predict(model_c504,validation_Test)
c50_table4<-confusionMatrix(data=prediction_c504,reference=validation_TestTarget,mode="everything",positive="yuksek")
c50_table4

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
model_CART1<-train(motivation~.,data=df_m, method='rpart1SE', trControl=ctrl_CART1)

prediction_CART1<-predict(model_CART1,validation_Test)
CART_table1<-confusionMatrix(data=prediction_CART1,reference=validation_TestTarget,mode="everything",positive="yuksek")
CART_table1


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
model_CART2<-train(motivation~.,data=df_m, method='rpart1SE', trControl=ctrl_CART2)

prediction_CART2<-predict(model_CART2,validation_Test)
CART_table2<-confusionMatrix(data=prediction_CART2,reference=validation_TestTarget,mode="everything",positive="yuksek")
CART_table2


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
model_CART3<-train(motivation~.,data=df_m, method='rpart1SE', trControl=ctrl_CART3, metric="ROC")

prediction_CART3<-predict(model_CART3,validation_Test)
CART_table3<-confusionMatrix(data=prediction_CART3,reference=validation_TestTarget,mode="everything",positive="yuksek")
CART_table3


print(model_CART3)
print(model_CART3$results)
print(model_CART3$resample)
### clipboarda kopyalar
write_clip(model_CART3$results)

## ROC 10 ##

set.seed(897)
ctrl_CART4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(898)
model_CART4<-train(motivation~.,data=df_m, method='rpart1SE', trControl=ctrl_CART4, metric="ROC")

prediction_CART4<-predict(model_CART4,validation_Test)
CART_table4<-confusionMatrix(data=prediction_CART4,reference=validation_TestTarget,mode="everything",positive="yuksek")
CART_table4

rpart.plot(model_CART4$finalModel,type=3,fallen.leaves = TRUE,cex=0.9, roundint=FALSE, space=0.1)

print(model_CART4)
print(model_CART4$results)
print(model_CART4$resample)
### clipboarda kopyalar
write_clip(model_CART4$results)


########## RANDOM FOREST ###########

## Accuracy 5 ###

set.seed(899)
ctrl_RF1 <- trainControl(method = "cv", number = 5)
set.seed(900)
model_RF1<-train(motivation~.,data=df_m, method='rf', trControl=ctrl_RF1)

prediction_RF1<-predict(model_RF1,validation_Test)
RF_table1<-confusionMatrix(data=prediction_RF1,reference=validation_TestTarget,mode="everything",positive="yuksek")
RF_table1



print(model_RF1)
print(model_RF1$results)
print(model_RF1$resample)
write_clip(model_RF1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(901)
ctrl_RF2 <- trainControl(method = "cv", number = 10)
set.seed(902)
model_RF2<-train(motivation~.,data=df_m, method='rf', trControl=ctrl_RF2)

prediction_RF2<-predict(model_RF2,validation_Test)
RF_table2<-confusionMatrix(data=prediction_RF2,reference=validation_TestTarget,mode="everything",positive="yuksek")
RF_table2



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
model_RF3<-train(motivation~.,data=df_m, method='rf', trControl=ctrl_RF3, metric="ROC")

prediction_RF3<-predict(model_RF3,validation_Test)
RF_table3<-confusionMatrix(data=prediction_RF3,reference=validation_TestTarget,mode="everything",positive="yuksek")
RF_table3


print(model_RF3)
print(model_RF3$results)
print(model_RF3$resample)
### clipboarda kopyalar
write_clip(model_RF3$results)

## ROC 10 ##

set.seed(905)
ctrl_RF4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(906)
model_RF4<-train(motivation~.,data=df_m, method='rf', trControl=ctrl_RF4, metric="ROC")

prediction_RF4<-predict(model_RF4,validation_Test)
RF_table4<-confusionMatrix(data=prediction_RF4,reference=validation_TestTarget,mode="everything",positive="yuksek")
RF_table4


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
model_LR1<-train(motivation~.,data=df_m, method='glm', trControl=ctrl_LR1)

prediction_LR1<-predict(model_LR1,validation_Test)
LR_table1<-confusionMatrix(data=prediction_LR1,reference=validation_TestTarget,mode="everything",positive="yuksek")
LR_table1




print(model_LR1)
print(model_LR1$results)
print(model_LR1$resample)
write_clip(model_LR1$results)
#model_svm1$finalModel yerine sadece model_svm1 kullanmalı

## Accuracy 10 ##
set.seed(909)
ctrl_LR2 <- trainControl(method = "cv", number = 10)
set.seed(910)
model_LR2<-train(motivation~.,data=df_m, method='glm', trControl=ctrl_LR2)

prediction_LR2<-predict(model_LR2,validation_Test)
LR_table2<-confusionMatrix(data=prediction_LR2,reference=validation_TestTarget,mode="everything",positive="yuksek")
LR_table2


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
model_LR3<-train(motivation~.,data=df_m, method='glm', trControl=ctrl_LR3, metric="ROC")

prediction_LR3<-predict(model_LR3,validation_Test)
LR_table3<-confusionMatrix(data=prediction_LR3,reference=validation_TestTarget,mode="everything",positive="yuksek")
LR_table3

print(model_LR3)
print(model_LR3$results)
print(model_LR3$resample)
### clipboarda kopyalar
write_clip(model_LR3$results)

## ROC 10 ##

set.seed(913)
ctrl_LR4 <- trainControl(method = "cv", number = 10, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(914)
model_LR4<-train(motivation~.,data=df_m, method='glm', trControl=ctrl_LR4, metric="ROC")

prediction_LR4<-predict(model_LR4,validation_Test)
LR_table4<-confusionMatrix(data=prediction_LR4,reference=validation_TestTarget,mode="everything",positive="yuksek")
LR_table4

print(model_LR4)
print(model_LR4$results)
print(model_LR4$resample)
### clipboarda kopyalar
write_clip(model_LR4$results)



#### PRINTING #############
#table1#
cat("ACC ile K-FOLD 5 ACCURACY :",
    ", KNN : ",knn_table1$overall[1],
    ",  NB:" ,nb_table1$overall[1],
    ",  SVM:",svm_table1$overall[1]
    ,", C5.0:",c50_table1$overall[1],
    ", CART:",CART_table1$overall[1],
    ", Random For:",RF_table1$overall[1],
    ", Log.Regrs:",LR_table1$overall[1],
    "\nSensitivity:    ",
    ",KNN:",knn_table1$byClass[1],
    ",  NB:" ,nb_table1$byClass[1],
    ",  SVM:",svm_table1$byClass[1]
    ,", C5.0:",c50_table1$byClass[1],
    ", CART:",CART_table1$byClass[1],
    ", Random For:",RF_table1$byClass[1],
    ", Log.Regrs:",LR_table1$byClass[1],
    "\nSpecifity:    ",
    ",KNN:",knn_table1$byClass[2],
    ",  NB:" ,nb_table1$byClass[2],
    ",  SVM:",svm_table1$byClass[2]
    ,", C5.0:",c50_table1$byClass[2],
    ", CART:",CART_table1$byClass[2],
    ", Random For:",RF_table1$byClass[2],
    ", Log.Regrs:",LR_table1$byClass[2],
    "\nPrecision:    ",
    ",KNN:",knn_table1$byClass[5],
    ",  NB:" ,nb_table1$byClass[5],
    ",  SVM:",svm_table1$byClass[5]
    ,", C5.0:",c50_table1$byClass[5],
    ", CART:",CART_table1$byClass[5],
    ", Random For:",RF_table1$byClass[5],
    ", Log.Regrs:",LR_table1$byClass[5],
    "\nF-1 Score:    ",
    ",KNN:",knn_table1$byClass[7],
    ",  NB:" ,nb_table1$byClass[7],
    ",  SVM:",svm_table1$byClass[7]
    ,", C5.0:",c50_table1$byClass[7],
    ", CART:",CART_table1$byClass[7],
    ", Random For:",RF_table1$byClass[7],
    ", Log.Regrs:",LR_table1$byClass[7])


#table 2 #
cat("ACC ile K-FOLD 10 ACCURACY :",
    ", KNN : ",knn_table2$overall[1],
    ",  NB:" ,nb_table2$overall[1],
    ",  SVM:",svm_table2$overall[1]
    ,", C5.0:",c50_table2$overall[1],
    ", CART:",CART_table2$overall[1],
    ", Random For:",RF_table2$overall[1],
    ", Log.Regrs:",LR_table2$overall[1],
    "\nSensitivity:    ",
    ",KNN:",knn_table2$byClass[1],
    ",  NB:" ,nb_table2$byClass[1],
    ",  SVM:",svm_table2$byClass[1]
    ,", C5.0:",c50_table2$byClass[1],
    ", CART:",CART_table2$byClass[1],
    ", Random For:",RF_table2$byClass[1],
    ", Log.Regrs:",LR_table2$byClass[1],
    "\nSpecifity:    ",
    ",KNN:",knn_table2$byClass[2],
    ",  NB:" ,nb_table2$byClass[2],
    ",  SVM:",svm_table2$byClass[2]
    ,", C5.0:",c50_table2$byClass[2],
    ", CART:",CART_table2$byClass[2],
    ", Random For:",RF_table2$byClass[2],
    ", Log.Regrs:",LR_table2$byClass[2],
    "\nPrecision:    ",
    ",KNN:",knn_table2$byClass[5],
    ",  NB:" ,nb_table2$byClass[5],
    ",  SVM:",svm_table2$byClass[5]
    ,", C5.0:",c50_table2$byClass[5],
    ", CART:",CART_table2$byClass[5],
    ", Random For:",RF_table2$byClass[5],
    ", Log.Regrs:",LR_table2$byClass[5],
    "\nF-1 Score:    ",
    ",KNN:",knn_table2$byClass[7],
    ",  NB:" ,nb_table2$byClass[7],
    ",  SVM:",svm_table2$byClass[7]
    ,", C5.0:",c50_table2$byClass[7],
    ", CART:",CART_table2$byClass[7],
    ", Random For:",RF_table2$byClass[7],
    ", Log.Regrs:",LR_table2$byClass[7])


#table3#
cat("ROC ile K-FOLD 5 ACCURACY :",
    ", KNN : ",knn_table3$overall[1],
    ",  NB:" ,nb_table3$overall[1],
    ",  SVM:",svm_table3$overall[1]
    ,", C5.0:",c50_table3$overall[1],
    ", CART:",CART_table3$overall[1],
    ", Random For:",RF_table3$overall[1],
    ", Log.Regrs:",LR_table3$overall[1],
    "\nSensitivity:    ",
    ",KNN:",knn_table3$byClass[1],
    ",  NB:" ,nb_table3$byClass[1],
    ",  SVM:",svm_table3$byClass[1]
    ,", C5.0:",c50_table3$byClass[1],
    ", CART:",CART_table3$byClass[1],
    ", Random For:",RF_table3$byClass[1],
    ", Log.Regrs:",LR_table3$byClass[1],
    "\nSpecifity:    ",
    ",KNN:",knn_table3$byClass[2],
    ",  NB:" ,nb_table3$byClass[2],
    ",  SVM:",svm_table3$byClass[2]
    ,", C5.0:",c50_table3$byClass[2],
    ", CART:",CART_table3$byClass[2],
    ", Random For:",RF_table3$byClass[2],
    ", Log.Regrs:",LR_table3$byClass[2],
    "\nPrecision:    ",
    ",KNN:",knn_table3$byClass[5],
    ",  NB:" ,nb_table3$byClass[5],
    ",  SVM:",svm_table3$byClass[5]
    ,", C5.0:",c50_table3$byClass[5],
    ", CART:",CART_table3$byClass[5],
    ", Random For:",RF_table3$byClass[5],
    ", Log.Regrs:",LR_table3$byClass[5],
    "\nF-1 Score:    ",
    ",KNN:",knn_table3$byClass[7],
    ",  NB:" ,nb_table3$byClass[7],
    ",  SVM:",svm_table3$byClass[7]
    ,", C5.0:",c50_table3$byClass[7],
    ", CART:",CART_table3$byClass[7],
    ", Random For:",RF_table3$byClass[7],
    ", Log.Regrs:",LR_table3$byClass[7])


cat("ROC ile K-FOLD 10 ACCURACY :",
    ", KNN : ",knn_table4$overall[1],
    ",  NB:" ,nb_table4$overall[1],
    ",  SVM:",svm_table4$overall[1]
    ,", C5.0:",c50_table4$overall[1],
    ", CART:",CART_table4$overall[1],
    ", Random For:",RF_table4$overall[1],
    ", Log.Regrs:",LR_table4$overall[1],
    "\nSensitivity:    ",
    ",KNN:",knn_table4$byClass[1],
    ",  NB:" ,nb_table4$byClass[1],
    ",  SVM:",svm_table4$byClass[1]
    ,", C5.0:",c50_table4$byClass[1],
    ", CART:",CART_table4$byClass[1],
    ", Random For:",RF_table4$byClass[1],
    ", Log.Regrs:",LR_table4$byClass[1],
    "\nSpecifity:    ",
    ",KNN:",knn_table4$byClass[2],
    ",  NB:" ,nb_table4$byClass[2],
    ",  SVM:",svm_table4$byClass[2]
    ,", C5.0:",c50_table4$byClass[2],
    ", CART:",CART_table4$byClass[2],
    ", Random For:",RF_table4$byClass[2],
    ", Log.Regrs:",LR_table4$byClass[2],
    "\nPrecision:    ",
    ",KNN:",knn_table4$byClass[5],
    ",  NB:" ,nb_table4$byClass[5],
    ",  SVM:",svm_table4$byClass[5]
    ,", C5.0:",c50_table4$byClass[5],
    ", CART:",CART_table4$byClass[5],
    ", Random For:",RF_table4$byClass[5],
    ", Log.Regrs:",LR_table4$byClass[5],
    "\nF-1 Score:    ",
    ",KNN:",knn_table4$byClass[7],
    ",  NB:" ,nb_table4$byClass[7],
    ",  SVM:",svm_table4$byClass[7]
    ,", C5.0:",c50_table4$byClass[7],
    ", CART:",CART_table4$byClass[7],
    ", Random For:",RF_table4$byClass[7],
    ", Log.Regrs:",LR_table4$byClass[7])




#############################################
############################################################
################################






set.seed(873)
ctrl_nb3 <- trainControl(method = "cv", number = 5, classProbs=TRUE, summaryFunction = twoClassSummary)
set.seed(874)
model_nb3<-train(motivation~.,data=df_m, method='naive_bayes', trControl=ctrl_nb3, metric="ROC")
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

