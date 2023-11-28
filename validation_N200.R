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

library(clusterSim)


df_m[,-ncol(df_m)]<-data.Normalization(df_m[,-ncol(df_m)],type="n4",normalization ="column")
df_m$motivation<-as.factor(df_m$motivation)
levels(df_m$motivation)
table(df_m$motivation)

df_v[,-ncol(df_v)]<-data.Normalization(df_v[,-ncol(df_v)],type="n4",normalization ="column")
df_v$motivation<-as.factor(df_v$motivation)
levels(df_v$motivation)
table(df_v$motivation)



############ RANDOM SUBSaMPLING for validation set#################
SVMmodel_dizimiz<-vector()
seedim<-711
require(class)
library(e1071)
library(C50)
library(rpart)
library(rpart.plot)
require(caret)
require(class)
help(createDataPartition)
##Acc
dizimiz<-vector()
NBdizimiz<-vector()
SVMdizimiz<-vector()
CARTdizimiz<-vector()
C50dizimiz<-vector()
RFdizimiz<-vector()
LRdizimiz<-vector()
##Sens
dizimizSens<-vector()
NBdizimizSens<-vector()
SVMdizimizSens<-vector()
CARTdizimizSens<-vector()
C50dizimizSens<-vector()
RFdizimizSens<-vector()
LRdizimizSens<-vector()
##Spec
dizimizSpec<-vector()
NBdizimizSpec<-vector()
SVMdizimizSpec<-vector()
CARTdizimizSpec<-vector()
C50dizimizSpec<-vector()
RFdizimizSpec<-vector()
LRdizimizSpec<-vector()
##Prec
dizimizPrec<-vector()
NBdizimizPrec<-vector()
SVMdizimizPrec<-vector()
CARTdizimizPrec<-vector()
C50dizimizPrec<-vector()
RFdizimizPrec<-vector()
LRdizimizPrec<-vector()
##F-1
dizimizF1<-vector()
NBdizimizF1<-vector()
SVMdizimizF1<-vector()
CARTdizimizF1<-vector()
C50dizimizF1<-vector()
RFdizimizF1<-vector()
LRdizimizF1<-vector()

library(caret)
library(randomForest)

for (i in 1:200){
  set.seed(seedim+i)
  
  trainingIndexes<-createDataPartition(y=df_m$motivation,p=.70,list=FALSE)
  #print(trainingIndexes[1:20])
  
  train_data<-df_m[trainingIndexes,]
  test_data<-df_m[-trainingIndexes,]
  
  train_Attributes<-train_data[,-ncol(train_data)]
  train_Target<-train_data[[ncol(train_data)]]
  
  test_Attributes<-test_data[,-ncol(test_data)]
  test_Target<-test_data[[ncol(test_data)]]
  
  #### düz tüm veriyle eğitim ###
  train_attributes_TUM<-df_m[,-ncol(df_m)]
  train_Target_TUM<-df_m[[ncol(df_m)]]
  
  ######## validation için ayırma #########
  validation_Test<-df_v[,-ncol(df_v)]
  validation_TestTarget<-df_v[[ncol(df_v)]]
  colnames(validation_Test)<-colnames(train_attributes_TUM)
  #### KNN_Random##
  #predictions<-knn(train = train_Attributes,test = test_Attributes,cl = train_Target,k = 3)
  
  predictions<-knn(train= train_attributes_TUM, test=validation_Test,cl=train_Target_TUM,k=9)
  
  #tablomuz <- confusionMatrix(data = predictions, reference = test_Target, mode = "everything",positive="1")
  tablomuz<-confusionMatrix(data=predictions,reference=validation_TestTarget,mode="everything", positive="1")
  
  dizimiz[i]<-tablomuz$overall[1]
  dizimizSens[i]<-tablomuz$byClass[1]
  dizimizSpec[i]<-tablomuz$byClass[2]
  dizimizPrec[i]<-tablomuz$byClass[5]
  dizimizF1[i]<-tablomuz$byClass[7]
  
  ##NAIVE BAYES_Random##
  NBmodelimiz<-naiveBayes(motivation ~.,data=df_m)
  #NBtahminimiz<-predict(NBmodelimiz, test_Attributes)
  NBtahminimiz<-predict(NBmodelimiz, validation_Test)
  NBtablomuz <- confusionMatrix(data = NBtahminimiz, reference = validation_TestTarget, mode = "everything",positive="1")
  
  NBdizimiz[i]<-NBtablomuz$overall[1]
  NBdizimizSens[i]<-NBtablomuz$byClass[1]
  NBdizimizSpec[i]<-NBtablomuz$byClass[2]
  NBdizimizPrec[i]<-NBtablomuz$byClass[5]
  NBdizimizF1[i]<-NBtablomuz$byClass[7]
  
  ## SVM_Random ##
  
  SVMmodelimiz<- svm(formula= motivation ~.,data=df_m,type="C-classification",kernel="linear")
  SVMtahminimiz<-predict(SVMmodelimiz, validation_Test)
  SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=validation_TestTarget,mode="everything",positive = "1")
  
  mdl2<-train(motivation~.,data=train_data,method="svmLinear")
  SVMmodel_dizimiz[i]<-mdl2
  
  SVMdizimiz[i]<-SVMtablomuz$overall[1]
  SVMdizimizSens[i]<-SVMtablomuz$byClass[1]
  SVMdizimizSpec[i]<-SVMtablomuz$byClass[2]
  SVMdizimizPrec[i]<-SVMtablomuz$byClass[5]
  SVMdizimizF1[i]<-SVMtablomuz$byClass[7]
  
  ### C5.0 _RANDOM###
  
  C50modelimiz<-C5.0(train_attributes_TUM, train_Target_TUM)
  C50tahminimiz<-predict(C50modelimiz,validation_Test)
  C50tablomuz<-confusionMatrix(data=C50tahminimiz, reference=validation_TestTarget,mode="everything", positive="1")
  
  C50dizimiz[i]<-C50tablomuz$overall[1]
  C50dizimizSens[i]<-C50tablomuz$byClass[1]
  C50dizimizSpec[i]<-C50tablomuz$byClass[2]
  C50dizimizPrec[i]<-C50tablomuz$byClass[5]
  C50dizimizF1[i]<-C50tablomuz$byClass[7]
  
  
  
  ### CART _Random###
  CARTmodelimiz<- rpart(motivation~., df_m,method="class") 
  #rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
  CARTtahminimiz<-predict(CARTmodelimiz,validation_Test,type="class")
  CARTtablomuz<-confusionMatrix(data=CARTtahminimiz,reference=validation_TestTarget,mode="everything", positive="1")
  
  CARTdizimiz[i]<-CARTtablomuz$overall[1]
  CARTdizimizSens[i]<-CARTtablomuz$byClass[1]
  CARTdizimizSpec[i]<-CARTtablomuz$byClass[2]
  CARTdizimizPrec[i]<-CARTtablomuz$byClass[5]
  CARTdizimizF1[i]<-CARTtablomuz$byClass[7]
  #rpart.plot(CARTmodelimiz,type=4,digits=3,fallen.leaves = TRUE, cex=0.9)
  
  ############## RANDOM FOREST_Random #########
  
  
  RFmodelimiz<- randomForest(motivation~.,data=df_m,proximity=TRUE)
  RFtahminimiz<-predict(RFmodelimiz,validation_Test,type="class")
  RFtablomuz<-confusionMatrix(data=RFtahminimiz,reference=validation_TestTarget,mode="everything",positive="1")
  
  RFdizimiz[i]<-RFtablomuz$overall[1]
  RFdizimizSens[i]<-RFtablomuz$byClass[1]
  RFdizimizSpec[i]<-RFtablomuz$byClass[2]
  RFdizimizPrec[i]<-RFtablomuz$byClass[5]
  RFdizimizF1[i]<-RFtablomuz$byClass[7]
  
  
  ####### Logistic Regression for Binary_Random ######
  LRmodelimiz<- glm(motivation~.,family= "binomial", data=df_m)
  
  LRtahminimiz<-predict(LRmodelimiz,validation_Test,type="response")
  #predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
  predicted <- as.factor(round(LRtahminimiz))
  
  LRtablomuz<-confusionMatrix(data=predicted, reference=validation_TestTarget,mode="everything",positive="1")
  
  
  LRdizimiz[i]<-LRtablomuz$overall[1]
  LRdizimizSens[i]<-LRtablomuz$byClass[1]
  LRdizimizSpec[i]<-LRtablomuz$byClass[2]
  LRdizimizPrec[i]<-LRtablomuz$byClass[5]
  LRdizimizF1[i]<-LRtablomuz$byClass[7]
  
}

print(paste0("RANDOM SS ACCURACY:    ","KNN:",mean(dizimiz),"  NB:" ,mean(NBdizimiz),"  SVM:",mean(SVMdizimiz)
             ," C5.0:",mean(C50dizimiz),
             " CART:",mean(CARTdizimiz),
             " Random For:",mean(RFdizimiz),
             " Log.Regrs:",mean(LRdizimiz)))

print(paste0("RANDOM SS SENSITIVITY:    ","KNN:",mean(dizimizSens),"  NB:" ,mean(NBdizimizSens),"  SVM:",mean(SVMdizimizSens)
             ," C5.0:",mean(C50dizimizSens),
             " CART:",mean(CARTdizimizSens),
             " Random For:",mean(RFdizimizSens),
             " Log.Regrs:",mean(LRdizimizSens)))

print(paste0("RANDOM SS SPECIFITY:    ","KNN:",mean(dizimizSpec),"  NB:" ,mean(NBdizimizSpec),"  SVM:",mean(SVMdizimizSpec)
             ," C5.0:",mean(C50dizimizSpec),
             " CART:",mean(CARTdizimizSpec),
             " Random For:",mean(RFdizimizSpec),
             " Log.Regrs:",mean(LRdizimizSpec)))

print(paste0("RANDOM SS PRECISION:    ","KNN:",mean(dizimizPrec),"  NB:" ,mean(NBdizimizPrec),"  SVM:",mean(SVMdizimizPrec)
             ," C5.0:",mean(C50dizimizPrec),
             " CART:",mean(CARTdizimizPrec),
             " Random For:",mean(RFdizimizPrec),
             " Log.Regrs:",mean(LRdizimizPrec)))

print(paste0("RANDOM SS F-1:    ","KNN:",mean(dizimizF1),"  NB:" ,mean(NBdizimizF1),"  SVM:",mean(SVMdizimizF1)
             ," C5.0:",mean(C50dizimizF1),
             " CART:",mean(CARTdizimizF1),
             "Random For:",mean(RFdizimizF1),
             "Log.Regrs:",mean(LRdizimizF1)))


#######################
cat("RANDOM SS ACCURACY:    ",",KNN:",mean(dizimiz),",  NB:" ,mean(NBdizimiz),",  SVM:",mean(SVMdizimiz)
    ,", C5.0:",mean(C50dizimiz),
    ", CART:",mean(CARTdizimiz),
    " ,Random For:",mean(RFdizimiz),
    " ,Log.Regrs:",mean(LRdizimiz),"\nRANDOM SS SENSITIVITY:    ",",KNN:",mean(dizimizSens),",  NB:" ,mean(NBdizimizSens),",  SVM:",mean(SVMdizimizSens)
    ,", C5.0:",mean(C50dizimizSens),
    ", CART:",mean(CARTdizimizSens),
    ", Random For:",mean(RFdizimizSens),
    ", Log.Regrs:",mean(LRdizimizSens),"\nRANDOM SS SPECIFITY:    ",",KNN:",mean(dizimizSpec),",  NB:" ,mean(NBdizimizSpec),",  SVM:",mean(SVMdizimizSpec)
    ,", C5.0:",mean(C50dizimizSpec),
    ", CART:",mean(CARTdizimizSpec),
    ", Random For:",mean(RFdizimizSpec),
    ",Log.Regrs:",mean(LRdizimizSpec),"\nRANDOM SS PRECISION:    ",",KNN:",mean(dizimizPrec),",  NB:" ,mean(NBdizimizPrec),",  SVM:",mean(SVMdizimizPrec)
    ,", C5.0:",mean(C50dizimizPrec),
    ", CART:",mean(CARTdizimizPrec),
    ", Random For:",mean(RFdizimizPrec),
    ", Log.Regrs:",mean(LRdizimizPrec),"\nRANDOM SS F-1:    ",",KNN:",mean(dizimizF1),",  NB:" ,mean(NBdizimizF1),",  SVM:",mean(SVMdizimizF1)
    ,", C5.0:",mean(C50dizimizF1),
    ", CART:",mean(CARTdizimizF1),
    ",Random For:",mean(RFdizimizF1),
    ",Log.Regrs:",mean(LRdizimizF1))

###################################################
############### OVERSAMPLING #######################
###################################################

require(ROSE)
#install.packages("ROSE")
library(ROSE)

set.seed(400)
Ogrenciler_Over<-ovun.sample(motivation~.,data=df_m,method="over",N=286)$data
#Ogrenciler_Over<-ovun.sample(motivation~.,data=df_m,method="both",N=400,p = 0.3)$data
table(Ogrenciler_Over$motivation)
summary(Ogrenciler_Over)

##### Over_ korelasyon ####
df2<-Ogrenciler_Over
df2$motivation<-as.numeric(df2$motivation)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.4)
#Zaten df_v'de sınav notu yok
#Ogrenciler_Over<- Ogrenciler_Over[,c(-15)]



### over TUM bölünmesi ##
#### düz tüm veriyle eğitim ###
train_attributes_Over_TUM<-Ogrenciler_Over[,-ncol(Ogrenciler_Over)]
train_Target_Over_TUM<-Ogrenciler_Over[[ncol(Ogrenciler_Over)]]

######## validation için ayırma #########
validation_Test_Over<-df_v[,-ncol(df_v)]
validation_TestTarget_Over<-df_v[[ncol(df_v)]]
#colnames(validation_Test_Over)<-colnames(train_attributes_Over_TUM)

### Knn_Over###
set.seed(31)
tahmin_Over<-knn(train_attributes_Over_TUM,validation_Test_Over,train_Target_Over_TUM,k=9)
summary(tahmin_Over)
tablo2 <- confusionMatrix(data = tahmin_Over, reference = validation_TestTarget_Over, mode = "everything",positive="1")
tablo2

accuracy_over<-tablo2$overall[1]

## NB_Over ##

set.seed(971)
NBmodelimiz_Over<-naiveBayes(motivation ~.,data=Ogrenciler_Over)
NBtahminimiz_Over<-predict(NBmodelimiz_Over, validation_Test_Over)
NBtablomuz_Over <- confusionMatrix(data = NBtahminimiz_Over, reference = validation_TestTarget_Over, mode = "everything",positive="1")
NBtablomuz_Over

NBtablomuz_Over$overall[1]
NBtablomuz_Over$byClass[1]
NBtablomuz_Over$byClass[2]
NBtablomuz_Over$byClass[5]
NBtablomuz_Over$byClass[7]

## SVM_Over ##
set.seed(765)
SVMmodelimiz_Over<- svm(formula= motivation ~.,data=Ogrenciler_Over,type="C-classification",kernel="linear")
SVMtahminimiz_Over<-predict(SVMmodelimiz_Over, validation_Test_Over)
SVMtablomuz_Over<-confusionMatrix(data=SVMtahminimiz_Over,reference=validation_TestTarget_Over,mode="everything",positive = "1")

SVMtablomuz_Over

SVMtablomuz_Over$overall[1]
SVMtablomuz_Over$byClass[1]
SVMtablomuz_Over$byClass[2]
SVMtablomuz_Over$byClass[5]
SVMtablomuz_Over$byClass[7]

### C5.0_Over###

set.seed(349)
C50modelimiz_Over<-C5.0(train_attributes_Over_TUM, train_Target_Over_TUM)
C50tahminimiz_Over<-predict(C50modelimiz_Over,validation_Test_Over)
C50tablomuz_Over<-confusionMatrix(data=C50tahminimiz_Over, reference=validation_TestTarget_Over,mode="everything", positive="1")
C50tablomuz_Over
C50tablomuz_Over$overall[1]
C50tablomuz_Over$byClass[1]
C50tablomuz_Over$byClass[2]
C50tablomuz_Over$byClass[5]
C50tablomuz_Over$byClass[7]

### CART _OVer###
set.seed(1990)
CARTmodelimiz_Over<- rpart(motivation~., Ogrenciler_Over,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_Over<-predict(CARTmodelimiz_Over,validation_Test_Over,type="class")
CARTtablomuz_Over<-confusionMatrix(data=CARTtahminimiz_Over,reference=validation_TestTarget_Over,mode="everything", positive="1")
CARTtablomuz_Over
CARTtablomuz_Over$overall[1]
CARTtablomuz_Over$byClass[1]
CARTtablomuz_Over$byClass[2]
CARTtablomuz_Over$byClass[5]
CARTtablomuz_Over$byClass[7]

############## RANDOM FOREST #########
library(randomForest)
set.seed(891)
RFmodelimiz_Over<- randomForest(motivation~.,data=Ogrenciler_Over,proximity=TRUE)
RFtahminimiz_Over<-predict(RFmodelimiz_Over,validation_Test_Over,type="class")
RFtablomuz_Over<-confusionMatrix(data=RFtahminimiz_Over,reference=validation_TestTarget_Over,mode="everything", positive="1")
RFtablomuz_Over


####### Logistic Regression for Binary ######
set.seed(724)
LRmodelimiz_Over<- glm(motivation~.,family= "binomial", data=Ogrenciler_Over)

LRtahminimiz_Over<-predict(LRmodelimiz_Over,validation_Test_Over,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_Over))

LRtablomuz_Over<-confusionMatrix(data=predicted, reference=validation_TestTarget_Over,mode="everything",positive="1")

LRtablomuz_Over

summary(LRmodelimiz_Over)


# PRINTING OVER #

print(paste0("OVERSAMPLING ACCURACY:    ","KNN:",tablo2$overall[1],"  NB:" ,NBtablomuz_Over$overall[1],
             "  SVM:",SVMtablomuz_Over$overall[1]
             ," C5.0:",C50tablomuz_Over$overall[1],
             " CART:",CARTtablomuz_Over$overall[1],
             " Random For:",RFtablomuz_Over$overall[1],
             " Log.Regrs:",LRtablomuz_Over$overall[1]))

print(paste0("OVERSAMPLING Sensitivity:    ","KNN:",tablo2$byClass[1],"  NB:" ,NBtablomuz_Over$byClass[1],
             "  SVM:",SVMtablomuz_Over$byClass[1]
             ," C5.0:",C50tablomuz_Over$byClass[1],
             " CART:",CARTtablomuz_Over$byClass[1],
             " Random For:",RFtablomuz_Over$byClass[1],
             " Log.Regrs:",LRtablomuz_Over$byClass[1]))

print(paste0("OVERSAMPLING Specifity:    ","KNN:",tablo2$byClass[2],"  NB:" ,NBtablomuz_Over$byClass[2],
             "  SVM:",SVMtablomuz_Over$byClass[2]
             ," C5.0:",C50tablomuz_Over$byClass[2],
             " CART:",CARTtablomuz_Over$byClass[2],
             " Random For:",RFtablomuz_Over$byClass[2],
             " Log.Regrs:",LRtablomuz_Over$byClass[2]))

print(paste0("OVERSAMPLING Precision:    ","KNN:",tablo2$byClass[5],"  NB:" ,NBtablomuz_Over$byClass[5],
             "  SVM:",SVMtablomuz_Over$byClass[5]
             ," C5.0:",C50tablomuz_Over$byClass[5],
             " CART:",CARTtablomuz_Over$byClass[5],
             " Random For:",RFtablomuz_Over$byClass[5],
             " Log.Regrs:",LRtablomuz_Over$byClass[5]))

print(paste0("OVERSAMPLING F-1 Score:    ","KNN:",tablo2$byClass[7],"  NB:" ,NBtablomuz_Over$byClass[7],
             "  SVM:",SVMtablomuz_Over$byClass[7]
             ," C5.0:",C50tablomuz_Over$byClass[7],
             " CART:",CARTtablomuz_Over$byClass[7],
             " Random For:",RFtablomuz_Over$byClass[7],
             " Log.Regrs:",LRtablomuz_Over$byClass[7]))

##########################

cat("OVERSAMPLING ACCURACY:    ",",KNN:",tablo2$overall[1],",  NB:" ,NBtablomuz_Over$overall[1],
    ",  SVM:",SVMtablomuz_Over$overall[1]
    ,", C5.0:",C50tablomuz_Over$overall[1],
    ", CART:",CARTtablomuz_Over$overall[1],
    ", Random For:",RFtablomuz_Over$overall[1],
    ", Log.Regrs:",LRtablomuz_Over$overall[1],"\nOVERSAMPLING Sensitivity:    ",",KNN:",tablo2$byClass[1],",  NB:" ,NBtablomuz_Over$byClass[1],
    ",  SVM:",SVMtablomuz_Over$byClass[1]
    ,", C5.0:",C50tablomuz_Over$byClass[1],
    ", CART:",CARTtablomuz_Over$byClass[1],
    ", Random For:",RFtablomuz_Over$byClass[1],
    ", Log.Regrs:",LRtablomuz_Over$byClass[1],"\nOVERSAMPLING Specifity:    ",",KNN:",tablo2$byClass[2],",  NB:" ,NBtablomuz_Over$byClass[2],
    ", SVM:",SVMtablomuz_Over$byClass[2]
    ,", C5.0:",C50tablomuz_Over$byClass[2],
    ", CART:",CARTtablomuz_Over$byClass[2],
    ", Random For:",RFtablomuz_Over$byClass[2],
    ", Log.Regrs:",LRtablomuz_Over$byClass[2],"\nOVERSAMPLING Precision:    ",",KNN:",tablo2$byClass[5],",  NB:" ,NBtablomuz_Over$byClass[5],
    ",  SVM:",SVMtablomuz_Over$byClass[5]
    ,", C5.0:",C50tablomuz_Over$byClass[5],
    ", CART:",CARTtablomuz_Over$byClass[5],
    ", Random For:",RFtablomuz_Over$byClass[5],
    ", Log.Regrs:",LRtablomuz_Over$byClass[5],"\nOVERSAMPLING F-1 Score:    ",",KNN:",tablo2$byClass[7],",  NB:" ,NBtablomuz_Over$byClass[7],
    ", SVM:",SVMtablomuz_Over$byClass[7]
    ,", C5.0:",C50tablomuz_Over$byClass[7],
    ", CART:",CARTtablomuz_Over$byClass[7],
    ", Random For:",RFtablomuz_Over$byClass[7],
    ", Log.Regrs:",LRtablomuz_Over$byClass[7])








###############################################
################ S     M     O     T     E ##################
#############################################

#library(performanceEstimation)
#help(performanceEstimation)
#help(smote)

#Ogrenciler_SMOTE <- smote(motivation ~ ., df_m,perc.over=5, perc.under=1.2,k = 5)
#Ogrenciler_SMOTE <- smote(motivation ~ ., df_m,perc.over=5)
#df_V_SMOTE<-themis::smote(df=df_v, var="motivation",k=5,over_ratio=8)
#table(df_V_SMOTE$motivation)
#df_v<-df_V_SMOTE
####### 3.4 için
set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df_m,var = "motivation",k = 5,over_ratio = 1.065)
table(Ogrenciler_SMOTE$motivation)



###### 3.5 için

set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df_m,var = "motivation",k = 5,over_ratio = 1.11)
table(Ogrenciler_SMOTE$motivation)


###### 3.6 için

set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df_m,var = "motivation",k = 5,over_ratio = 1.15)
table(Ogrenciler_SMOTE$motivation)

###### 4.0 için **************

set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df_m,var = "motivation",k = 5,over_ratio = 1)
table(Ogrenciler_SMOTE$motivation)



######## SMOTE Korelasyon ######

df2<-Ogrenciler_SMOTE
df2$motivation<-as.numeric(df2$motivation)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.57)

#Ogrenciler_Over<- Ogrenciler_Over[,c(-15)]


#hist(as.numeric(as.character(Ogrenciler_SMOTE$motivation)),labels=TRUE, main='I. SET "Motivation" Özniteliği (N=400)',xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
#axis(side=1, at=c(1, 0))


### over TUM bölünmesi ##
#### düz tüm veriyle eğitim ###
train_attributes_SMOTE_TUM<-Ogrenciler_SMOTE[,-ncol(Ogrenciler_SMOTE)]
train_Target_SMOTE_TUM<-Ogrenciler_SMOTE[[ncol(Ogrenciler_SMOTE)]]

######## validation için ayırma #########
validation_Test_SMOTE<-df_v[,-ncol(df_v)]
validation_TestTarget_SMOTE<-df_v[[ncol(df_v)]]
colnames(validation_Test_SMOTE)<-colnames(train_attributes_SMOTE_TUM)


### Knn_SMOTE###
set.seed(75)
tahmin_SMOTE<-knn(train_attributes_SMOTE_TUM,validation_Test_SMOTE,train_Target_SMOTE_TUM,k=9)
#tahmin_SMOTE <- relevel(tahmin_SMOTE, "1")
summary(tahmin_SMOTE)
knnTablo_SMOTE <- confusionMatrix(data = tahmin_SMOTE, reference = validation_TestTarget_SMOTE, mode = "everything",positive="1")
knnTablo_SMOTE

knnTablo_SMOTE$overall[1]
knnTablo_SMOTE$byClass[1]
knnTablo_SMOTE$byClass[2]
knnTablo_SMOTE$byClass[5]
knnTablo_SMOTE$byClass[7]


## NB_SMOTE ##

set.seed(78)
NBmodelimiz_SMOTE<-naiveBayes(motivation ~.,data=Ogrenciler_SMOTE)
NBtahminimiz_SMOTE<-predict(NBmodelimiz_SMOTE, validation_Test_SMOTE)
NBtablomuz_SMOTE <- confusionMatrix(data = NBtahminimiz_SMOTE, reference = validation_TestTarget_SMOTE, mode = "everything",positive="1")
NBtablomuz_SMOTE

NBtablomuz_SMOTE$overall[1]
NBtablomuz_SMOTE$byClass[1]
NBtablomuz_SMOTE$byClass[2]
NBtablomuz_SMOTE$byClass[5]
NBtablomuz_SMOTE$byClass[7]


## SVM_SMOTE ##
set.seed(769)
SVMmodelimiz_SMOTE<- svm(formula= motivation ~.,data=Ogrenciler_SMOTE,type="C-classification",kernel="linear")
SVMtahminimiz_SMOTE<-predict(SVMmodelimiz_SMOTE, validation_Test_SMOTE)
SVMtablomuz_SMOTE<-confusionMatrix(data=SVMtahminimiz_SMOTE,reference=validation_TestTarget_SMOTE,mode="everything",positive = "1")
SVMtablomuz_SMOTE
SVMtablomuz_SMOTE$overall[1]
SVMtablomuz_SMOTE$byClass[1]
SVMtablomuz_SMOTE$byClass[2]
SVMtablomuz_SMOTE$byClass[5]
SVMtablomuz_SMOTE$byClass[7]

### C5.0_SMOTE###

set.seed(353)
C50modelimiz_SMOTE<-C5.0(train_attributes_SMOTE_TUM, train_Target_SMOTE_TUM)
C50tahminimiz_SMOTE<-predict(C50modelimiz_SMOTE,validation_Test_SMOTE)
C50tablomuz_SMOTE<-confusionMatrix(data=C50tahminimiz_SMOTE, reference=validation_TestTarget_SMOTE,mode="everything", positive="1")
C50tablomuz_SMOTE
C50tablomuz_SMOTE$overall[1]
C50tablomuz_SMOTE$byClass[1]
C50tablomuz_SMOTE$byClass[2]
C50tablomuz_SMOTE$byClass[5]
C50tablomuz_SMOTE$byClass[7]
#plot(C50modelimiz_SMOTE, cex.lab =0.001)
#prp(C50modelimiz_SMOTE)
summary(C50modelimiz_SMOTE)

### CART _SMOTE###
set.seed(838)
CARTmodelimiz_SMOTE<- rpart(motivation~., Ogrenciler_SMOTE,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_SMOTE<-predict(CARTmodelimiz_SMOTE,validation_Test_SMOTE,type="class")
CARTtablomuz_SMOTE<-confusionMatrix(data=CARTtahminimiz_SMOTE,reference=validation_TestTarget_SMOTE,mode="everything", positive="1")
CARTtablomuz_SMOTE
rpart.plot(CARTmodelimiz_SMOTE,type=3,fallen.leaves = TRUE,cex=0.9)

CARTtablomuz_SMOTE$overall[1]
CARTtablomuz_SMOTE$byClass[1]
CARTtablomuz_SMOTE$byClass[2]
CARTtablomuz_SMOTE$byClass[5]
CARTtablomuz_SMOTE$byClass[7]

############## RandomForest_SMOTE ###########

library(randomForest)
set.seed(892)
RFmodelimiz_SMOTE<- randomForest(motivation~.,data=Ogrenciler_SMOTE,proximity=TRUE)
RFtahminimiz_SMOTE<-predict(RFmodelimiz_SMOTE,validation_Test_SMOTE,type="class")
RFtablomuz_SMOTE<-confusionMatrix(data=RFtahminimiz_SMOTE,reference=validation_TestTarget_SMOTE,mode="everything",positive="1")
RFtablomuz_SMOTE
RFtablomuz_SMOTE$overall[1]
RFtablomuz_SMOTE$byClass[1]
RFtablomuz_SMOTE$byClass[2]
RFtablomuz_SMOTE$byClass[5]
RFtablomuz_SMOTE$byClass[7]

############## LogisticRegression_SMOTE ###########

set.seed(729)
LRmodelimiz_SMOTE<- glm(motivation~.,family= "binomial", data=Ogrenciler_SMOTE)

LRtahminimiz_SMOTE<-predict(LRmodelimiz_SMOTE,validation_Test_SMOTE,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_SMOTE))

LRtablomuz_SMOTE<-confusionMatrix(data=predicted, reference=validation_TestTarget_SMOTE,mode="everything",positive="1")

LRtablomuz_SMOTE

summary(LRmodelimiz_SMOTE)


# PRINTING SMOTE #

print(paste0("SMOTE ACCURACY:    ","KNN:",knnTablo_SMOTE$overall[1],"  NB:" ,NBtablomuz_SMOTE$overall[1],
             "  SVM:",SVMtablomuz_SMOTE$overall[1]
             ," C5.0:",C50tablomuz_SMOTE$overall[1],
             " CART:",CARTtablomuz_SMOTE$overall[1],
             " Random For:",RFtablomuz_SMOTE$overall[1],
             " Log.Regrs:",LRtablomuz_SMOTE$overall[1]))

print(paste0("SMOTE Sensitivity:    ","KNN:",knnTablo_SMOTE$byClass[1],"  NB:" ,NBtablomuz_SMOTE$byClass[1],
             "  SVM:",SVMtablomuz_SMOTE$byClass[1]
             ," C5.0:",C50tablomuz_SMOTE$byClass[1],
             " CART:",CARTtablomuz_SMOTE$byClass[1],
             " Random For:",RFtablomuz_SMOTE$byClass[1],
             " Log.Regrs:",LRtablomuz_SMOTE$byClass[1]))

print(paste0("SMOTE Specifity:    ","KNN:",knnTablo_SMOTE$byClass[2],"  NB:" ,NBtablomuz_SMOTE$byClass[2],
             "  SVM:",SVMtablomuz_SMOTE$byClass[2]
             ," C5.0:",C50tablomuz_SMOTE$byClass[2],
             " CART:",CARTtablomuz_SMOTE$byClass[2],
             " Random For:",RFtablomuz_SMOTE$byClass[2],
             " Log.Regrs:",LRtablomuz_SMOTE$byClass[2]))

print(paste0("SMOTE Precision:    ","KNN:",knnTablo_SMOTE$byClass[5],"  NB:" ,NBtablomuz_SMOTE$byClass[5],
             "  SVM:",SVMtablomuz_SMOTE$byClass[5]
             ," C5.0:",C50tablomuz_SMOTE$byClass[5],
             " CART:",CARTtablomuz_SMOTE$byClass[5],
             " Random For:",RFtablomuz_SMOTE$byClass[5],
             " Log.Regrs:",LRtablomuz_SMOTE$byClass[5]))

print(paste0("SMOTE F-1 Score:    ","KNN:",knnTablo_SMOTE$byClass[7],"  NB:" ,NBtablomuz_SMOTE$byClass[7],
             "  SVM:",SVMtablomuz_SMOTE$byClass[7]
             ," C5.0:",C50tablomuz_SMOTE$byClass[7],
             " CART:",CARTtablomuz_SMOTE$byClass[7],
             " Random For:",RFtablomuz_SMOTE$byClass[7],
             " Log.Regrs:",LRtablomuz_SMOTE$byClass[7]))

#################################################3
cat("SMOTE ACCURACY:    ",",KNN:",knnTablo_SMOTE$overall[1],",  NB:" ,NBtablomuz_SMOTE$overall[1],
    ",  SVM:",SVMtablomuz_SMOTE$overall[1]
    ,", C5.0:",C50tablomuz_SMOTE$overall[1],
    ", CART:",CARTtablomuz_SMOTE$overall[1],
    ", Random For:",RFtablomuz_SMOTE$overall[1],
    ", Log.Regrs:",LRtablomuz_SMOTE$overall[1],"\nSMOTE Sensitivity:    ",",KNN:",knnTablo_SMOTE$byClass[1],",  NB:" ,NBtablomuz_SMOTE$byClass[1],
    ",  SVM:",SVMtablomuz_SMOTE$byClass[1]
    ,", C5.0:",C50tablomuz_SMOTE$byClass[1],
    ", CART:",CARTtablomuz_SMOTE$byClass[1],
    ", Random For:",RFtablomuz_SMOTE$byClass[1],
    ", Log.Regrs:",LRtablomuz_SMOTE$byClass[1],"\nSMOTE Specifity:    ",",KNN:",knnTablo_SMOTE$byClass[2],",  NB:" ,NBtablomuz_SMOTE$byClass[2],
    ",  SVM:",SVMtablomuz_SMOTE$byClass[2]
    ,", C5.0:",C50tablomuz_SMOTE$byClass[2],
    ", CART:",CARTtablomuz_SMOTE$byClass[2],
    ", Random For:",RFtablomuz_SMOTE$byClass[2],
    ", Log.Regrs:",LRtablomuz_SMOTE$byClass[2],
    "\nSMOTE Precision:    ",",KNN:",knnTablo_SMOTE$byClass[5],",  NB:" ,NBtablomuz_SMOTE$byClass[5],
    ",  SVM:",SVMtablomuz_SMOTE$byClass[5]
    ,", C5.0:",C50tablomuz_SMOTE$byClass[5],
    ", CART:",CARTtablomuz_SMOTE$byClass[5],
    ", Random For:",RFtablomuz_SMOTE$byClass[5],
    ", Log.Regrs:",LRtablomuz_SMOTE$byClass[5],
    "\nSMOTE F-1 Score:    ",",KNN:",knnTablo_SMOTE$byClass[7],",  NB:" ,NBtablomuz_SMOTE$byClass[7],
    ",  SVM:",SVMtablomuz_SMOTE$byClass[7]
    ,", C5.0:",C50tablomuz_SMOTE$byClass[7],
    ", CART:",CARTtablomuz_SMOTE$byClass[7],
    ", Random For:",RFtablomuz_SMOTE$byClass[7],
    ", Log.Regrs:",LRtablomuz_SMOTE$byClass[7])



##################################################
#####################   MWMOTE DENEMESİ  #############
##################################################

#df_m$motivation<-as.numeric(df_m$motivation)

#3.4 için
set.seed(919)
newSamples <- mwmote(df_m, numInstances = 169, classAttr = "motivation")

#3.5 için
set.seed(920)
newSamples <- mwmote(df_m, numInstances = 155, classAttr = "motivation")

#3.6 için
set.seed(921)
newSamples <- mwmote(df_m, numInstances = 141, classAttr = "motivation")


# 4.0 için *********
set.seed(923)
newSamples <- mwmote(df_m, numInstances = 79, classAttr = "motivation")
table(newSamples$motivation)


table(newSamples$motivation)
Ogrenciler_MWMOTE<-rbind(df_m,newSamples)
table(Ogrenciler_MWMOTE$motivation)


### Mwmote TUM bölünmesi ##
#### düz tüm veriyle eğitim ###
train_attributes_MWMOTE_TUM<-Ogrenciler_MWMOTE[,-ncol(Ogrenciler_MWMOTE)]
train_Target_MWMOTE_TUM<-Ogrenciler_MWMOTE[[ncol(Ogrenciler_MWMOTE)]]

######## validation için ayırma #########
validation_Test_MWMOTE<-df_v[,-ncol(df_v)]
validation_TestTarget_MWMOTE<-df_v[[ncol(df_v)]]
colnames(validation_Test_MWMOTE)<-colnames(train_attributes_MWMOTE_TUM)


### Knn_MWMOTE###
set.seed(75)
tahmin_MWMOTE<-knn(train_attributes_MWMOTE_TUM,validation_Test_MWMOTE,train_Target_MWMOTE_TUM,k=9)
#tahmin_SMOTE <- relevel(tahmin_SMOTE, "1")
summary(tahmin_MWMOTE)
knnTablo_MWMOTE <- confusionMatrix(data = tahmin_MWMOTE, reference = validation_TestTarget_MWMOTE, mode = "everything",positive="1")
knnTablo_MWMOTE



knnTablo_MWMOTE$overall[1]
knnTablo_MWMOTE$byClass[1]
knnTablo_MWMOTE$byClass[2]
knnTablo_MWMOTE$byClass[5]
knnTablo_MWMOTE$byClass[7]


## NB_SMOTE ##

set.seed(78)
NBmodelimiz_MWMOTE<-naiveBayes(motivation ~.,data=Ogrenciler_MWMOTE)
NBtahminimiz_MWMOTE<-predict(NBmodelimiz_MWMOTE, validation_Test_MWMOTE)

NBtablomuz_MWMOTE <- confusionMatrix(data = NBtahminimiz_MWMOTE, reference = validation_TestTarget_MWMOTE, mode = "everything",positive="1")
NBtablomuz_MWMOTE

NBtablomuz_MWMOTE$overall[1]
NBtablomuz_MWMOTE$byClass[1]
NBtablomuz_MWMOTE$byClass[2]
NBtablomuz_MWMOTE$byClass[5]
NBtablomuz_MWMOTE$byClass[7]


## SVM_MWMOTE ##
set.seed(769)
SVMmodelimiz_MWMOTE<- svm(formula= motivation ~.,data=Ogrenciler_MWMOTE,type="C-classification",kernel="linear")
SVMtahminimiz_MWMOTE<-predict(SVMmodelimiz_MWMOTE, validation_Test_MWMOTE)
SVMtablomuz_MWMOTE<-confusionMatrix(data=SVMtahminimiz_MWMOTE,reference=validation_TestTarget_MWMOTE,mode="everything",positive = "1")
SVMtablomuz_MWMOTE
SVMtablomuz_MWMOTE$overall[1]
SVMtablomuz_MWMOTE$byClass[1]
SVMtablomuz_MWMOTE$byClass[2]
SVMtablomuz_MWMOTE$byClass[5]
SVMtablomuz_MWMOTE$byClass[7]

### C5.0_MWMOTE###

set.seed(353)
C50modelimiz_MWMOTE<-C5.0(train_attributes_MWMOTE_TUM, train_Target_MWMOTE_TUM)
C50tahminimiz_MWMOTE<-predict(C50modelimiz_MWMOTE,validation_Test_MWMOTE)
C50tablomuz_MWMOTE<-confusionMatrix(data=C50tahminimiz_MWMOTE, reference=validation_TestTarget_MWMOTE,mode="everything", positive="1")
C50tablomuz_MWMOTE
C50tablomuz_MWMOTE$overall[1]
C50tablomuz_MWMOTE$byClass[1]
C50tablomuz_MWMOTE$byClass[2]
C50tablomuz_MWMOTE$byClass[5]
C50tablomuz_MWMOTE$byClass[7]
#plot(C50modelimiz_MWMOTE, cex.lab =0.001)
#prp(C50modelimiz_MWMOTE)
summary(C50modelimiz_MWMOTE)

### CART _MWMOTE###
set.seed(838)
CARTmodelimiz_MWMOTE<- rpart(motivation~., Ogrenciler_MWMOTE,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_MWMOTE<-predict(CARTmodelimiz_MWMOTE,validation_Test_MWMOTE,type="class")
CARTtablomuz_MWMOTE<-confusionMatrix(data=CARTtahminimiz_MWMOTE,reference=validation_TestTarget_MWMOTE,mode="everything", positive="1")
CARTtablomuz_MWMOTE
rpart.plot(CARTmodelimiz_MWMOTE,type=3,fallen.leaves = TRUE,cex=0.9)

CARTtablomuz_MWMOTE$overall[1]
CARTtablomuz_MWMOTE$byClass[1]
CARTtablomuz_MWMOTE$byClass[2]
CARTtablomuz_MWMOTE$byClass[5]
CARTtablomuz_MWMOTE$byClass[7]

############## RandomForest_MWMOTE ###########

library(randomForest)
set.seed(892)
RFmodelimiz_MWMOTE<- randomForest(motivation~.,data=Ogrenciler_MWMOTE,proximity=TRUE)
RFtahminimiz_MWMOTE<-predict(RFmodelimiz_MWMOTE,validation_Test_MWMOTE,type="class")
RFtablomuz_MWMOTE<-confusionMatrix(data=RFtahminimiz_MWMOTE,reference=validation_TestTarget_MWMOTE,mode="everything",positive="1")
RFtablomuz_MWMOTE
RFtablomuz_MWMOTE$overall[1]
RFtablomuz_MWMOTE$byClass[1]
RFtablomuz_MWMOTE$byClass[2]
RFtablomuz_MWMOTE$byClass[5]
RFtablomuz_MWMOTE$byClass[7]

############## LogisticRegression_MWMOTE ###########

set.seed(729)
LRmodelimiz_MWMOTE<- glm(motivation~.,family= "binomial", data=Ogrenciler_MWMOTE)

LRtahminimiz_MWMOTE<-predict(LRmodelimiz_MWMOTE,validation_Test_MWMOTE,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_MWMOTE))

LRtablomuz_MWMOTE<-confusionMatrix(data=predicted, reference=validation_TestTarget_MWMOTE,mode="everything",positive="1")

LRtablomuz_MWMOTE

summary(LRmodelimiz_MWMOTE)


# PRINTING MWMOTE #

print(paste0("MWMOTE ACCURACY:    ","KNN:",knnTablo_MWMOTE$overall[1],"  NB:" ,NBtablomuz_MWMOTE$overall[1],
             "  SVM:",SVMtablomuz_MWMOTE$overall[1]
             ," C5.0:",C50tablomuz_MWMOTE$overall[1],
             " CART:",CARTtablomuz_MWMOTE$overall[1],
             " Random For:",RFtablomuz_MWMOTE$overall[1],
             " Log.Regrs:",LRtablomuz_MWMOTE$overall[1]))

print(paste0("MWMOTE Sensitivity:    ","KNN:",knnTablo_MWMOTE$byClass[1],"  NB:" ,NBtablomuz_MWMOTE$byClass[1],
             "  SVM:",SVMtablomuz_MWMOTE$byClass[1]
             ," C5.0:",C50tablomuz_MWMOTE$byClass[1],
             " CART:",CARTtablomuz_MWMOTE$byClass[1],
             " Random For:",RFtablomuz_MWMOTE$byClass[1],
             " Log.Regrs:",LRtablomuz_MWMOTE$byClass[1]))

print(paste0("MWMOTE Specifity:    ","KNN:",knnTablo_MWMOTE$byClass[2],"  NB:" ,NBtablomuz_MWMOTE$byClass[2],
             "  SVM:",SVMtablomuz_MWMOTE$byClass[2]
             ," C5.0:",C50tablomuz_MWMOTE$byClass[2],
             " CART:",CARTtablomuz_MWMOTE$byClass[2],
             " Random For:",RFtablomuz_MWMOTE$byClass[2],
             " Log.Regrs:",LRtablomuz_MWMOTE$byClass[2]))

print(paste0("MWMOTE Precision:    ","KNN:",knnTablo_MWMOTE$byClass[5],"  NB:" ,NBtablomuz_MWMOTE$byClass[5],
             "  SVM:",SVMtablomuz_MWMOTE$byClass[5]
             ," C5.0:",C50tablomuz_MWMOTE$byClass[5],
             " CART:",CARTtablomuz_MWMOTE$byClass[5],
             " Random For:",RFtablomuz_MWMOTE$byClass[5],
             " Log.Regrs:",LRtablomuz_MWMOTE$byClass[5]))

print(paste0("MWMOTE F-1 Score:    ","KNN:",knnTablo_MWMOTE$byClass[7],"  NB:" ,NBtablomuz_MWMOTE$byClass[7],
             "  SVM:",SVMtablomuz_MWMOTE$byClass[7]
             ," C5.0:",C50tablomuz_MWMOTE$byClass[7],
             " CART:",CARTtablomuz_MWMOTE$byClass[7],
             " Random For:",RFtablomuz_MWMOTE$byClass[7],
             " Log.Regrs:",LRtablomuz_MWMOTE$byClass[7]))



###############
cat("MWMOTE ACCURACY:    ",",KNN:",knnTablo_MWMOTE$overall[1],",  NB:" ,NBtablomuz_MWMOTE$overall[1],
    ",  SVM:",SVMtablomuz_MWMOTE$overall[1]
    ,", C5.0:",C50tablomuz_MWMOTE$overall[1],
    ", CART:",CARTtablomuz_MWMOTE$overall[1],
    ", Random For:",RFtablomuz_MWMOTE$overall[1],
    ", Log.Regrs:",LRtablomuz_MWMOTE$overall[1],"\nMWMOTE Sensitivity:    ",",KNN:",knnTablo_MWMOTE$byClass[1],",  NB:" ,NBtablomuz_MWMOTE$byClass[1],
    ",  SVM:",SVMtablomuz_MWMOTE$byClass[1]
    ,", C5.0:",C50tablomuz_MWMOTE$byClass[1],
    ", CART:",CARTtablomuz_MWMOTE$byClass[1],
    ", Random For:",RFtablomuz_MWMOTE$byClass[1],
    ", Log.Regrs:",LRtablomuz_MWMOTE$byClass[1],"\nMWMOTE Specifity:    ",",KNN:",knnTablo_MWMOTE$byClass[2],",  NB:" ,NBtablomuz_MWMOTE$byClass[2],
    ",  SVM:",SVMtablomuz_MWMOTE$byClass[2]
    ,", C5.0:",C50tablomuz_MWMOTE$byClass[2],
    ", CART:",CARTtablomuz_MWMOTE$byClass[2],
    ", Random For:",RFtablomuz_MWMOTE$byClass[2],
    ", Log.Regrs:",LRtablomuz_MWMOTE$byClass[2],
    "\nMWMOTE Precision:    ",",KNN:",knnTablo_MWMOTE$byClass[5],",  NB:" ,NBtablomuz_MWMOTE$byClass[5],
    ",  SVM:",SVMtablomuz_MWMOTE$byClass[5]
    ,", C5.0:",C50tablomuz_MWMOTE$byClass[5],
    ", CART:",CARTtablomuz_MWMOTE$byClass[5],
    ", Random For:",RFtablomuz_MWMOTE$byClass[5],
    ", Log.Regrs:",LRtablomuz_MWMOTE$byClass[5],
    "\nMWMOTE F-1 Score:    ",",KNN:",knnTablo_MWMOTE$byClass[7],",  NB:" ,NBtablomuz_MWMOTE$byClass[7],
    ",  SVM:",SVMtablomuz_MWMOTE$byClass[7]
    ,", C5.0:",C50tablomuz_MWMOTE$byClass[7],
    ", CART:",CARTtablomuz_MWMOTE$byClass[7],
    ", Random For:",RFtablomuz_MWMOTE$byClass[7],
    ", Log.Regrs:",LRtablomuz_MWMOTE$byClass[7])




