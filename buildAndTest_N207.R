######## 

url<-"https://raw.githubusercontent.com/mithatyavuzarslan/predictingMotivation/main/finalSetSon_Mor.csv"
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

hist(df$motivation,labels=TRUE, main='I.SET (N=207)\n "Motivasyon" Hedef Niteliği\n 0:Düşük 1:Yüksek',
     xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))


library(clusterSim)


df[,-ncol(df)]<-data.Normalization(df[,-ncol(df)],type="n4",normalization ="column")

df$motivation<-as.factor(df$motivation)
levels(df$motivation)
table(df$motivation)



############ RANDOM SUBSaMPLING#################

seedim<-713
require(class)
library(e1071)
library(C50)
library(rpart)
library(rpart.plot)
require(caret)
require(class)
help(createDataPartition)

NB_modeldizimiz<-vector()
SVM_modeldizimiz<-vector()
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
  trainingIndexes<-createDataPartition(y=df$motivation,p=.70,list=FALSE)
  #print(trainingIndexes[1:20])
  
  train_data<-df[trainingIndexes,]
  test_data<-df[-trainingIndexes,]
  
  train_Attributes<-train_data[,-ncol(train_data)]
  train_Target<-train_data[[ncol(train_data)]]
  
  test_Attributes<-test_data[,-ncol(test_data)]
  test_Target<-test_data[[ncol(test_data)]]
  
  
  #### KNN_Random##
  predictions<-knn(train = train_Attributes,test = test_Attributes,cl = train_Target,k = 9)
  
  tablomuz <- confusionMatrix(data = predictions, reference = test_Target, mode = "everything",positive="1")
  
  dizimiz[i]<-tablomuz$overall[1]
  dizimizSens[i]<-tablomuz$byClass[1]
  dizimizSpec[i]<-tablomuz$byClass[2]
  dizimizPrec[i]<-tablomuz$byClass[5]
  dizimizF1[i]<-tablomuz$byClass[7]
  
  ##NAIVE BAYES_Random##
  NBmodelimiz<-naiveBayes(motivation ~.,data=train_data)
  NBtahminimiz<-predict(NBmodelimiz, test_Attributes)
  NBtablomuz <- confusionMatrix(data = NBtahminimiz, reference = test_Target, mode = "everything",positive="1")
  
  #mdl <- train(motivation ~ .,data=train_data,method="naive_bayes")
  #NB_modeldizimiz[i]<-mdl
  
  
  NBdizimiz[i]<-NBtablomuz$overall[1]
  NBdizimizSens[i]<-NBtablomuz$byClass[1]
  NBdizimizSpec[i]<-NBtablomuz$byClass[2]
  NBdizimizPrec[i]<-NBtablomuz$byClass[5]
  NBdizimizF1[i]<-NBtablomuz$byClass[7]
  
  ## SVM_Random ##
  
  SVMmodelimiz<- svm(formula= motivation ~.,data=train_data,type="C-classification",kernel="linear")
  SVMtahminimiz<-predict(SVMmodelimiz, test_Attributes)
  SVMtablomuz<-confusionMatrix(data=SVMtahminimiz,reference=test_Target,mode="everything",positive = "1")
  
  
  
  SVMdizimiz[i]<-SVMtablomuz$overall[1]
  SVMdizimizSens[i]<-SVMtablomuz$byClass[1]
  SVMdizimizSpec[i]<-SVMtablomuz$byClass[2]
  SVMdizimizPrec[i]<-SVMtablomuz$byClass[5]
  SVMdizimizF1[i]<-SVMtablomuz$byClass[7]
  
  ### C5.0 _RANDOM###
  
  C50modelimiz<-C5.0(train_Attributes, train_Target)
  C50tahminimiz<-predict(C50modelimiz,test_Attributes)
  C50tablomuz<-confusionMatrix(data=C50tahminimiz, reference=test_Target,mode="everything", positive="1")
  
  C50dizimiz[i]<-C50tablomuz$overall[1]
  C50dizimizSens[i]<-C50tablomuz$byClass[1]
  C50dizimizSpec[i]<-C50tablomuz$byClass[2]
  C50dizimizPrec[i]<-C50tablomuz$byClass[5]
  C50dizimizF1[i]<-C50tablomuz$byClass[7]
  
  
  
  ### CART _Random###
  CARTmodelimiz<- rpart(motivation~., train_data,method="class") 
  #rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
  CARTtahminimiz<-predict(CARTmodelimiz,test_Attributes,type="class")
  CARTtablomuz<-confusionMatrix(data=CARTtahminimiz,reference=test_Target,mode="everything", positive="1")
  
  CARTdizimiz[i]<-CARTtablomuz$overall[1]
  CARTdizimizSens[i]<-CARTtablomuz$byClass[1]
  CARTdizimizSpec[i]<-CARTtablomuz$byClass[2]
  CARTdizimizPrec[i]<-CARTtablomuz$byClass[5]
  CARTdizimizF1[i]<-CARTtablomuz$byClass[7]
  
  
  ############## RANDOM FOREST_Random #########
  
  
  RFmodelimiz<- randomForest(motivation~.,data=train_data,proximity=TRUE)
  RFtahminimiz<-predict(RFmodelimiz,test_Attributes,type="class")
  RFtablomuz<-confusionMatrix(data=RFtahminimiz,reference=test_Target,mode="everything",positive="1")
  
  RFdizimiz[i]<-RFtablomuz$overall[1]
  RFdizimizSens[i]<-RFtablomuz$byClass[1]
  RFdizimizSpec[i]<-RFtablomuz$byClass[2]
  RFdizimizPrec[i]<-RFtablomuz$byClass[5]
  RFdizimizF1[i]<-RFtablomuz$byClass[7]
  
  
  ####### Logistic Regression for Binary_Random ######
  LRmodelimiz<- glm(motivation~.,family= "binomial", data=train_data)
  
  LRtahminimiz<-predict(LRmodelimiz,test_Attributes,type="response")
  #predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
  predicted <- as.factor(round(LRtahminimiz))
  
  LRtablomuz<-confusionMatrix(data=predicted, reference=test_Target,mode="everything",positive="1")
  
  
  LRdizimiz[i]<-LRtablomuz$overall[1]
  LRdizimizSens[i]<-LRtablomuz$byClass[1]
  LRdizimizSpec[i]<-LRtablomuz$byClass[2]
  LRdizimizPrec[i]<-LRtablomuz$byClass[5]
  LRdizimizF1[i]<-LRtablomuz$byClass[7]
  
  
  
}


print(paste0("RANDOM SS ACCURACY:    ",",KNN:",mean(dizimiz),",  NB:" ,mean(NBdizimiz),",  SVM:",mean(SVMdizimiz)
             ,", C5.0:",mean(C50dizimiz),
             ", CART:",mean(CARTdizimiz),
             " ,Random For:",mean(RFdizimiz),
             " ,Log.Regrs:",mean(LRdizimiz)))

print(paste0("RANDOM SS SENSITIVITY:    ",",KNN:",mean(dizimizSens),",  NB:" ,mean(NBdizimizSens),",  SVM:",mean(SVMdizimizSens)
             ,", C5.0:",mean(C50dizimizSens),
             ", CART:",mean(CARTdizimizSens),
             ", Random For:",mean(RFdizimizSens),
             ", Log.Regrs:",mean(LRdizimizSens)))

print(paste0("RANDOM SS SPECIFITY:    ",",KNN:",mean(dizimizSpec),",  NB:" ,mean(NBdizimizSpec),",  SVM:",mean(SVMdizimizSpec)
             ,", C5.0:",mean(C50dizimizSpec),
             ", CART:",mean(CARTdizimizSpec),
             ", Random For:",mean(RFdizimizSpec),
             ",Log.Regrs:",mean(LRdizimizSpec)))

print(paste0("RANDOM SS PRECISION:    ",",KNN:",mean(dizimizPrec),",  NB:" ,mean(NBdizimizPrec),",  SVM:",mean(SVMdizimizPrec)
             ,", C5.0:",mean(C50dizimizPrec),
             ", CART:",mean(CARTdizimizPrec),
             ", Random For:",mean(RFdizimizPrec),
             ", Log.Regrs:",mean(LRdizimizPrec)))

print(paste0("RANDOM SS F-1:    ",",KNN:",mean(dizimizF1),",  NB:" ,mean(NBdizimizF1),",  SVM:",mean(SVMdizimizF1)
             ,", C5.0:",mean(C50dizimizF1),
             ", CART:",mean(CARTdizimizF1),
             ",Random For:",mean(RFdizimizF1),
             ",Log.Regrs:",mean(LRdizimizF1)))


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
help(ovun.sample)
str(df$motivation)


set.seed(400)
Ogrenciler_Over<-ovun.sample(motivation~.,data=df,method="over",N=286)$data
Ogrenciler_Over<-ovun.sample(motivation~.,data=df,method="over",N=574)$data
table(Ogrenciler_Over$motivation)

summary(Ogrenciler_Over)

##### Over_ korelasyon ####
df2<-Ogrenciler_Over
df2$motivation<-as.numeric(df2$motivation)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.6)
#Ogrenciler_Over<- Ogrenciler_Over[,c(-14)]
#YuklenenOdev cikarildi
levels(Ogrenciler_Over$motivation)
s<-as.numeric(as.character(Ogrenciler_Over$motivation))
table(s)
hist(as.numeric(as.character(Ogrenciler_Over$motivation)),labels=TRUE, 
     main='Yukarı Örnekleme Uygulanan \nI. SET "Motivation" Özniteliği (N=286)',
     xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))




require(caret)
library(caret)
set.seed(20)
egitimIndisleri_Over<-createDataPartition(y = Ogrenciler_Over$motivation,p = .70, list=FALSE)


egitim_Over <- Ogrenciler_Over[egitimIndisleri_Over,]
test_Over <- Ogrenciler_Over[-egitimIndisleri_Over,]

egitimNitelik_Over<-egitim_Over[,-ncol(egitim_Over)]
egitimHedefNitelik_Over<-egitim_Over[[ncol(egitim_Over)]]

testNitelik_Over<-test_Over[,-ncol(test_Over)]
testHedefNitelik_Over<-test_Over[[ncol(test_Over)]]

str(egitimNitelik_Over)
str(testNitelik_Over)
require(class)
library(class)

### Knn_Over###
set.seed(31)
tahmin_Over<-knn(egitimNitelik_Over,testNitelik_Over,egitimHedefNitelik_Over,k=9)
summary(tahmin_Over)
tablo2 <- confusionMatrix(data = tahmin_Over, reference = testHedefNitelik_Over, mode = "everything",positive="1")
tablo2

accuracy_over<-tablo2$overall[1]

## NB_Over ##

set.seed(971)
NBmodelimiz_Over<-naiveBayes(motivation ~.,data=egitim_Over)
NBtahminimiz_Over<-predict(NBmodelimiz_Over, testNitelik_Over)
NBtablomuz_Over <- confusionMatrix(data = NBtahminimiz_Over, reference = testHedefNitelik_Over, mode = "everything",positive="1")
NBtablomuz_Over

NBtablomuz_Over$overall[1]
NBtablomuz_Over$byClass[1]
NBtablomuz_Over$byClass[2]
NBtablomuz_Over$byClass[5]
NBtablomuz_Over$byClass[7]

## SVM_Over ##
set.seed(765)
SVMmodelimiz_Over<- svm(formula= motivation ~.,data=egitim_Over,type="C-classification",kernel="linear")
SVMtahminimiz_Over<-predict(SVMmodelimiz_Over, testNitelik_Over)
SVMtablomuz_Over<-confusionMatrix(data=SVMtahminimiz_Over,reference=testHedefNitelik_Over,mode="everything",positive = "1")

SVMtablomuz_Over

SVMtablomuz_Over$overall[1]
SVMtablomuz_Over$byClass[1]
SVMtablomuz_Over$byClass[2]
SVMtablomuz_Over$byClass[5]
SVMtablomuz_Over$byClass[7]


### C5.0_Over###

set.seed(349)
C50modelimiz_Over<-C5.0(egitimNitelik_Over, egitimHedefNitelik_Over)
C50tahminimiz_Over<-predict(C50modelimiz_Over,testNitelik_Over)
C50tablomuz_Over<-confusionMatrix(data=C50tahminimiz_Over, reference=testHedefNitelik_Over,mode="everything", positive="1")
C50tablomuz_Over
C50tablomuz_Over$overall[1]
C50tablomuz_Over$byClass[1]
C50tablomuz_Over$byClass[2]
C50tablomuz_Over$byClass[5]
C50tablomuz_Over$byClass[7]

### CART _OVer###
set.seed(831)
CARTmodelimiz_Over<- rpart(motivation~., egitim_Over,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_Over<-predict(CARTmodelimiz_Over,testNitelik_Over,type="class")
CARTtablomuz_Over<-confusionMatrix(data=CARTtahminimiz_Over,reference=testHedefNitelik_Over,mode="everything", positive="1")

CART3<-as.data.frame(CARTmodelimiz_Over$variable.importance)


CART3<-cbind(rownames(CART3),CART3)

CART3<-CART3[order(CART3$`CARTmodelimiz_Over$variable.importance`),]

CART3$`CARTmodelimiz_Over$variable.importance`<-as.numeric(CART3$`CARTmodelimiz_Over$variable.importance`)

CART3$`CARTmodelimiz_Over$variable.importance`<-round(CART3$`CARTmodelimiz_Over$variable.importance`,digits=2)

aa<-ggplot(CART3,aes(y=CART3$`CARTmodelimiz_Over$variable.importance`, x=reorder(CART3$`rownames(CART3)`,CART3$`CARTmodelimiz_Over$variable.importance`)))+ geom_bar(stat="identity") +geom_text(aes(label = CART3$`CARTmodelimiz_Over$variable.importance`),hjust=1,colour = "green")
aa+coord_flip()
##
require("ROCR")

ali<-predict(CARTmodelimiz_Over,test_Over,type="prob")
library(pROC)
tree.roc<-roc(as.numeric(test_Over$motivation),ali)
##

CARTtablomuz_Over
CARTtablomuz_Over$overall[1]
CARTtablomuz_Over$byClass[1]
CARTtablomuz_Over$byClass[2]
CARTtablomuz_Over$byClass[5]
CARTtablomuz_Over$byClass[7]

############## RANDOM FOREST #########
library(randomForest)
set.seed(891)
RFmodelimiz_Over<- randomForest(motivation~.,data=egitim_Over,proximity=TRUE)
RFtahminimiz_Over<-predict(RFmodelimiz_Over,testNitelik_Over,type="class")
RFtablomuz_Over<-confusionMatrix(data=RFtahminimiz_Over,reference=testHedefNitelik_Over,mode="everything",positive="1")
RFtablomuz_Over
library("party")
randomForest::getTree(RFmodelimiz_Over,3, labelVar = TRUE)
importance(RFmodelimiz_Over)
varImpPlot(RFmodelimiz_Over)

RFmodelimiz_Over$importance

RF3<-as.data.frame(RFmodelimiz_Over$importance)
barplot(colnames(RF3),RF3$MeanDecreaseGini)

barplot(RF3$MeanDecreaseGini)
View(RF3)
RF3<-cbind(rownames(RF3),RF3)
barplot(RF3)
RF3<-RF3[order(RF3$MeanDecreaseGini),]
barplot(RF3$`rownames(RF3)`)

barplot(RF3$MeanDecreaseGini)
par(mar=c(2, 5, 3, 1)) 
par(mar=c(3, 7, 3, 1))
barplot(sort(RF3$MeanDecreaseGini),names.arg = RF3$`rownames(RF3)`,horiz = TRUE,las=2,xlim=c(0,25),cex.names = 0.50,)
View(RF3)
RF3$MeanDecreaseGini<-as.numeric(RF3$MeanDecreaseGini)

RF3$MeanDecreaseGini<-round(RF3$MeanDecreaseGini,digits=2)

aa<-ggplot(RF3,aes(y=RF3$MeanDecreaseGini, x=reorder(RF3$`rownames(RF3)`,RF3$MeanDecreaseGini)))+ geom_bar(stat="identity") +geom_text(aes(label = RF3$MeanDecreaseGini),hjust=1,colour = "green")
aa+coord_flip()
aa


####### Logistic Regression for Binary ######
set.seed(724)
LRmodelimiz_Over<- glm(motivation~.,family= "binomial", data=egitim_Over)

LRtahminimiz_Over<-predict(LRmodelimiz_Over,testNitelik_Over,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_Over))

LRtablomuz_Over<-confusionMatrix(data=predicted, reference=testHedefNitelik_Over,mode="everything",positive="1")

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

######################
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
    ",  SVM:",SVMtablomuz_Over$byClass[2]
    ,", C5.0:",C50tablomuz_Over$byClass[2],
    ", CART:",CARTtablomuz_Over$byClass[2],
    ", Random For:",RFtablomuz_Over$byClass[2],
    ", Log.Regrs:",LRtablomuz_Over$byClass[2],
    "\nOVERSAMPLING Precision:    ",",KNN:",tablo2$byClass[5],",  NB:" ,NBtablomuz_Over$byClass[5],
    ",  SVM:",SVMtablomuz_Over$byClass[5]
    ,", C5.0:",C50tablomuz_Over$byClass[5],
    ", CART:",CARTtablomuz_Over$byClass[5],
    ", Random For:",RFtablomuz_Over$byClass[5],
    ", Log.Regrs:",LRtablomuz_Over$byClass[5],
    "\nOVERSAMPLING F-1 Score:    ",",KNN:",tablo2$byClass[7],",  NB:" ,NBtablomuz_Over$byClass[7],
    ",  SVM:",SVMtablomuz_Over$byClass[7]
    ,", C5.0:",C50tablomuz_Over$byClass[7],
    ", CART:",CARTtablomuz_Over$byClass[7],
    ", Random For:",RFtablomuz_Over$byClass[7],
    ", Log.Regrs:",LRtablomuz_Over$byClass[7])












###############################################
################ S     M     O     T     E ##################
#############################################

#Ogrenciler_SMOTE<-smotefamily::SMOTE(YYY,YYY$motivation,K=5)
#Ogrenciler_SMOTE<-Ogrenciler_SMOTE$data
#table(Ogrenciler_SMOTE$motivation)
#Ogrenciler_SMOTE$motivation<-as.factor(Ogrenciler_SMOTE$motivation)
#levels(Ogrenciler_SMOTE$motivation)<-c("1","0")
#Ogrenciler_SMOTE<-Ogrenciler_SMOTE[,-ncol(Ogrenciler_SMOTE)]

#require(DMwR)
#install.packages("DMwR")
#library(DMwR)
#install.packages("performanceEstimation")

#unloadNamespace("performanceEstimation")


library(themis)
#library(performanceEstimation)
#help(performanceEstimation)
#help(smote)

#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=4)
####### 3.4 için
set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df,var = "motivation",k = 5,over_ratio = 1.065)
table(Ogrenciler_SMOTE$motivation)
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=5, perc.under=1.2,k = 5)
#library(DMwR)
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=10, perc.under=1.1)
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=7,k = 5)


###### 3.5 için
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=7.9, perc.under=1.15)
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=7, perc.under=1.15)
#table(Ogrenciler_SMOTE$motivation)
set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df,var = "motivation",k = 5,over_ratio = 1.11)
table(Ogrenciler_SMOTE$motivation)


###### 3.6 için
#Ogrenciler_SMOTE <- smote(motivation ~ ., df,perc.over=1, perc.under=1.15)
#table(Ogrenciler_SMOTE$motivation)
set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df,var = "motivation",k = 5,over_ratio = 1.15)
table(Ogrenciler_SMOTE$motivation)

###### 4.0 için **************

set.seed(600)
Ogrenciler_SMOTE<-themis::smote(df = df,var = "motivation",k = 5,over_ratio = 1)
table(Ogrenciler_SMOTE$motivation)


summary(df$motivation)

hist(as.numeric(as.character(Ogrenciler_SMOTE$motivation)),labels=TRUE, 
     main='SMOTE Uygulanan \nI. SET "Motivation" Özniteliği (N=286)',
     xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))


######## SMOTE Korelasyon ######

df2<-Ogrenciler_SMOTE
df2$motivation<-as.numeric(df2$motivation)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.6)

#Ogrenciler_Over<- Ogrenciler_Over[,c(-15)]


require(caret)
library(caret)
set.seed(21)
egitimIndisleri_SMOTE<-createDataPartition(y = Ogrenciler_SMOTE$motivation,p = .70, list=FALSE)


egitim_SMOTE <- Ogrenciler_SMOTE[egitimIndisleri_SMOTE,]
test_SMOTE <- Ogrenciler_SMOTE[-egitimIndisleri_SMOTE,]

egitimNitelik_SMOTE<-egitim_SMOTE[,-ncol(egitim_SMOTE)]
egitimHedefNitelik_SMOTE<-egitim_SMOTE[[ncol(egitim_SMOTE)]]

testNitelik_SMOTE<-test_SMOTE[,-ncol(test_SMOTE)]
testHedefNitelik_SMOTE<-test_SMOTE[[ncol(test_SMOTE)]]



summary(testHedefNitelik_SMOTE)

str(testNitelik_SMOTE)



levels(testHedefNitelik_SMOTE)



### Knn_SMOTE###
set.seed(75)
tahmin_SMOTE<-knn(egitimNitelik_SMOTE,testNitelik_SMOTE,egitimHedefNitelik_SMOTE,k=9)
#tahmin_SMOTE <- relevel(tahmin_SMOTE, "1")
summary(tahmin_SMOTE)
knnTablo_SMOTE <- confusionMatrix(data = tahmin_SMOTE, reference = testHedefNitelik_SMOTE, mode = "everything",positive="1")
knnTablo_SMOTE

knnTablo_SMOTE$overall[1]
knnTablo_SMOTE$byClass[1]
knnTablo_SMOTE$byClass[2]
knnTablo_SMOTE$byClass[5]
knnTablo_SMOTE$byClass[7]
## NB_SMOTE ##

set.seed(78)
NBmodelimiz_SMOTE<-naiveBayes(motivation ~.,data=egitim_SMOTE)
NBtahminimiz_SMOTE<-predict(NBmodelimiz_SMOTE, testNitelik_SMOTE)
NBtablomuz_SMOTE <- confusionMatrix(data = NBtahminimiz_SMOTE, reference = testHedefNitelik_SMOTE, mode = "everything",positive="1")
NBtablomuz_SMOTE

NBtablomuz_SMOTE$overall[1]
NBtablomuz_SMOTE$byClass[1]
NBtablomuz_SMOTE$byClass[2]
NBtablomuz_SMOTE$byClass[5]
NBtablomuz_SMOTE$byClass[7]

## SVM_SMOTE ##
set.seed(769)
SVMmodelimiz_SMOTE<- svm(formula= motivation ~.,data=egitim_SMOTE,type="C-classification",kernel="linear")
SVMtahminimiz_SMOTE<-predict(SVMmodelimiz_SMOTE, testNitelik_SMOTE)
SVMtablomuz_SMOTE<-confusionMatrix(data=SVMtahminimiz_SMOTE,reference=testHedefNitelik_SMOTE,mode="everything",positive = "1")
SVMtablomuz_SMOTE
SVMtablomuz_SMOTE$overall[1]
SVMtablomuz_SMOTE$byClass[1]
SVMtablomuz_SMOTE$byClass[2]
SVMtablomuz_SMOTE$byClass[5]
SVMtablomuz_SMOTE$byClass[7]


### C5.0_SMOTE###

set.seed(353)
C50modelimiz_SMOTE<-C5.0(egitimNitelik_SMOTE, egitimHedefNitelik_SMOTE)
C50tahminimiz_SMOTE<-predict(C50modelimiz_SMOTE,testNitelik_SMOTE)
C50tablomuz_SMOTE<-confusionMatrix(data=C50tahminimiz_SMOTE, reference=testHedefNitelik_SMOTE,mode="everything", positive="1")
C50tablomuz_SMOTE
C50tablomuz_SMOTE$overall[1]
C50tablomuz_SMOTE$byClass[1]
C50tablomuz_SMOTE$byClass[2]
C50tablomuz_SMOTE$byClass[5]
C50tablomuz_SMOTE$byClass[7]

### CART _SMOTE###
set.seed(838)
CARTmodelimiz_SMOTE<- rpart(motivation~., egitim_SMOTE,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_SMOTE<-predict(CARTmodelimiz_SMOTE,testNitelik_SMOTE,type="class")
CARTtablomuz_SMOTE<-confusionMatrix(data=CARTtahminimiz_SMOTE,reference=testHedefNitelik_SMOTE,mode="everything", positive="1")
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
RFmodelimiz_SMOTE<- randomForest(motivation~.,data=egitim_SMOTE,proximity=TRUE)
RFtahminimiz_SMOTE<-predict(RFmodelimiz_SMOTE,testNitelik_SMOTE,type="class")
RFtablomuz_SMOTE<-confusionMatrix(data=RFtahminimiz_SMOTE,reference=testHedefNitelik_SMOTE,mode="everything",positive="1")
RFtablomuz_SMOTE

varImpPlot(RFmodelimiz_SMOTE, main="Random Forest\n Variable Importance Plot")


RFtablomuz_SMOTE$overall[1]
RFtablomuz_SMOTE$byClass[1]
RFtablomuz_SMOTE$byClass[2]
RFtablomuz_SMOTE$byClass[5]
RFtablomuz_SMOTE$byClass[7]

############## LogisticRegression_SMOTE ###########

set.seed(729)
LRmodelimiz_SMOTE<- glm(motivation~.,family= "binomial", data=egitim_SMOTE)

LRtahminimiz_SMOTE<-predict(LRmodelimiz_SMOTE,testNitelik_SMOTE,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_SMOTE))

LRtablomuz_SMOTE<-confusionMatrix(data=predicted, reference=testHedefNitelik_SMOTE,mode="everything",positive="1")

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

####################

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





####################################
################## MWMOTE ###################
####################################
library(imbalance)
#3.4 için
set.seed(919)
newSamples <- mwmote(df, numInstances = 169, classAttr = "motivation",)

#3.5 için
set.seed(920)
newSamples <- mwmote(df, numInstances = 155, classAttr = "motivation")

#3.6 için
set.seed(921)
newSamples <- mwmote(df, numInstances = 141, classAttr = "motivation")

# 4.0 için *********
set.seed(923)
newSamples <- mwmote(df, numInstances = 79, classAttr = "motivation")
table(newSamples$motivation)


Ogrenciler_MWMOTE<-rbind(df,newSamples)
table(Ogrenciler_MWMOTE$motivation)

hist(as.numeric(as.character(Ogrenciler_MWMOTE$motivation)),labels=TRUE, 
     main='MWMOTE Uygulanan \nI. SET "Motivation" Özniteliği (N=286)',
     xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))



######## MWMOTE Korelasyon ######

df2<-Ogrenciler_MWMOTE
df2$motivation<-as.numeric(df2$motivation)
korelasyon<-cor(df2[,1:ncol(df2)],use = "pairwise.complete.obs",method="pearson")
korelasyon
corrplot(korelasyon,method="number", tl.cex=0.6, number.cex=0.57)


hist(as.numeric(as.character(Ogrenciler_MWMOTE$motivation)),labels=TRUE, main='I. SET "Motivation" Özniteliği (N=348)\n 3.İterasyon (Pozitif sınıf>=3,6)',xlab='Motivasyon', xlim=c(0,1),ylim=c(0,250), breaks=2, xaxt="n")
axis(side=1, at=c(1, 0))


require(caret)
library(caret)
set.seed(21)
egitimIndisleri_MWMOTE<-createDataPartition(y = Ogrenciler_MWMOTE$motivation,p = .70, list=FALSE)


egitim_MWMOTE <- Ogrenciler_MWMOTE[egitimIndisleri_MWMOTE,]
test_MWMOTE <- Ogrenciler_MWMOTE[-egitimIndisleri_MWMOTE,]

egitimNitelik_MWMOTE<-egitim_MWMOTE[,-ncol(egitim_MWMOTE)]
egitimHedefNitelik_MWMOTE<-egitim_MWMOTE[[ncol(egitim_MWMOTE)]]

testNitelik_MWMOTE<-test_MWMOTE[,-ncol(test_MWMOTE)]
testHedefNitelik_MWMOTE<-test_MWMOTE[[ncol(test_MWMOTE)]]



summary(testHedefNitelik_MWMOTE)

str(testNitelik_MWMOTE)



levels(testHedefNitelik_MWMOTE)



### Knn_MWMOTE###
set.seed(75)
tahmin_MWMOTE<-knn(egitimNitelik_MWMOTE,testNitelik_MWMOTE,egitimHedefNitelik_MWMOTE,k=9)
#tahmin_MWMOTE <- relevel(tahmin_MWMOTE, "1")
summary(tahmin_MWMOTE)
knnTablo_MWMOTE <- confusionMatrix(data = tahmin_MWMOTE, reference = testHedefNitelik_MWMOTE, mode = "everything",positive="1")
knnTablo_MWMOTE

knnTablo_MWMOTE$overall[1]
knnTablo_MWMOTE$byClass[1]
knnTablo_MWMOTE$byClass[2]
knnTablo_MWMOTE$byClass[5]
knnTablo_MWMOTE$byClass[7]
## NB_MWMOTE ##

set.seed(78)
NBmodelimiz_MWMOTE<-naiveBayes(motivation ~.,data=egitim_MWMOTE)
NBtahminimiz_MWMOTE<-predict(NBmodelimiz_MWMOTE, testNitelik_MWMOTE)
NBtablomuz_MWMOTE <- confusionMatrix(data = NBtahminimiz_MWMOTE, reference = testHedefNitelik_MWMOTE, mode = "everything",positive="1")
NBtablomuz_MWMOTE

NBtablomuz_MWMOTE$overall[1]
NBtablomuz_MWMOTE$byClass[1]
NBtablomuz_MWMOTE$byClass[2]
NBtablomuz_MWMOTE$byClass[5]
NBtablomuz_MWMOTE$byClass[7]

## SVM_MWMOTE ##
set.seed(769)
SVMmodelimiz_MWMOTE<- svm(formula= motivation ~.,data=egitim_MWMOTE,type="C-classification",kernel="linear")
SVMtahminimiz_MWMOTE<-predict(SVMmodelimiz_MWMOTE, testNitelik_MWMOTE)
SVMtablomuz_MWMOTE<-confusionMatrix(data=SVMtahminimiz_MWMOTE,reference=testHedefNitelik_MWMOTE,mode="everything",positive = "1")
SVMtablomuz_MWMOTE
SVMtablomuz_MWMOTE$overall[1]
SVMtablomuz_MWMOTE$byClass[1]
SVMtablomuz_MWMOTE$byClass[2]
SVMtablomuz_MWMOTE$byClass[5]
SVMtablomuz_MWMOTE$byClass[7]


### C5.0_MWMOTE###

set.seed(353)
C50modelimiz_MWMOTE<-C5.0(egitimNitelik_MWMOTE, egitimHedefNitelik_MWMOTE)
C50tahminimiz_MWMOTE<-predict(C50modelimiz_MWMOTE,testNitelik_MWMOTE)
C50tablomuz_MWMOTE<-confusionMatrix(data=C50tahminimiz_MWMOTE, reference=testHedefNitelik_MWMOTE,mode="everything", positive="1")
C50tablomuz_MWMOTE
C50tablomuz_MWMOTE$overall[1]
C50tablomuz_MWMOTE$byClass[1]
C50tablomuz_MWMOTE$byClass[2]
C50tablomuz_MWMOTE$byClass[5]
C50tablomuz_MWMOTE$byClass[7]

### CART _MWMOTE###
set.seed(838)
CARTmodelimiz_MWMOTE<- rpart(motivation~., egitim_MWMOTE,method="class") 
#rpart.plot(agac11,type=3,digits=3,fallen.leaves = TRUE, cex=0.9)
CARTtahminimiz_MWMOTE<-predict(CARTmodelimiz_MWMOTE,testNitelik_MWMOTE,type="class")
CARTtablomuz_MWMOTE<-confusionMatrix(data=CARTtahminimiz_MWMOTE,reference=testHedefNitelik_MWMOTE,mode="everything", positive="1")
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
RFmodelimiz_MWMOTE<- randomForest(motivation~.,data=egitim_MWMOTE,proximity=TRUE)
RFtahminimiz_MWMOTE<-predict(RFmodelimiz_MWMOTE,testNitelik_MWMOTE,type="class")
RFtablomuz_MWMOTE<-confusionMatrix(data=RFtahminimiz_MWMOTE,reference=testHedefNitelik_MWMOTE,mode="everything",positive="1")
RFtablomuz_MWMOTE

varImpPlot(RFmodelimiz_MWMOTE, main="Random Forest\n Variable Importance Plot")

RFtablomuz_MWMOTE$overall[1]
RFtablomuz_MWMOTE$byClass[1]
RFtablomuz_MWMOTE$byClass[2]
RFtablomuz_MWMOTE$byClass[5]
RFtablomuz_MWMOTE$byClass[7]

############## LogisticRegression_MWMOTE ###########

set.seed(729)
LRmodelimiz_MWMOTE<- glm(motivation~.,family= "binomial", data=egitim_MWMOTE)

LRtahminimiz_MWMOTE<-predict(LRmodelimiz_MWMOTE,testNitelik_MWMOTE,type="response")
#predicted <- as.factor(ifelse(LRtahminimiz_Over> 0.5, 1, 0))
predicted <- as.factor(round(LRtahminimiz_MWMOTE))

LRtablomuz_MWMOTE<-confusionMatrix(data=predicted, reference=testHedefNitelik_MWMOTE,mode="everything",positive="1")

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
##########

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
