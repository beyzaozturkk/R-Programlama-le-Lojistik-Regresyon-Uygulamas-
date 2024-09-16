########### Logistic Regresyon #########################
diabetes<-read.csv('diabetes.csv' , header = TRUE , sep = "," , dec = ".")
View(diabetes)

#Pregnancies (Hamilelik sayısı): Kadının geçirdiği hamilelik sayısı.
#Glucose (Glukoz): Oral glukoz tolerans testinde ölçülen kan şekeri seviyesi (mg/dL).
#BloodPressure (Kan basıncı): Diyastolik kan basıncı (mm Hg).
#SkinThickness (Cilt kalınlığı): Triceps cilt kalınlığı (mm) ile vücut yağının bir göstergesi.
#Insulin: 2 saatlik serum insülin seviyesi (mu U/ml).
#BMI (Beden Kitle İndeksi): Ağırlık ve boy oranına dayalı bir obezite ölçütü (kg/m²).
#DiabetesPedigreeFunction: Ailedeki diyabet öyküsüne dayalı bir risk fonksiyonu.
#Age (Yaş): Kişinin yaşı (yıl olarak).
#Outcome (Sonuç): Hastanın diyabet olup olmadığını gösteren ikili bir değişken (0: Hayır, 1: Evet).
#Bu değişkenler kullanılarak, kişinin diyabet riski tahmin edilmeye çalışılır.

library(caret)
library(glmnet)
library(tidyverse)

table(diabetes$Outcome) # diabet hastası olanlar(1) ve olmayanlar (0) için frekans dağılımı

#diabet hastası olanlar ve olmayanlar arasındaki frekanslarda dengesiz bir durum var.
#yaklaşık olarak eşit olmaları gerekir.

Diabetes <- diabetes %>% filter(Outcome == "1")
NotDiabets <- diabetes %>% filter(Outcome == "0")

nrow(Diabetes) #diabet hastası olanların sayısı
nrow(NotDiabets) #diabet hastası olmayanların sayısı

set.seed(123)

#Diabetes(1) olanların frekansı düşük olduğundan veri bölme işlemi Diabetes üzerinden yapılır.

Diabetes_index<- sample(1:nrow(Diabetes) , size = 0.80*nrow(Diabetes) )
set.seed(123)
NotDiabets_index <- sample(1:nrow(Diabetes) , size = 0.80*nrow(Diabetes) )

#trainset oluşturma

trainSet <- Diabetes[ Diabetes_index , ]
trainNotSet <- NotDiabets[ NotDiabets_index  , ]


train_set<- rbind(trainSet , trainNotSet)

table(train_set$Outcome) # frekanslar eşitlendi



#test set oluşturmak için,

testSet <- Diabetes[ -Diabetes_index , ]
testNotSet <- NotDiabets[ -NotDiabets_index  , ]


test_set<- rbind(testSet , testNotSet)

table(test_set$Outcome) # 0.20lik kısım test set değeri olur



## GLM ile Logistic Regresyon Modeli Oluşturma (train_set kullanılır)


# modelLogit <- glm(status ~ . , data = trainSet , family = binomial(link = "logit"))

#family=binomial olması 0 ve 1 olarak 2 olası değere sahip olduğu için

model<- glm(Outcome ~ . , data = train_set , family = "binomial")
model
summary(model)

### ANOVA Değişken Deviance Değerleri ####

#Hangi değişkenin ne kadar deviance etkisi oluşturduğunu kontrol ederiz. 
#Değişkenler eklendikçe Resid.Dev değerlerinde düşme bekleriz.
#Eğer eklenen değişken bir önceki Resid. Dev değerinde fazla bir düşme etkisi oluşturmazsa değişkenin çok da bir etkisi yoktur denir. 

anova(model)
summary(model)


##variable Importance ####

#Değişkenlerin önem derecelerini verir. 

varImp(model)


### Model Üzerinden Tahminler (test_set kullanılır)

library(caret)

#type=response sigmoid fonksiyon oluşturarak 0 ile 1 arasında değerler yapıyoruz
#Değerler olasılık değeri olarak gelir

predictions1 <- predict(model , test_set , type = "response")
predictions1
#predictions2 <- plogis(predict(modelLogit , testSet)) kodu da kullanılabilir.

# Gerçek sınıflar faktör formatına dönüştürülüyor
test_set$Outcome <- as.factor(test_set$Outcome )

# Olasılık tahminlerini 0.5 eşik değeri ile ikili sınıfa dönüştürme
predictions1_class <- ifelse(predictions1 > 0.5, 1, 0)

# Tahmin edilen sınıflar da faktör formatına dönüştürülüyor
predictions1_class <- as.factor(predictions1_class)

# Confusion Matrix oluEturma

#Yukarıdaki olasılık hesaplarına göre gerçek değerlerle tahminlerin durumuna bakılır
cm <- confusionMatrix(predictions1_class, test_set$Outcome)
cm

# Accuracy (Doğruluk)

confusion_table <- cm$table
confusion_table

accur <- (confusion_table[1, 1] + confusion_table[2, 2]) / sum(confusion_table)
accur# modelin doğruluk oranıdır. DoDğruluk performansı.

errorRate<-(confusion_table[1, 2] + confusion_table[2, 1]) / sum(confusion_table)
errorRate #hata oranına bakılır. 

## Precision ve Recall Değerlerinin Hesaplanması 

library(PerformanceAnalytics)
library(caret)

sensitivity(confusion_table)

specificity(confusion_table)

### Optimal Cutoff value en iyi accuracy veren değerdir.


# pROC paketini yükleyin

library(pROC)

# ROC eğrisini oluşturma

roc_curve <- roc(test_set$Outcome, predictions1)
## AUC Değeri ve ROC modeli

roc_curve #eğri altında kalan alanı verir.

#sensitivity=1-specificity olunca specificity 0'a giderse sensitivity 1' yaklaşır


optCutoff <- coords(roc_curve, "best", ret = "threshold")
optCutoff

# Control = negative class
# Case  = positive class

plot(roc_curve)

# Olasılık tahminlerini 0.5 eşik değeri ile ikili sınıfa dönüştürme
predictions_class <- ifelse(predictions1 > 0.36, 1, 0)

# Tahmin edilen sınıflar da faktör formatına dönüştürülüyor
predictions_class <- as.factor(predictions_class)

# Confusion Matrix oluşturma

#Yukarıdaki olasılık hesaplarına göre gerçek değerlerle tahminlerin durumuna bakılır
cmp <- confusionMatrix(predictions_class, test_set$Outcome)
cmp
cm
# Accuracy (Doğruluk)

confusion_table_p <- cmp$table
confusion_table_p 

accur_p <- (confusion_table_p[1, 1] + confusion_table_p[2, 2]) / sum(confusion_table_p)
accur_p# modelin doğruluk oranıdır. Doğruluk performansı azalmıştır.
errorRate_p<-(confusion_table_p[1, 2] + confusion_table_p[2, 1]) / sum(confusion_table_p)
errorRate_p #hata oranına bakılır. 

confusion_table
confusion_table_p 

optCutoff1<-0.36
predictions1
table(test_set$Outcome)

predictedClass <- ifelse(predictions1 > 0.36 , 1 , 0)
predictedClass <- as.factor(predictedClass)

## Positive class varsayılan olarak ilk class 0'dır
confusionMatrix(predictedClass ,test_set$Outcome )

## Positive class değiştirme 
confusionMatrix(predictedClass ,test_set$Outcome , positive ="1")


## Recall ve Precision ve F1 metriklerini ekleme
confusionMatrix(predictedClass , test_set$Outcome , 
                positive = "1" , mode = "prec_recall")

## Conf. mat. kaydetme ve içindeki değerlere erişme 
cmOpt_1_caret <- caret::confusionMatrix(predictedClass , reference =test_set$Outcome  , 
                                        positive = "1" , mode = "prec_recall")

cmOpt_1_caret$byClass[1]


### Kappa İstatistiği ve McNemar Test İstatistiği

confusionMatrix(predictedClass , reference = test_set$Outcome, positive = "1")

## Kappa 0.1 - 0.2 - Kötü
## Kappa 0.2 - 0.4 - Kötü idare eder 
## Kappa 0.4 - 0.6 - Orta
## Kappa 0.6 - 0.8 - İyi
## Kappa 0.8 - 1.0 - Mükemmel

## Mcnemar p-value > 0.05 Tahmin edilen ve gerçek değerler 
## birbirine benzer ilişkili. (Yani iyi bir model) denir.
#H0: Değerler birbirine benzerdir.


#####Regularization Yöntemleri İle Model Tuning: modelin complexitiysini azaltmak için yapılır.

library(glmnet) #regularization yapmak lazım. 
library(tidyverse)

summary(model) #anlamsız değişkenler çıkartılarak da model yaılabilir

head(diabetes)


### Regularization Adımları

library(glmnet)

y = train_set$Outcome #bağımlı değişkenimiz

ncol(train_set)
X = train_set[, -9] # bağımsız değişkenleri alacağımıdan bağımlı değişkeni çıkarırız
X = as.matrix(X) #matrise Cçeviririz



##cross validation ile uygun alpha değerini bulmak için 

#alpha=1 yaptığımızdan lasso regresyon olur

modelLassoCV <- cv.glmnet(X , y , alpha = 1 ,  family = "binomial" )
modelLassoCV


modelLassoCV$glmnet.fit# deviance ratio= null dev-residual dev
plot(modelLassoCV) #binomial deviance gösterir düşük olması beklenir. lambda değerleri ondalık değer ama log alınınca eksi değerler olur

modelLassoCV$lambda.min
coef(modelLassoCV , modelLassoCV$lambda.min ) #katsayılara bakalım
summary(model) # en başta oluşturulan model


## Model Üzerinden Tahminler 

#bulduğumuz lambda değerini veririz

modelLasso <- glmnet(X , y , alpha = 1 , lambda = modelLassoCV$lambda.min, 
                     family = "binomial")
modelLasso
model

modelLasso$nulldev
deviance(modelLasso) 

testControl <- as.matrix(test_set[, -9])
head(testControl)
actuals <- test_set[,9]

# predictionsLasso <- predict(modelLogitLasso , testControl)
predictionsLasso <- predict(modelLasso , testControl , type="response") # Skorlar elde edilir
predictionsLasso
predictionsLasso<-as.factor(predictionsLasso)
roc_curve1 <- roc(test_set$Outcome, predictionsLasso)
## AUC Değeri ve ROC modeli

roc_curve1 #eğri altında kalan alanı verir.

#sensitivity=1-specificity olunca specificity 0'a giderse sensitivity 1' yaklaşır


optCutoff1 <- coords(roc_curve1, "best", ret = "threshold")
optCutoff1

# Control = negative class
# Case  = positive class

plot(roc_curve1)

#gerçek değerler 1 ise Diabetes değilse NotDiabetes ata 
actualsLabeled <- ifelse(actuals == 1 , "Diabetes" , "NotDiabetes")

preditionsLabeled <- ifelse(predictionsLasso[,1] > 0.40 , "Diabetes" , "NotDiabetes")

library(caret)
caret::confusionMatrix(as.factor(preditionsLabeled) , as.factor(actualsLabeled) , 
                       positive = "Diabetes")

# Eski Model Lasso yok
confusionMatrix(predictedClass , test_set$Outcome,positive = "0")

# Yeni modell Lasso var
confusionMatrix(as.factor(preditionsLabeled) , as.factor(actualsLabeled),positive = "NotDiabetes")

# Eski Model Lasso yok
confusionMatrix(predictedClass ,test_set$Outcome , positive ="0" ,
                mode = "prec_recall")

# Yeni modell Lasso var
confusionMatrix(as.factor(preditionsLabeled) , as.factor(actualsLabeled) , 
                positive = "NotDiabetes" , mode = "prec_recall")

# Eski Model Lasso yok
confusionMatrix(predictedClass , test_set$Outcome,positive = "1")

# Yeni modell Lasso var
confusionMatrix(as.factor(preditionsLabeled) , as.factor(actualsLabeled),positive = "Diabetes")

# Eski Model Lasso yok
confusionMatrix(predictedClass ,test_set$Outcome , positive ="1" ,
                       mode = "prec_recall")

# Yeni modell Lasso var
confusionMatrix(as.factor(preditionsLabeled) , as.factor(actualsLabeled) , 
                       positive = "Diabetes" , mode = "prec_recall")









