setwd("/Users/microsoft/Desktop/prog.ml")
dati<-read.csv("kyliej.csv")
str(dati)
dati<-dati[dati$videoViewCount>0,]
dati$y<-dati$likesCount/dati$videoViewCount
dati$date<-substr(dati$timestamp,1,10)
str(dati)
dati$data <- as.Date(dati$date, "%Y-%m-%d")
dati$age<-round(difftime("2023-12-29", dati$data,units="days"),0)
dati$ratio<-dati$commentsCount/dati$videoViewCount

plot(dati$y~dati$age)
plot(dati$y~dati$videoViewCount)
plot(dati$y~dati$videoViewCount, xlim=c(0, 50000000))
plot(dati$y~dati$videoDuration)


boxplot(dati$y)

cor(dati[,c(1,2,3,5,6,10)])

# creazione dataset train e test  -----------------------------------------

n=nrow(dati)
n
test=dati[1:(n/4),]
train=dati[(n/4):n,]



# regressione lineare -----------------------------------------------------


lm1<-lm(y~videoViewCount+videoDuration+age+ratio, data=train)
summary(lm1)
pred_lm<-predict(lm1, newdata=test)
mse_lm<-mean((test$y-pred_lm)^2)
mse_lm


# albero di regressione ---------------------------------------------------


train2<-train[1:95,]
train1<-train[96:284,]
library(tree)
tree1<-tree(y~videoViewCount+videoDuration+age+ratio, data=train1)

plot(tree1)
text(tree1)# aggiunge i nomi delle variabili
prune.t1<-prune.tree(tree1,newdata=train2)
plot(prune.t1)
# questo grafico mi da l'andamento della devianza rispetto alla dimensione dell'albero


# potatura
mt1<-prune.tree(tree1, best=2)
# sto dicendo potami l'albero  basando come criterio l'avere 2 foglie
plot(mt1)
text(mt1)
pred_tree<-predict(mt1, newdata=test)
mse_tree<-mean((test$y-pred_tree)^2)
mse_tree
  

# random forest -----------------------------------------------------------


library(randomForest)
err=rep(0,3)
  set.seed(555)
for (i in 1:4){
  rf<- randomForest(y~videoViewCount+videoDuration+age+ratio, data=train1,nodesize=1, mtry=i)  
  predrf=predict(rf, newdata=train2)
  err[i] <- mean((train2$y-predrf)^2)
  cat(i, "")
}
  err
  
set.seed(123)  
rf<- randomForest(y~videoViewCount+videoDuration+age+ratio, data=train1,nodesize=1, mtry=3)  
pred_rf  =predict(rf, newdata=test)
mse_rf <- mean((test$y-pred_rf)^2)
mse_lm
mse_tree
mse_rf


# svr####
library(e1071)
set.seed(123)
svr<-svm(y~videoViewCount+videoDuration+age+ratio, data=train)
pred_svr<-predict(svr, newdata=test)
mse_svr<-mean((test$y-pred_svr)^2)

mse_svr


# xgboost -----------------------------------------------------------------


dataset<-train[, c(3,5,9,10)] 
dataset$age<- as.numeric(dataset$age)
mat<- as.matrix(dataset)
dataset_test<-test[,c(3,5,9,10)]
dataset_test$age<-as.numeric(dataset_test$age)
mat_test<-as.matrix(dataset_test)
library(xgboost)
set.seed(123)
m1_xgb <-
  xgboost(
    data = mat,
    label = train[, 6],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  ) 


pred_xgb<-predict(m1_xgb, newdata=mat_test)
mse_xgb<-mean((test$y-pred_xgb)^2)
mse_xgb



# confronto metriche ------------------------------------------------------

mse_lm
mse_tree
mse_rf
mse_svr
mse_xgb
mse_value=c(mse_lm,
            mse_tree,
            mse_rf,
            mse_svr,
            mse_xgb)
names(mse_value)=c("lm", "tree", "rf", "svr", "xgb")
barplot(mse_value)


rmse_value=sqrt(mse_value)
barplot(rmse_value)

boxplot(dati$y)
boxplot(test$y)



# valutazione predizioni --------------------------------------------------

 
plot(test$y~test$age, pch=20)
points(pred_svr~test$age, col="red", pch=20)
points(pred_rf~test$age, col="green", pch=20)
points(pred_tree~test$age, col="blue", pch=20)
points(pred_lm~test$age, col="orange", pch=20)
points(pred_xgb~test$age, col="purple", pch=20)




plot(test$y~test$videoViewCount, pch=20)
points(pred_svr~test$videoViewCount, col="red", pch=20)
points(pred_rf~test$videoViewCount, col="green", pch=20)
points(pred_tree~test$videoViewCount, col="blue", pch=20)
points(pred_lm~test$videoViewCount, col="orange", pch=20)
points(pred_xgb~test$videoViewCount, col="purple", pch=20)



plot(test$y~test$ratio, pch=20)
points(pred_svr~test$ratio, col="red", pch=20)
points(pred_rf~test$ratio, col="green", pch=20)
points(pred_tree~test$ratio, col="blue", pch=20)
points(pred_lm~test$ratio, col="orange", pch=20)
points(pred_xgb~test$ratio, col="purple", pch=20)

