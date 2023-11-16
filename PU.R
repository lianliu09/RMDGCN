library(xgboost)
library(caret)
library(pROC)


d_R_matrix=read.csv("/A.csv",header=F)
R=read.csv("/RNAf.csv",header=F)
d_dose=read.csv("/d_d.csv",header=F)
d_sy=read.csv("/d_d.csv",header=F)
label=as.vector(t(as.matrix(d_R_matrix)))

RR=R[rep(1:nrow(R),each=116),]
dd=d_dose[rep(1:nrow(d_dose),3618),]
feature=cbind(RR,dd)
data=cbind(feature,label)
colnames(data)<-c(1:116,"Target_label")
result=matrix(0,419688,100)
p_index=which(data$Target_label==1)
n_index=which(data$Target_label==0)
for(i in 1:100){
  print(paste(i,"-th test"))
  n_train_index=sample(n_index,length(p_index))
  train_index=c(p_index,n_train_index)
  train=data[train_index,]
  label <- train$Target_label
  test=data[-train_index,]
  train <- model.matrix(Target_label~.,train)[,-1]
  
  test <- model.matrix(Target_label~.,test)[,-1]
  xgb.fit = xgboost(data = train, label =  label, nrounds = 10 ,
                    objective = "binary:logistic",max_depth = 10, eta = 1,eval_metric ="auc")
  classification = predict(xgb.fit, test)
  pro=predict(xgb.fit, test)
  result[p_index,i]=1
  result[n_train_index]=0
  result[-train_index,i]=pro
}

prob=rowMeans(result)
probability=t(matrix(prob,ncol=3618,nrow=116))

