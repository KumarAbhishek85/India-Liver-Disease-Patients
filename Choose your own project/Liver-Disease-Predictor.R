#import dat set from Github
indian_liver_patient <- read.csv("https://raw.githubusercontent.com/KumarAbhishek85/India-Liver-Disease-Patients/master/Choose%20your%20own%20project/indian_liver_patient.csv")
head(indian_liver_patient)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#study dimensions of data set
nrow(indian_liver_patient)
ncol(indian_liver_patient)

#visual analysis of data 
indian_liver_patient%>%group_by(Gender)%>%summarize(ratio= n()/nrow(.))
indian_liver_patient%>%group_by(Gender)%>%summarize(occurrence = mean(Dataset==1))
indian_liver_patient%>%group_by(Age)%>%summarize(occurrence = mean(Dataset==1))%>%ggplot(aes(Age,occurrence, col=occurrence))+geom_point()+stat_ellipse(type="norm")
indian_liver_patient%>%group_by(Age)%>%summarize(occurrence = mean(Dataset==1))%>%summarize(r=cor(Age, occurrence))
indian_liver_patient%>%group_by(Gender,Age)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Age,occurrence,group=Gender, col=Gender))+geom_point()+stat_ellipse(type="norm")
indian_liver_patient%>%group_by(Gender,Total_Bilirubin)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Total_Bilirubin,occurrence,col=Gender))+geom_point()
indian_liver_patient%>%group_by(Gender,Direct_Bilirubin)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Direct_Bilirubin,occurrence,col=Gender))+geom_point()
indian_liver_patient%>%group_by(Gender,Alkaline_Phosphotase)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Alkaline_Phosphotase,occurrence,col=Gender))+geom_point()
indian_liver_patient%>%group_by(Gender,Alamine_Aminotransferase)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Alamine_Aminotransferase,occurrence,col=Gender))+geom_point()
indian_liver_patient%>%group_by(Gender,Aspartate_Aminotransferase)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Aspartate_Aminotransferase,occurrence,col=Gender))+geom_point()
indian_liver_patient%>%group_by(Gender,Total_Protiens)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Total_Protiens,occurrence,col=Gender))+geom_point()+stat_ellipse(type="norm")
indian_liver_patient%>%group_by(Gender,Albumin)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Albumin,occurrence,col=Gender))+geom_point()+stat_ellipse(type="norm")
indian_liver_patient%>%group_by(Gender,Albumin_and_Globulin_Ratio)%>%summarize(occurrence=mean(Dataset==1))%>%ggplot(aes(Albumin_and_Globulin_Ratio,occurrence,col=Gender))+geom_point()+stat_ellipse(type="norm")
indian_liver_patient%>%summarize(r=cor(Total_Bilirubin,Alkaline_Phosphotase))
indian_liver_patient%>%summarize(r=cor(Total_Bilirubin,Albumin))
set.seed(1,sample.kind = "Rounding")

#Partition data into trainign and testing sets
index<-createDataPartition(indian_liver_patient$Dataset,times=1,p=0.15,list=FALSE)
train<-indian_liver_patient%>%slice(-index)
test<-indian_liver_patient%>%slice(index)

#create a vector of models to be applied
models<-c("glm", "knn", "qda","lda", "gamLoess", "rf")

#fit models to data set and set column names to model name
set.seed(1,sample.kind = "Rounding")
fits <- lapply(models, function(model){ 
  print(model)
  train(factor(Dataset) ~ ., method = model, data = train)
})
names(fits) <- models

#use fitted models to predict diagnosis
y_hat<-sapply(fits,function(fits){
  predict(fits,test,type = "raw")
})

#check dimensions of y_hat
tibble(nrow(y_hat),ncol(y_hat ))

#calculate ccauracies of all models
accuracy<-0
for (i in seq(1,6,by=1)){
  accuracy[i]= mean(factor(y_hat[,i])==factor(test$Dataset))
}
print(tibble(models,accuracy))

#Tune kNN model to find the optimum k
set.seed(1,sample.kind = "Rounding")
train_knn <- train(factor(Dataset) ~ ., method = "knn",
                   data = train, tuneGrid = data.frame(k = seq(1, 50, by=2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

#fit the optimum kNN model to data and check accuracy of model on test set
fit_knn<-knn3(factor(Dataset)~.,data=train,k=train_knn$bestTune)
y_hat_knn<-predict(fit_knn,test,type="class")
accuracy_knn<-mean(factor(y_hat_knn)==factor(test$Dataset))
accuracy_knn

#Tune random forest model using mtry
train_rf<-train(factor(Dataset)~.,method = "rf",data=train,tuneGrid=(data.frame(mtry=seq(1,10,by=2))))
ggplot(train_rf, highlight = TRUE)
train_rf$bestTune

#Fit the optimum vaue of mtry and check model's accuracy on test set
fit_rf<-train(factor(Dataset)~.,method="rf",data=train,tuneGrid=data.frame(mtry =train_rf$bestTune$mtry))
y_hat_rf<-predict(fit_rf,test,type="raw")
accuracy_rf<-mean(y_hat_rf==test$Dataset)
accuracy_rf

#importance based ranking of variables
varImp(fit_rf)

