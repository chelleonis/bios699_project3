#data file for imputation

set.seed(123)
require(SurvELM)
require(survival)
#Lung DATA
data(lung)
lung=na.omit(lung)
lung[,3]=lung[,3]-1
n=dim(lung)[1]
# Divide the original dataset into training and test datasets
# In this sample code, 50% of the original data are used as training data 
# while the remaining 50% are used as test data
L=sample(1:n,ceiling(n*0.5))
trset<-lung[L,]
teset<-lung[-L,]
#Specify the indexes of survival times and censoring status
rii=c(2,3)
#A kernel ELM base model
kerelmsurv=ELMBJ(trset[,-rii],Surv(trset[,rii[1]],trset[,rii[2]]))
#The traing MSE
tr_mse=kerelmsurv$trainMSE
#New survival times imputed for training data
y_impute=kerelmsurv$newy

