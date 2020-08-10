
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")  #Check, and if needed install the necessary packages

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code
bank<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE) # Load the datafile to R

str(bank) # See if some data types were misclassified when importing data from CSV

# Fixing incorrectly classified data types:
bank$SEX <- as.factor(bank$SEX)
bank$EDUCATION<- as.factor(bank$EDUCATION)
bank$MARRIAGE <- as.factor(bank$MARRIAGE)
bank$PAY_1 <- as.factor(bank$PAY_1)
bank$PAY_2 <- as.factor(bank$PAY_2)
bank$PAY_3 <- as.factor(bank$PAY_3)
bank$PAY_4 <- as.factor(bank$PAY_4)
bank$PAY_5 <- as.factor(bank$PAY_5)
bank$PAY_6 <- as.factor(bank$PAY_6)
bank$default_0<-as.factor(bank$default_0)
bank$AGE<-scale(bank$AGE)
bank$BILL_AMT1<-scale(bank$BILL_AMT1)
bank$BILL_AMT2<-scale(bank$BILL_AMT2)
bank$BILL_AMT3<-scale(bank$BILL_AMT3)
bank$BILL_AMT4<-scale(bank$BILL_AMT4)
bank$BILL_AMT5<-scale(bank$BILL_AMT5)
bank$BILL_AMT6<-scale(bank$BILL_AMT6)

bank$PAY_AMT1<-scale(bank$PAY_AMT1)
bank$PAY_AMT2<-scale(bank$PAY_AMT2)
bank$PAY_AMT3<-scale(bank$PAY_AMT3)
bank$PAY_AMT4<-scale(bank$PAY_AMT4)
bank$PAY_AMT5<-scale(bank$PAY_AMT5)
bank$PAY_AMT6<-scale(bank$PAY_AMT6)

bank$LIMIT_BAL<-scale(bank$LIMIT_BAL)


#Feature Engineering

bank$Amount_Owed<-bank$BILL_AMT1+bank$BILL_AMT2+
  bank$BILL_AMT3+bank$BILL_AMT4+bank$BILL_AMT5+
  bank$BILL_AMT6-bank$PAY_AMT1-bank$PAY_AMT2-bank$PAY_AMT3-
  bank$PAY_AMT4-bank$PAY_AMT5-bank$PAY_AMT6

bank$AVG_Amount_Owed<-bank$Amount_Owed/6

bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_1)) >=1,1,0)
bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_2)) >=1,bank$Payments_Missed+1,bank$Payments_Missed)
bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_3)) >=1,bank$Payments_Missed+1,bank$Payments_Missed)
bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_4)) >=1,bank$Payments_Missed+1,bank$Payments_Missed)
bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_5)) >=1,bank$Payments_Missed+1,bank$Payments_Missed)
bank$Payments_Missed<- ifelse(as.numeric(as.character(bank$PAY_6)) >=1,bank$Payments_Missed+1,bank$Payments_Missed)

bank$BalLim<- ((bank$BILL_AMT1+bank$BILL_AMT2+bank$BILL_AMT3+bank$BILL_AMT4+bank$BILL_AMT5+bank$BILL_AMT6)/6)/bank$LIMIT_BAL

#Apply combinerarecategories function to the data and then split it into testing and training data.

table(bank$Group.State)# check for rare categories

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

bank<-combinerarecategories(bank,20) #combine categories with <10 values in STCdata into "Other"


set.seed(1) #set a random number generation seed to ensure that the split is the same everytime


inTrain <- createDataPartition(y = bank$default_0,
                               p = 19200/24000, list = FALSE) #80/20
training <- bank[ inTrain,]
testing <- bank[ -inTrain,]
###
### Support Vector Machines
###

model_svm <- svm(default_0 ~., data=training, probability=TRUE)
summary(model_svm)

svm_probabilities<-attr(predict(model_svm,newdata=testing, probability=TRUE), "prob")
svm_prediction<-svm_probabilities[,1]

svm_classification<-rep("1",1000)
svm_classification[svm_prediction<0.22108]="0" 
svm_classification<-as.factor(svm_classification)
confusionMatrix(svm_classification,testing$default_0,positive = "1")

####ROC Curve
svm_ROC_prediction <- prediction(svm_prediction, testing$default_0) #Calculate errors
svm_ROC_testing <- performance(svm_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(svm_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(svm_ROC_prediction,"auc") #Create AUC data
svm_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
svm_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(svm_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_svm <- performance(svm_ROC_prediction,"lift","rpp")
plot(Lift_svm)
