
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code
credit<-read_excel(file.choose())
sum(is.na(credit))
str(credit)

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
#data cleaning
credit$SEX <- as.factor(credit$SEX)
credit$EDUCATION<- as.factor(credit$EDUCATION)
credit$MARRIAGE <- as.factor(credit$MARRIAGE)
credit$PAY_1 <- as.factor(credit$PAY_1)
credit$PAY_2 <- as.factor(credit$PAY_2)
credit$PAY_3 <- as.factor(credit$PAY_3)
credit$PAY_4 <- as.factor(credit$PAY_4)
credit$PAY_5 <- as.factor(credit$PAY_5)
credit$PAY_6 <- as.factor(credit$PAY_6)
credit$default_0<-as.factor(credit$default_0)
credit$AGE<-scale(credit$AGE)
credit$BILL_AMT1<-scale(credit$BILL_AMT1)
credit$BILL_AMT2<-scale(credit$BILL_AMT2)
credit$BILL_AMT3<-scale(credit$BILL_AMT3)
credit$BILL_AMT4<-scale(credit$BILL_AMT4)
credit$BILL_AMT5<-scale(credit$BILL_AMT5)
credit$BILL_AMT6<-scale(credit$BILL_AMT6)

credit$PAY_AMT1<-scale(credit$PAY_AMT1)
credit$PAY_AMT2<-scale(credit$PAY_AMT2)
credit$PAY_AMT3<-scale(credit$PAY_AMT3)
credit$PAY_AMT4<-scale(credit$PAY_AMT4)
credit$PAY_AMT5<-scale(credit$PAY_AMT5)
credit$PAY_AMT6<-scale(credit$PAY_AMT6)

credit$LIMIT_BAL<-scale(credit$LIMIT_BAL)


#Feature Engineering

credit$Amount_Owed<-credit$BILL_AMT1+credit$BILL_AMT2+
  credit$BILL_AMT3+credit$BILL_AMT4+credit$BILL_AMT5+
  credit$BILL_AMT6-credit$PAY_AMT1-credit$PAY_AMT2-credit$PAY_AMT3-
  credit$PAY_AMT4-credit$PAY_AMT5-credit$PAY_AMT6

credit$AVG_Amount_Owed<-credit$Amount_Owed/6

credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_1)) >=1,1,0)
credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_2)) >=1,credit$Payments_Missed+1,credit$Payments_Missed)
credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_3)) >=1,credit$Payments_Missed+1,credit$Payments_Missed)
credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_4)) >=1,credit$Payments_Missed+1,credit$Payments_Missed)
credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_5)) >=1,credit$Payments_Missed+1,credit$Payments_Missed)
credit$Payments_Missed<- ifelse(as.numeric(as.character(credit$PAY_6)) >=1,credit$Payments_Missed+1,credit$Payments_Missed)

credit$BalLim<- ((credit$BILL_AMT1+credit$BILL_AMT2+credit$BILL_AMT3+credit$BILL_AMT4+credit$BILL_AMT5+credit$BILL_AMT6)/6)/credit$LIMIT_BAL

## Predict test file
new<-read_excel(file.choose())
sum(is.na(new))
str(new)

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
#data cleaning
new$SEX <- as.factor(new$SEX)
new$EDUCATION<- as.factor(new$EDUCATION)
new$MARRIAGE <- as.factor(new$MARRIAGE)
new$PAY_1 <- as.factor(new$PAY_1)
new$PAY_2 <- as.factor(new$PAY_2)
new$PAY_3 <- as.factor(new$PAY_3)
new$PAY_4 <- as.factor(new$PAY_4)
new$PAY_5 <- as.factor(new$PAY_5)
new$PAY_6 <- as.factor(new$PAY_6)
new$AGE<-scale(new$AGE)
new$BILL_AMT1<-scale(new$BILL_AMT1)
new$BILL_AMT2<-scale(new$BILL_AMT2)
new$BILL_AMT3<-scale(new$BILL_AMT3)
new$BILL_AMT4<-scale(new$BILL_AMT4)
new$BILL_AMT5<-scale(new$BILL_AMT5)
new$BILL_AMT6<-scale(new$BILL_AMT6)

new$PAY_AMT1<-scale(new$PAY_AMT1)
new$PAY_AMT2<-scale(new$PAY_AMT2)
new$PAY_AMT3<-scale(new$PAY_AMT3)
new$PAY_AMT4<-scale(new$PAY_AMT4)
new$PAY_AMT5<-scale(new$PAY_AMT5)
new$PAY_AMT6<-scale(new$PAY_AMT6)

new$LIMIT_BAL<-scale(new$LIMIT_BAL)


#Feature Engineering

new$Amount_Owed<-new$BILL_AMT1+new$BILL_AMT2+
  new$BILL_AMT3+new$BILL_AMT4+new$BILL_AMT5+
  new$BILL_AMT6-new$PAY_AMT1-new$PAY_AMT2-new$PAY_AMT3-
  new$PAY_AMT4-new$PAY_AMT5-new$PAY_AMT6

new$AVG_Amount_Owed<-new$Amount_Owed/6

new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_1)) >=1,1,0)
new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_2)) >=1,new$Payments_Missed+1,new$Payments_Missed)
new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_3)) >=1,new$Payments_Missed+1,new$Payments_Missed)
new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_4)) >=1,new$Payments_Missed+1,new$Payments_Missed)
new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_5)) >=1,new$Payments_Missed+1,new$Payments_Missed)
new$Payments_Missed<- ifelse(as.numeric(as.character(new$PAY_6)) >=1,new$Payments_Missed+1,new$Payments_Missed)

new$BalLim<- ((new$BILL_AMT1+new$BILL_AMT2+new$BILL_AMT3+new$BILL_AMT4+new$BILL_AMT5+new$BILL_AMT6)/6)/new$LIMIT_BAL

###
### Random Forest
###

model_forest <- randomForest(default_0~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 

plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot

varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values


###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",1000)
forest_classification[forest_probabilities[,2]<0.22108]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)
