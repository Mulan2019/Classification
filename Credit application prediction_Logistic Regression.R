
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages

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


# Select the variables to be included in the "base-case" model

model_logistic<-glm(default_0~., data=subset(credit, select=-c( ID )), family="binomial"(link="logit"))
summary(model_logistic) 
# to add surrogates paste this to the list of variables; note, it will run quite a bit slower
#Special.Pay_surrogate + Early.RPL_surrogate + Latest.RPL_surrogate + 
#Initial.System.Date_surrogate + CRM.Segment_surrogate + MDR.High.Grade_surrogate + 
#Total.School.Enrollment_surrogate + FirstMeeting_surrogate + 
#LastMeeting_surrogate + DifferenceTraveltoFirstMeeting_surrogate + 
#DifferenceTraveltoLastMeeting_surrogate + FPP.to.School.enrollment_surrogate

##The model clearly has too many variables, most of which are insignificant 

## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both"
## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
frequency(credit,  default_0 == 1)
new_probabilities<-predict(model_logistic_stepwiseAIC,newdata=new,type="response") #Predict probabilities
new_classification<-rep("1",1000)
new_classification[new_probabilities<0.221083]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))


#export
write.csv(new_classification,file="new application prediction.csv")
getwd()
