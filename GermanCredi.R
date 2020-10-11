

install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("e1071")


library(readxl)


View(GermanCredit_assgt1_F18.xls)


mdData<-GermanCredit_assgt1_F18
View(mdData)

summary(mdData)
attributes(mdData)
str(mdData)


#Changing to Factor
cols <- c("RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES","NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING", "MALE_DIV", "MALE_SINGLE", "MALE_MAR_or_WID", "CO-APPLICANT","GUARANTOR","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","CHK_ACCT", "HISTORY", "SAV_ACCT", "EMPLOYMENT","PRESENT_RESIDENT","JOB")
mdData[cols] <- lapply(mdData[cols], factor)
sapply(mdData, class)

#Changing to Numeric values
colmn <- c("NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING")
mdData[colmn] <- lapply(mdData[colmn], as.numeric)
sapply(mdData, class)



library(rpart)
library(rpart.plot)
library(caret)
library(e1071)




#Developing Decision Tree Model
#Model 1
rpModel1=rpart(RESPONSE ~ ., data=mydata, method="class")
print(rpModel1)
rpart.plot::prp(rpModel1, type=2, extra=1)
summary(rpModel1)
plotcp(rpModel1)
pred1=predict(rpModel1, mydata, type='class')
table(pred = pred1, true = mydata$RESPONSE)
#confusionMatrix(pred1,mydata$RESPONSE) #Confisuion Matrix for Model 1
mean(pred1==mydata$RESPONSE)





#Accuracy : 78.7%
#Model2 - Based on Gini
rpmodel2 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'))
rpart.plot::prp(rpmodel2, type=2, extra=1)
summary(rpmodel2)
plotcp(rpmodel2)
pred2=predict(rpmodel2, mdData, type='class')
table(pred = pred2, true = mdData$RESPONSE)
confusionMatrix(pred2,mdData$RESPONSE) #Confisuion Matrix for Model 2
mean(pred2==mdData$RESPONSE)



#Accuracy 76.2%
#Model 4 - Based on information
rpmodel3 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='information'))
rpart.plot::prp(rpmodel3, type=2, extra=1)
summary(rpmodel3)
plotcp(rpmodel3)
pred3=predict(rpmodel3, mdData, type='class')
table(pred = pred3, true = mdData$RESPONSE)
confusionMatrix(pred2,mdData$RESPONSE) #Confisuion Matrix for Model 4
mean(pred3==mdData$RESPONSE)



#Accuracy : 76.2%
#Model 5 - Based on Gini and MinSplit 
rpmodel5 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(minsplit = 30
                                                                                                               ))
rpart.plot::prp(rpmodel5, type=2, extra=1)
summary(rpmodel5)
plotcp(rpmodel5)
pred5=predict(rpmodel5, mdData, type='class')
table(pred = pred5, true = mdData$RESPONSE)
confusionMatrix(pred5,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred5==mdData$RESPONSE)


#Model 6 Gini and maxdepth 78.7 %


rpmodel6 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(maxdepth = 8))

rpart.plot::prp(rpmodel6, type=2, extra=1)
summary(rpmodel6)
plotcp(rpmodel6)
pred6=predict(rpmodel6, mdData, type='class')
table(pred = pred6, true = mdData$RESPONSE)
confusionMatrix(pred6,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred6==mdData$RESPONSE)

#Model 7 76.2 %   Gini and CP value and MaxDepth


rpmodel7 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(cp=.01,maxdepth = 25))

rpart.plot::prp(rpmodel7, type=2, extra=1)
summary(rpmodel7)
plotcp(rpmodel7)
pred7=predict(rpmodel7, mdData, type='class')
table(pred = pred7, true = mdData$RESPONSE)
confusionMatrix(pred7,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred7==mdData$RESPONSE)


#Model 8 
#minsplit = 30, maxdepth = 5, minbucket = 10
#Accuracy 76.2
rpmodel8 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(minsplit = 30,
                                                                                                               maxdepth = 5,maxsurrogate = 0,xval = 2, minbucket = 10))
rpart.plot::prp(rpmodel8, type=2, extra=1)
summary(rpmodel8)
plotcp(rpmodel8)
pred8=predict(rpmodel8, mdData, type='class')
table(pred = pred8, true = mdData$RESPONSE)
confusionMatrix(pred8,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred8==mdData$RESPONSE)



#Model 9
#CP = .01
#Accuracy = 72%
rpmodel8 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.01))
                                                                                                              
rpart.plot::prp(rpmodel8, type=2, extra=1)
summary(rpmodel8)
plotcp(rpmodel8)
pred8=predict(rpmodel8, mdData, type='class')
table(pred = pred8, true = mdData$RESPONSE)
confusionMatrix(pred8,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred8==mdData$RESPONSE)

#Lift Curve & ROCR

install.packages('ROCR')
library(ROCR)
#score test data set
mdData$score<-predict(rpmodel6,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="red")

lift1_1<-performance(pred,"lift","rpp")
plot(lift1_1,main="Lift Curve",colorize=F)


#QNO 6

#Trying to find best Tree Model

trnindex <- sample(1:nrow(newdata), size=round(nrow(newdata)*0.5), replace=F)
TrnData <- newdata[trnindex,]
TstData <- newdata[-trnindex,]
dim(TrnData)
dim(TstData)

#develop a tree on the training data
library(rpart)
rpart.fit <- rpart(RESPONSE ~ ., data = TrnData, method = "class")
predTst <- predict(rpart.fit, TstData, type = "class")

#Confusion table
table(pred = predTst, true = TstData$RESPONSE)
#Confisuion Matrix
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(predTst,TstData$RESPONSE)

#Accuracy
mean(predTst==TstData$RESPONSE)


PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel6,mdTst, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, mdTst$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`mdTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))








