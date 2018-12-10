#model 4 - Remove MaritalStatus and StockOptionLevel, Add YearsWithCurrManager and JobLevel - best so far
#DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$YearsInCurrentRole, DFTrainRel$YearsWithCurrManager, DFTrainRel$JobLevel  
New_Naive_Bayes_Model4 <- naiveBayes(Attrition ~ OverTime + YearsAtCompany + YearsInCurrentRole + YearsWithCurrManager + JobLevel,data = DFTrain)
New_Naive_Bayes_Model4
New_NB_Prediction4 <- predict(New_Naive_Bayes_Model4, DFTrain[,c('OverTime','YearsAtCompany','YearsInCurrentRole','YearsWithCurrManager','JobLevel')])
confusionMatrix(table(New_NB_Prediction4,DFTrain$Attrition))
Test_Data_NB_Pred4 <- predict(New_Naive_Bayes_Model4, DFTest[,c('OverTime','YearsAtCompany','YearsInCurrentRole','YearsWithCurrManager','JobLevel')])
confusionMatrix(table(Test_Data_NB_Pred4,DFTest$Attrition))
#Test_Data_NB_Pred4  
#No Yes
# No  243  31
# Yes   8  18
# Accuracy : 0.87            
# 95% CI : (0.8266, 0.9059)
# No Information Rate : 0.8367          
# P-Value [Acc > NIR] : 0.065830        
# Kappa : 0.4136          
# Mcnemar's Test P-Value : 0.000427        
# Sensitivity : 0.9681          
# Specificity : 0.3673          
# Pos Pred Value : 0.8869          
# Neg Pred Value : 0.6923          
# Prevalence : 0.8367          
# Detection Rate : 0.8100          
# Detection Prevalence : 0.9133          
# Balanced Accuracy : 0.6677

New_Naive_Bayes_Model5 <- naiveBayes(Attrition ~ OverTime + YearsAtCompany + YearsInCurrentRole + JobLevel + Single + EnvironmentSatisfaction,data = DFTrain)
New_Naive_Bayes_Model5
New_NB_Prediction5 <- predict(New_Naive_Bayes_Model5, DFTrain[,c('OverTime','YearsAtCompany','YearsInCurrentRole','JobLevel', 'Single', 'EnvironmentSatisfaction')])
confusionMatrix(table(New_NB_Prediction5,DFTrain$Attrition))
Test_Data_NB_Pred5 <- predict(New_Naive_Bayes_Model5, DFTest[,c('OverTime','YearsAtCompany','YearsInCurrentRole','JobLevel', 'Single', 'EnvironmentSatisfaction')])
confusionMatrix(table(Test_Data_NB_Pred5,DFTest$Attrition))
#Test_Data_NB_Pred5  No Yes
# No  236  30
# Yes  15  19
# Accuracy : 0.85            
# 95% CI : (0.8045, 0.8884)
# No Information Rate : 0.8367          
# P-Value [Acc > NIR] : 0.29662         
# Kappa : 0.3741          
# Mcnemar's Test P-Value : 0.03689         
# Sensitivity : 0.9402          
# Specificity : 0.3878          
# Pos Pred Value : 0.8872          
# Neg Pred Value : 0.5588          
# Prevalence : 0.8367          
# Detection Rate : 0.7867          
# Detection Prevalence : 0.8867          
# Balanced Accuracy : 0.6640







#I'm not supposed to need to set continuous as factors, but it seems to cut off the later variables if I don't
#Set Survey Integers as Factors to run auto NB function
#Create New Dataset to Play with
DFTranB <- DFTrain
DFTranB$Education <- as.factor(DFTranB$Education)
DFTranB$EnvironmentSatisfaction <- as.factor(DFTranB$EnvironmentSatisfaction)
DFTranB$JobInvolvement <- as.factor(DFTranB$JobInvolvement)
DFTranB$JobLevel <- as.factor(DFTranB$JobLevel)
DFTranB$JobSatisfaction <- as.factor(DFTranB$JobSatisfaction)
DFTranB$NumCompaniesWorked <- as.factor(DFTranB$NumCompaniesWorked)
DFTranB$PerformanceRating <- as.factor(DFTranB$PerformanceRating)
DFTranB$RelationshipSatisfaction <- as.factor(DFTranB$RelationshipSatisfaction)
DFTranB$StockOptionLevel <- as.factor(DFTranB$StockOptionLevel)
DFTranB$TrainingTimesLastYear <- as.factor(DFTranB$TrainingTimesLastYear)
DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)

DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)
DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)
DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)
DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)

#first lets try this on the plain data
Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = DFTrain)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, DFTrain)
table(NB_Prediction,DFTrain$Attrition)
NB_Test <- predict(Naive_Bayes_Model, DFTest)
confusionMatrix(table(NB_Test,DFTest$Attrition))
# NB_Test  No Yes
# No  213  23
# Yes  38  26
# Accuracy : 0.7967          
# 95% CI : (0.7466, 0.8407)
# No Information Rate : 0.8367          
# P-Value [Acc > NIR] : 0.97186         
# 
# Kappa : 0.3376          
# Mcnemar's Test P-Value : 0.07305         
#             Sensitivity : 0.8486          
#             Specificity : 0.5306          
#          Pos Pred Value : 0.9025          
#          Neg Pred Value : 0.4063   

#Create abridged dataset













#Now convert to factors to see if that helps
DFTranB <- DFTrain
DFTranB$Education <- as.factor(DFTranB$Education)
DFTranB$EnvironmentSatisfaction <- as.factor(DFTranB$EnvironmentSatisfaction)
DFTranB$JobInvolvement <- as.factor(DFTranB$JobInvolvement)
DFTranB$JobLevel <- as.factor(DFTranB$JobLevel)
DFTranB$JobSatisfaction <- as.factor(DFTranB$JobSatisfaction)
DFTranB$NumCompaniesWorked <- as.factor(DFTranB$NumCompaniesWorked)
DFTranB$PerformanceRating <- as.factor(DFTranB$PerformanceRating)
DFTranB$RelationshipSatisfaction <- as.factor(DFTranB$RelationshipSatisfaction)
DFTranB$StockOptionLevel <- as.factor(DFTranB$StockOptionLevel)
DFTranB$WorkLifeBalance <- as.factor(DFTranB$WorkLifeBalance)

DFTranB$TrainingTimesLastYear <- as.factor(DFTranB$TrainingTimesLastYear)


Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = DFTranB)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, DFTranB)
table(NB_Prediction,DFTranB$Attrition)
NB_Test <- predict(Naive_Bayes_Model, DFTest)
confusionMatrix(table(NB_Test,DFTest$Attrition))
# NB_Test  No Yes
# No   54   2
# Yes 197  47
# 
# Accuracy : 0.3367    








#```{r} 



training_data$YearsAtCompany <- as.factor(training_data$YearsAtCompany)
training_data$YearsInCurrentRole <- as.factor(training_data$YearsInCurrentRole)
training_data$YearsWithCurrManager<- as.factor(training_data$YearsWithCurrManager)
training_data$JobLevel <- as.factor(training_data$JobLevel)
training_data$StockOptionLevel <- as.factor(training_data$StockOptionLevel)

DFTrainRelFacs <- DFTrainRel
DFTrainRelFacs$Education <- as.factor(DFTrainRelFacs$Education)
DFTrainRelFacs$EnvironmentSatisfaction <- as.factor(DFTrainRelFacs$EnvironmentSatisfaction)
DFTrainRelFacs$JobInvolvement <- as.factor(DFTrainRelFacs$JobInvolvement)
DFTrainRelFacs$JobLevel <- as.factor(DFTrainRelFacs$JobLevel)
DFTrainRelFacs$JobSatisfaction <- as.factor(DFTrainRelFacs$JobSatisfaction)
DFTrainRelFacs$NumCompaniesWorked <- as.factor(DFTrainRelFacs$NumCompaniesWorked)
DFTrainRelFacs$PerformanceRating <- as.factor(DFTrainRelFacs$PerformanceRating)
DFTrainRelFacs$RelationshipSatisfaction <- as.factor(DFTrainRelFacs$RelationshipSatisfaction)
DFTrainRelFacs$StockOptionLevel <- as.factor(DFTrainRelFacs$StockOptionLevel)
DFTrainRelFacs$TrainingTimesLastYear <- as.factor(DFTrainRelFacs$TrainingTimesLastYear)
DFTrainRelFacs$WorkLifeBalance <- as.factor(DFTrainRelFacs$WorkLifeBalance)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Attrition ~., data=DFTrainRelFacs)
#What does the model say? Print the model summary
Naive_Bayes_Model
DFTrainRelFacs$AttInt <- as.integer(DFTrainRelFacs$Attrition)
DFTrainRelFacs$AttInt2 <- c(1)
DFTrainRelFacs$AttInt <- DFTrainRelFacs$AttInt - DFTrainRelFacs$AttInt2
model1 <- glm(formula = AttInt ~ ., family = binomial(link = "logit"), data = DFTrainRelFacs)
summary(model1)
```


```{r Naive Bayes original - tried on TrainRel-plain data, Facs- integers-into-categorical, FeatureSelection}
Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = DFTrainRel)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, DFTrainRel)
table(NB_Prediction,DFTrainRel$Attrition)
NB_Test <- predict(Naive_Bayes_Model, DFValRel)
confusionMatrix(table(NB_Test,DFValRel$Attrition))

Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = DFTrainRelFacs)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, DFTrainRelFacs)
table(NB_Prediction,DFTrainRelFacs$Attrition)
NB_Test <- predict(Naive_Bayes_Model, DFValRel)
confusionMatrix(table(NB_Test,DFValRel$Attrition))

Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = DFTrainFS)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, DFTrainFS)
table(NB_Prediction,DFTrainFS$Attrition)
NB_Test <- predict(Naive_Bayes_Model, DFValFS)
confusionMatrix(table(NB_Test,DFValFS$Attrition))
```

```{r NB}
###Refining of original Naive Bayes
training_data<- DFTrain
training_data <- training_data[,-1] #ID
Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = training_data)
Naive_Bayes_Model
NB_Prediction <- predict(Naive_Bayes_Model, training_data)
confusionMatrix(table(NB_Prediction,training_data$Attrition))
test_data <- DFTest
NB_Test <- predict(Naive_Bayes_Model, test_data)
confusionMatrix(table(NB_Test,test_data$Attrition))
lapply(training_data, summary)
#remove Over18 PerformanceRating StandardHours
training_data[,c('Over18','PerformanceRating','StandardHours','EmployeeNumber','EmployeeCount')] <- NULL
# ggpairs(training_data[c(1,4,6,11,17,18,21,24,27:31)])
#no multicolinearity in our continuous variables
training_data[c(24,27:30)]<- NULL #YEARS Variables
training_data[c(7,9,12,13,15,19,22:24)]<- lapply(training_data[c(7,9,12,13,15,19,22:24)], as.factor)
training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)]<- lapply(training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)], as.integer)
# chisq.test(training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)])
chisq.test(training_data[,c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)],training_data[,3])
ggpairs(training_data[c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)])
##corrplot(training_data[c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)], method = 'number')
training_data <- training_data[,-26]
training_data <- training_data[,-c(4,17,18)]
New_Naive_Bayes_Model <- naiveBayes(Attrition ~ ., data = training_data)
New_Naive_Bayes_Model
New_NB_Prediction <- predict(New_Naive_Bayes_Model, training_data)
table(New_NB_Prediction,training_data$Attrition)
test_data <- test_data[,-1] #ID
test_data[,c('Over18','PerformanceRating','StandardHours','EmployeeNumber','EmployeeCount')] <- NULL
test_data[c(24,27:30)]<- NULL #YEARS Variables
test_data[c(7,9,12,13,15,19,22:24)]<- lapply(test_data[c(7,9,12,13,15,19,22:24)], as.factor)
# 
test_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)]<- lapply(test_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)], as.integer)
test_data <- test_data[,-26]
ctrl <- rfeControl(functions = nbFuncs)
Attrition <- as.vector(
  as.numeric(
    as.factor(training_data$Attrition)))
# rfe(x=as.matrix(training_data[,c(1,3:25)]),
#     y=Attrition,
#     sizes =c(2:10), metric = 'RMSE', 
#     maximize = T, 
#     testX= test_data[c(1,3:25)],
#     testY=training_data$Attrition,
#     rfeControl = ctrl, size = 5)
# DFTrainRel$OverTime, 
# DFTrainRel$TotalWorkingYears, 
# DFTrainRel$YearsAtCompany, 
# DFTrainRel$StockOptionLevel, 
# DFTrainRel$MaritalStatus
training_data<- DFTrain
training_data <- training_data[,-1] #ID

#model 1 - 'OverTime','YearsInCurrentRole','StockOptionLevel','YearsAtCompany','TotalWorkingYears'
New_Naive_Bayes_Model <- naiveBayes(Attrition ~ OverTime + YearsInCurrentRole + StockOptionLevel + YearsAtCompany + TotalWorkingYears,data = training_data)
New_Naive_Bayes_Model
New_NB_Prediction <- predict(New_Naive_Bayes_Model, training_data[,c('OverTime','YearsInCurrentRole','StockOptionLevel','YearsAtCompany','TotalWorkingYears')])
confusionMatrix(table(New_NB_Prediction,training_data$Attrition))
Test_Data_NB_Pred <- predict(New_Naive_Bayes_Model, test_data[,c('OverTime','YearsInCurrentRole','StockOptionLevel','YearsAtCompany','MaritalStatus')])
confusionMatrix(table(Test_Data_NB_Pred,test_data$Attrition))

#model 2 - Add Marital Status, Remove Years in Current Role
New_Naive_Bayes_Model2 <- naiveBayes(Attrition ~ OverTime + TotalWorkingYears + YearsAtCompany + StockOptionLevel + MaritalStatus, data = training_data)
New_Naive_Bayes_Model2
New_NB_Prediction2 <- predict(New_Naive_Bayes_Model2, training_data[,c('OverTime','TotalWorkingYears','YearsAtCompany','StockOptionLevel','MaritalStatus')])
confusionMatrix(table(New_NB_Prediction2,training_data$Attrition))
Test_Data_NB_Pred2 <- predict(New_Naive_Bayes_Model2, test_data[,c('OverTime','TotalWorkingYears','YearsAtCompany','StockOptionLevel','MaritalStatus')])
confusionMatrix(table(Test_Data_NB_Pred,test_data$Attrition))

#test_data <- DFTest[,-1] #Eliminate ID
#Test_Data_NB <- naiveBayes(Attrition ~ OverTime + TotalWorkingYears + YearsAtCompany + StockOptionLevel + MaritalStatus, data = test_data)
#Test_Data_NB
#Test_Data_NB_Pred <- predict(Test_Data_NB, test_data[,c('OverTime','TotalWorkingYears','YearsAtCompany','StockOptionLevel','MaritalStatus')])
### By using Naive Bayes and specifically looking at OverTime, Total Working Years, Years at the Company, Stock Option Level, and Marital Status we can determine with 60% accuracy attrition as yes or no.
confusionMatrix(table(Test_Data_NB_Pred,test_data$Attrition))

#model 3 - Remove Total Working Years, Add Years in Current Role
#DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$StockOptionLevel, DFTrainRel$YearsInCurrentRole, DFTrainRel$MaritalStatus
New_Naive_Bayes_Model3 <- naiveBayes(Attrition ~ OverTime + YearsInCurrentRole + StockOptionLevel + YearsAtCompany + MaritalStatus,data = training_data)
New_Naive_Bayes_Model3
New_NB_Prediction3 <- predict(New_Naive_Bayes_Model3, training_data[,c('OverTime','YearsInCurrentRole','StockOptionLevel','YearsAtCompany','MaritalStatus')])
confusionMatrix(table(New_NB_Prediction3,training_data$Attrition))
Test_Data_NB_Pred3 <- predict(New_Naive_Bayes_Model3, test_data[,c('OverTime','YearsInCurrentRole','StockOptionLevel','YearsAtCompany','MaritalStatus')])
confusionMatrix(table(Test_Data_NB_Pred3,test_data$Attrition))

#model 4 - Remove MaritalStatus and StockOptionLevel, Add YearsWithCurrManager and JobLevel
#DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$YearsInCurrentRole, DFTrainRel$YearsWithCurrManager, DFTrainRel$JobLevel  
New_Naive_Bayes_Model4 <- naiveBayes(Attrition ~ OverTime + YearsAtCompany + YearsInCurrentRole + YearsWithCurrManager + JobLevel,data = training_data)
New_Naive_Bayes_Model4
New_NB_Prediction4 <- predict(New_Naive_Bayes_Model4, training_data[,c('OverTime','YearsAtCompany','YearsInCurrentRole','YearsWithCurrManager','JobLevel')])
confusionMatrix(table(New_NB_Prediction4,training_data$Attrition))
Test_Data_NB_Pred4 <- predict(New_Naive_Bayes_Model4, test_data[,c('OverTime','YearsAtCompany','YearsInCurrentRole','YearsWithCurrManager','JobLevel')])
confusionMatrix(table(Test_Data_NB_Pred4,test_data$Attrition))

#model 5 - Remove YearsAtCompany, Add StockOptionLevel
#DFTrainRel$OverTime, DFTrainRel$StockOptionLevel, DFTrainRel$MaritalStatus, DFTrainRel$YearsInCurrentRole, DFTrainRel$JobLevel  
New_Naive_Bayes_Model5 <- naiveBayes(Attrition ~ OverTime + StockOptionLevel + MaritalStatus + YearsInCurrentRole + JobLevel,data = training_data)
New_Naive_Bayes_Model5
New_NB_Prediction5 <- predict(New_Naive_Bayes_Model5, training_data[,c('OverTime','StockOptionLevel','MaritalStatus','YearsInCurrentRole','JobLevel')])
confusionMatrix(table(New_NB_Prediction5,training_data$Attrition))
Test_Data_NB_Pred5 <- predict(New_Naive_Bayes_Model5, test_data[,c('OverTime','StockOptionLevel','MaritalStatus','YearsInCurrentRole','JobLevel')])
confusionMatrix(table(Test_Data_NB_Pred5,test_data$Attrition))

#New Model with Composite Scores - zero specificity
#top3-  StockOptionLevel + YearsComposite + HappinessNoJI  
New_Naive_Bayes_Model6 <- naiveBayes(Attrition ~ StockOptionLevel  + YearsComposite + HappinessNoJI,data = training_data)
New_Naive_Bayes_Model6
New_NB_Prediction6 <- predict(New_Naive_Bayes_Model6, training_data[,c('StockOptionLevel','YearsComposite','HappinessNoJI')])
confusionMatrix(table(New_NB_Prediction5,training_data$Attrition))
Test_Data_NB_Pred6 <- predict(New_Naive_Bayes_Model6, test_data[,c('StockOptionLevel','YearsComposite','HappinessNoJI')])
confusionMatrix(table(Test_Data_NB_Pred6,test_data$Attrition))


#model 7 - Remove YearsAtCompany, Add StockOptionLevel
#DFTrainRel$OverTime, DFTrainRel$StockOptionLevel, DFTrainRel$MaritalStatus, YearsComposite, DFTrainRel$JobLevel  
New_Naive_Bayes_Model7 <- naiveBayes(Attrition ~ OverTime + StockOptionLevel + MaritalStatus + YearsComposite + JobLevel,data = training_data)
New_Naive_Bayes_Model5
New_NB_Prediction7 <- predict(New_Naive_Bayes_Model7, training_data[,c('OverTime','StockOptionLevel','MaritalStatus','YearsComposite','JobLevel')])
confusionMatrix(table(New_NB_Prediction5,training_data$Attrition))
Test_Data_NB_Pred7 <- predict(New_Naive_Bayes_Model7, test_data[,c('OverTime','StockOptionLevel','MaritalStatus','YearsComposite','JobLevel')])
confusionMatrix(table(Test_Data_NB_Pred7,test_data$Attrition))

#model 8 - Remove MaritalStatus and StockOptionLevel, Add YearsWithCurrManager and JobLevel
#DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$YearsInCurrentRole, DFTrainRel$YearsWithCurrManager, DFTrainRel$JobLevel  
New_Naive_Bayes_Model8 <- naiveBayes(Attrition ~ OverTime + YearsComposite + JobLevel,data = training_data)
New_Naive_Bayes_Model4
New_NB_Prediction8<- predict(New_Naive_Bayes_Model8, training_data[,c('OverTime','YearsComposite','JobLevel')])
confusionMatrix(table(New_NB_Prediction8,training_data$Attrition))
Test_Data_NB_Pred8 <- predict(New_Naive_Bayes_Model8, test_data[,c('OverTime','YearsComposite','JobLevel')])
confusionMatrix(table(Test_Data_NB_Pred8,test_data$Attrition))



```

