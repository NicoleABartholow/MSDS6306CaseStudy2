#Data Preparation
#Making Categorical Variables into integers/continuous Variables
#```{r}
#DFTrak - Training Dataset adjusted for kNN model
#DFTek - Test Dataset adjusted for knn model

DFTrak <- DFTrain
DFTrak$Attrition <- as.integer(DFTrak$Attrition)
DFTrak$BusinessTravel <- as.integer(DFTrak$BusinessTravel)
DFTrak$Department <- as.integer(DFTrak$Department)
DFTrak$EducationField <- as.integer(DFTrak$EducationField)
DFTrak$Gender <- as.integer(DFTrak$Gender)
DFTrak$JobRole <- as.integer(DFTrak$JobRole)
DFTrak$MaritalStatus <- as.integer(DFTrak$MaritalStatus)
DFTrak$OverTime <- as.integer(DFTrak$OverTime)

DFTek <- DFTest
DFTek$Attrition <- as.integer(DFTek$Attrition)
DFTek$BusinessTravel <- as.integer(DFTek$BusinessTravel)
DFTek$Department <- as.integer(DFTek$Department)
DFTek$EducationField <- as.integer(DFTek$EducationField)
DFTek$Gender <- as.integer(DFTek$Gender)
DFTek$JobRole <- as.integer(DFTek$JobRole)
DFTek$MaritalStatus <- as.integer(DFTek$MaritalStatus)
DFTek$OverTime <- as.integer(DFTek$OverTime)
#```
#####DELETE THIS WHEN DONE
#Run through adding more 
results1 <- class::knn(DFTrak[,c(20, 33,  13, 16)], DFTek[,c(20, 33,  13, 16)], DFTrak$Attrition, k=7)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#pulls one correct yes into a no prediction
# 1   2
# 1 245   6
# 2  39  10
# Accuracy : 0.85            
# 95% CI : (0.8045, 0.8884)
# No Information Rate : 0.9467          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.2472          
# Mcnemar's Test P-Value : 1.84e-06        
# 
# Sensitivity : 0.8627          
# Specificity : 0.6250          
# Pos Pred Value : 0.9761          
# Neg Pred Value : 0.2041      

#15 is pretty good, 13 is pretty good, 24 is pretty good

#score with JobLevel - sort of best so far
#Job Level, Marital Status, OverTime
results1 <- class::knn(DFTrak[,c(16, 20, 13)], DFTek[,c(16,20, 13)], DFTrak$Attrition, k=7)
DFTek$AttPred1 <- results1
kNN_Prediction <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
confusionMatrix(table(DFTek$Attrition,kNN_Prediction))
# 249   2
# 40   9
#Sensitivity : 0.8616          
#Specificity : 0.8182          
#Pos Pred Value : 0.9920          
#Neg Pred Value : 0.1837

#score with StockOptionLevel
results1 <- class::knn(DFTrak[,c(16, 20, 24)], DFTek[,c(16,20, 24)], DFTrak$Attrition, k=7)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#242   9
# 41   8
#Sensitivity : 0.8551          
#Specificity : 0.4706          
#Pos Pred Value : 0.9641          
#Neg Pred Value : 0.1633 


# MonthlyIncome, TotalWorkingYears, JobLevel, Age, YearsAtCompany
results1 <- class::knn(DFTrak[,c(17, 25, 13, 1, 28)], DFTek[,c(17, 25, 13, 1, 28)], DFTrak$Attrition, k=7)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))




#score with numcompanies worked-19
results1 <- class::knn(DFTrak[,c(16, 20, 19)], DFTek[,c(16,20, 19)], DFTrak$Attrition, k=7)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
# 242   9
#  41   8
#Sensitivity : 0.8551          
#Specificity : 0.4706          
#Pos Pred Value : 0.9641          
#Neg Pred Value : 0.1633 

#- score with Env Sat-7
#> results1 <- class::knn(DFTrak[,c(16, 20, 9)], DFTek[,c(16,20, 9)], DFTrak$Attrition, k=7)
#> DFTek$AttPred1 <- results1
#> confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
# 245   6
#  42   7
#Sensitivity : 0.8537          
#Specificity : 0.5385


#- score without Env Sat-7
#> results1 <- class::knn(DFTrak[,c(16, 20, 9)], DFTek[,c(16,20, 9)], DFTrak$Attrition, k=7)
#> DFTek$AttPred1 <- results1
#> confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
# 244   7
#  40   9
#Sensitivity : 0.8592          
#Specificity : 0.5625
#Pos Pred Value : 0.9641          
#Neg Pred Value : 0.1633


#- score with Env Sat-5
#Sensitivity : 0.8537     
#Specificity : 0.5385


#old score
#Sensitivity : 0.8561          
#Specificity : 0.5333

####DELETE THIS WHEN DONE-END



#with Everything, nope
results34 <- class::knn(DFTrak, DFTek, DFTrak$Attrition, k=6)
DFTek$AttPred34 <- results34
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred34))
#Sensitivity : 0.8541         
#Specificity : 0.4211


#Run Model with each predictor to determine best predictors???
#k = 34 (Square root of number of observations in training set) Overall Accuracy .8367 
#Sets everything to NO
#Overtime and Marital Status
results34 <- class::knn(DFTrak[,c(16, 20)], DFTek[,c(16,20)], DFTrak$Attrition, k=34)
DFTek$AttPred34 <- results34
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred34))
#Sensitivity : 0.8526          
#Specificity : 0.4667 

#Change to use the single closest predictor
results1 <- class::knn(DFTrak[,c(16, 20)], DFTek[,c(16,20)], DFTrak$Attrition, k=1)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#Sensitivity : 0.8566          
#Specificity : 0.5714 

#Change to use the three closest predictor
results1 <- class::knn(DFTrak[,c(16, 20)], DFTek[,c(16,20)], DFTrak$Attrition, k=3)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#pulls one correct yes into a no prediction
#Sensitivity : 0.8561          
#Specificity : 0.5333

#Run through adding more 
results1 <- class::knn(DFTrak[,c(16, 20)], DFTek[,c(16,20)], DFTrak$Attrition, k=3)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#pulls one correct yes into a no prediction
#Sensitivity : 0.8561          
#Specificity : 0.5333








#Overtime and Marital Status and ENvSat
results34 <- class::knn(DFTrak[,c(16, 20, 10)], DFTek[,c(16,20, 10)], DFTrak$Attrition, k=34)
DFTek$AttPred34 <- results34
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred34))
Sensitivity : 0.8367          
Specificity :     NA 


results34 <- class::knn(DFTrak, DFTek, DFTrak$Attrition, k=34)
DFTek$AttPred34 <- results34
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred34))

#Create a Train and Test Dataframe with the best predictors and run kNN on those
#DFTrak <- assign best features
#DFTek <- match training DF


#kNN Models
#```{r kNN models}
#kNN for Attrition
#k = 34 (Square root of number of observations in training set) Overall Accuracy .8367 
#Sets everything to NO
results34 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=34)
DFTek$AttPred34 <- results34
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred34))
#k = 1 Overall Accuracy 0.73333
#The only kNN where I predicted some of the positives
results1 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=1)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#k = 2 Overall Accuracy 0.6767
#The only kNN where I predicted some of the positives
results2 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=2)
DFTek$AttPred2 <- results2
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred2))
#k = 3 Overall Accuracy 0.6767
#The only kNN where I predicted some of the positives correctly
results3 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=3)
DFTek$AttPred3 <- results3
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred3))
#k = 5 Overall Accuracy 0.83
results5 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=5)
DFTek$AttPred5 <- results5
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred5))
#k = 10 Overall Accuracy 0.8267
results10 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=10)
DFTek$AttPred10 <- results10
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred10))
#k = 20 Overall Accuracy .8367
results20 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=20)
DFTek$AttPred20 <- results20
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred20))
#k = 30 0Overall Accuracy Overall Accuracy .8367
results30 <- class::knn(DFTrak[,c(10, 20, 33)], DFTek[,c(10, 20, 33)], DFTrak$Att, k=30)
DFTek$AttPred30 <- results30
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred30))
#Try with additional variables - does not improve accuracy over .8367
#k = 3Plus (Square root of number of observations in training set) Overall Accuracy .8367
results3Plus <- class::knn(DFTrak[,c(10, 13,  20, 33)], DFTek[,c(10, 13, 20, 33)], DFTrak$Att, k=3)
DFTek$AttPred3Plus <- results3Plus
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred3Plus))
#k = 1 Overall Accuracy 
#Try different variables - remove overtime worked .7433
results1 <- class::knn(DFTrak[,c(10, 33)], DFTek[,c(10,  33)], DFTrak$Att, k=1)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#k = 1 Overall Accuracy 
#Try different variables - add Job Involvement .74
#Correctly predicted 7 yes
results1 <- class::knn(DFTrak[,c(10, 13, 33)], DFTek[,c(10, 13,  33)], DFTrak$Att, k=1)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#k = 3 Overall Accuracy 
#Try different variables - add Job Involvement, increase k .83
#Correctly predicted 1 yes
results1 <- class::knn(DFTrak[,c(11, 14, 33)], DFTek[,c(11, 14,  33)], DFTrak$Att, k=3)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#k = 3 Overall Accuracy 
#Try different variables Envoironment Satisfaction, Hourly Rate, Years with Current Manager.8067
#Correctly predict 4 yes
results1 <- class::knn(DFTrain[,c(11, 13, 33)], DFVal[,c(11, 13,  33)], DFTrain$Att, k=3)
DFTek$AttPred1 <- results1
confusionMatrix(table(DFTek$Attrition,DFTek$AttPred1))
#```





#Replace this dataframe name  
#```{r Cross Validation model}
resultsCV <- class::knn.cv(DFTrak[,c(10, 20, 33)],DFTrak$Att, k=1)
DFTrak$AttPredCV <- resultsCV
confusionMatrix(table(DFTrak$Attrition,DFTrak$AttPredCV))
#```
