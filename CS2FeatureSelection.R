



#Predictions for model using Test dataset
TestAttrition <- DFTest$Attrition
DFTrainFS<- subset(DFTest, select=-c(Attrition ))
ctrl <- rfeControl(functions = nbFuncs)
rfe1 <- rfe(x=DFTrainFS, y=TestAttrition, sizes = 2^(2:27), maxit = 10, metric = "Accuracy" , rfeControl = ctrl, testx = DFTrainFS, testy = TestAttrition)
# Variables Accuracy  Kappa AccuracySD KappaSD Selected
# 4   0.7807 0.3022    0.04505  0.1072         
# 8   0.7627 0.3067    0.05760  0.1157         
# 16   0.7847 0.3222    0.04490  0.1066         
# 30   0.8291 0.3769    0.04663  0.1252        *
  # The top 5 variables (out of 30):
  # MonthlyIncome, TotalWorkingYears, JobLevel, Age, YearsAtCompany

#Feature Selection Analysis
#```{r Feature selection }
DFTrainFS <- cbind.data.frame(DFTrainRel$ID, DFTrainRel$BusinessTravel, DFTrainRel$Department, DFTrainRel$Education, DFTrainRel$EducationField, DFTrainRel$EnvironmentSatisfaction, DFTrainRel$Gender, DFTrainRel$HourlyRate, DFTrainRel$JobInvolvement, DFTrainRel$JobLevel, DFTrainRel$JobRole, DFTrainRel$JobSatisfaction, DFTrainRel$MaritalStatus, DFTrainRel$MonthlyRate, DFTrainRel$NumCompaniesWorked, DFTrainRel$OverTime, DFTrainRel$PercentSalaryHike, DFTrainRel$PerformanceRating, DFTrainRel$RelationshipSatisfaction, DFTrainRel$StockOptionLevel, DFTrainRel$TotalWorkingYears, DFTrainRel$TrainingTimesLastYear, DFTrainRel$WorkLifeBalance, DFTrainRel$YearsAtCompany, DFTrainRel$YearsSinceLastPromotion, DFTrainRel$YearsWithCurrManager, DFTrainRel$YearsInCurrentRole )
DFValFS <- cbind.data.frame(DFValRel$ID, DFValRel$BusinessTravel, DFValRel$Department, DFValRel$Education, DFValRel$EducationField, DFValRel$EnvironmentSatisfaction, DFValRel$Gender, DFValRel$HourlyRate, DFValRel$JobInvolvement, DFValRel$JobLevel, DFValRel$JobRole, DFValRel$JobSatisfaction, DFValRel$MaritalStatus, DFValRel$MonthlyRate, DFValRel$NumCompaniesWorked, DFValRel$OverTime, DFValRel$PercentSalaryHike, DFValRel$PerformanceRating, DFValRel$RelationshipSatisfaction, DFValRel$StockOptionLevel, DFValRel$TotalWorkingYears, DFValRel$TrainingTimesLastYear, DFValRel$WorkLifeBalance, DFValRel$YearsAtCompany, DFValRel$YearsSinceLastPromotion, DFValRel$YearsWithCurrManager, DFValRel$YearsInCurrentRole )
#TrainAttrition <- as.integer(DFTrainRel$Attrition)-1
#TestAttrition <- as.integer(DFValRel$Attrition)-1
TrainAttrition <- DFTrainRel$Attrition
TestAttrition <- DFValRel$Attrition
ctrl <- rfeControl(functions = nbFuncs)
rfe1 <- rfe(x=DFTrainFS, y=TrainAttrition, sizes = c(2,5), maxit = 10, metric = "Accuracy" , rfeControl = ctrl, testx = DFValFS, testy = TestAttrition)
#top5 - DFTrainRel$OverTime, DFTrainRel$TotalWorkingYears, DFTrainRel$YearsAtCompany, DFTrainRel$MaritalStatus, DFTrainRel$YearsInCurrentRole
#Variables Accuracy  Kappa AccuracySD KappaSD Selected
#         4   0.8282 0.1955    0.02308 0.07625         
#         8   0.8326 0.2846    0.02641 0.04974         
#        16   0.8509 0.2403    0.01554 0.06693        *
#        27   0.8498 0.1529    0.01489 0.08675  

#Remove some correlated variables - Monthly Income, Total Working Years, Performance rating, Years since last propotion, yeasr with current manager
DFTrainFS <- cbind.data.frame(DFTrainRel$ID, DFTrainRel$BusinessTravel, DFTrainRel$Education, DFTrainRel$EducationField, DFTrainRel$EnvironmentSatisfaction, DFTrainRel$Gender, DFTrainRel$HourlyRate, DFTrainRel$JobInvolvement, DFTrainRel$JobLevel, DFTrainRel$JobRole, DFTrainRel$JobSatisfaction, DFTrainRel$MaritalStatus, DFTrainRel$MonthlyRate, DFTrainRel$NumCompaniesWorked, DFTrainRel$OverTime, DFTrainRel$PercentSalaryHike, DFTrainRel$RelationshipSatisfaction, DFTrainRel$StockOptionLevel, DFTrainRel$TrainingTimesLastYear, DFTrainRel$WorkLifeBalance,  DFTrainRel$YearsInCurrentRole )
DFValFS <- cbind.data.frame(DFValRel$ID, DFValRel$BusinessTravel, DFValRel$Education, DFValRel$EducationField, DFValRel$EnvironmentSatisfaction, DFValRel$Gender, DFValRel$HourlyRate, DFValRel$JobInvolvement, DFValRel$JobLevel, DFValRel$JobRole, DFValRel$JobSatisfaction, DFValRel$MaritalStatus, DFValRel$MonthlyRate, DFValRel$NumCompaniesWorked, DFValRel$OverTime, DFValRel$PercentSalaryHike, DFValRel$RelationshipSatisfaction, DFValRel$StockOptionLevel, DFValRel$TrainingTimesLastYear, DFValRel$WorkLifeBalance, DFValRel$YearsInCurrentRole )

rfe2 <- rfe(x=DFTrainFS, y=TrainAttrition, sizes = 2^(2:27), maxit = 10, metric = "Accuracy" , rfeControl = ctrl, testx = DFValFS, testy = TestAttrition)
rfe2

#top5 - DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$StockOptionLevel, DFTrainRel$YearsInCurrentRole, DFTrainRel$MaritalStatus
#Variables Accuracy  Kappa AccuracySD KappaSD Selected
#         2   0.8309 0.0356    0.02096 0.06053         
#         5   0.8255 0.2204    0.02494 0.08264         
#        25   0.8499 0.1189    0.01379 0.05350 

#top5 -  DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$YearsInCurrentRole, DFTrainRel$YearsWithCurrManager, DFTrainRel$JobLevel  
#Variables Accuracy  Kappa AccuracySD KappaSD Selected
##         4   0.8239 0.1511    0.02436 0.05541         
#         8   0.8421 0.2606    0.01742 0.06804         
#        16   0.8532 0.1969    0.01763 0.06772        *
#        24   0.8491 0.1407    0.01734 0.05961  

#top5 - DFTrainRel$OverTime, DFTrainRel$YearsAtCompany, DFTrainRel$StockOptionLevel, DFTrainRel$MaritalStatus, DFTrainRel$YearsInCurrentRole
#         4   0.8387 0.1787    0.01494 0.07049         
#         8   0.8541 0.2321    0.01106 0.06567         
#       16   0.8548 0.1483    0.01251 0.06738        *
#        22   0.8509 0.1069    0.01230 0.05773 


#top5 - DFTrainRel$OverTime, DFTrainRel$StockOptionLevel, DFTrainRel$MaritalStatus, DFTrainRel$YearsInCurrentRole, DFTrainRel$JobLevel
#        4   0.8455 0.1613    0.01242 0.09275         
#         8   0.8560 0.1868    0.01384 0.09583        *
#        16   0.8496 0.1024    0.01515 0.06421         
#        21   0.8478 0.0809    0.01430 0.05013         
#```

#```{r rfe}
###Code used to identify 5 best parameters to use when applying Naive Bayes. Take time and processing power. 
#####THIS IS THE BEST PART
a <- rfe(x=DFTrainFS, y=TrainAttrition, sizes = c(2,5), maxit = 10, metric = "Accuracy" , rfeControl = ctrl, testx = DFValFS)1
a.results
#top5 - DFTrainRel$OverTime, DFTrainRel$TotalWorkingYears, DFTrainRel$YearsAtCompany, DFTrainRel$StockOptionLevel, DFTrainRel$MaritalStatus
#Variables Accuracy  Kappa AccuracySD KappaSD Selected
#         4   0.8339 0.2035    0.01486 0.07075         
#         8   0.8353 0.2942    0.01807 0.06181         
#        16   0.8609 0.2873    0.01107 0.06562        *
#        27   0.8553 0.1725    0.01195 0.06204
```