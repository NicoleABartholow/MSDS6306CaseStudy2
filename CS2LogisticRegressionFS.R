

#```{r}
##Logistic Regression for Feature Selection
#Logistic regression requires 0-1 (binary) output, must translate Attrition from 1,2 to 0, 1
DFTrain$AttInt <- as.integer(DFTrain$Attrition) #make an integer column of Attrition
DFTrain$AttInt2 <- c(1) #make a column of 1 for subtraction
DFTrain$AttInt <- DFTrain$AttInt - DFTrain$AttInt2 # subtract 1 from each value of AttInt
DFTrain<- subset(DFTrain, select=-c(AttInt2, Attrition)) #remove Attrition and column of 1 from dataset - it confuses the model

#first run model with everything and see what it chooses as important
model <- glm(formula = AttInt ~ ., family = binomial(link = "logit"), data = DFTrain)
summary(model)

# Null deviance: 1031.48  on 1169  degrees of freedom
# Residual deviance:  674.25  on 1124  degrees of freedom
# AIC: 766.25
# Number of Fisher Scoring iterations: 15

#Logistic Regression model chose, in order:
#   OverTimeYes                       1.959e+00  2.216e-01   8.843  < 2e-16 ***
#   EnvironmentSatisfaction          -4.713e-01  9.504e-02  -4.958 7.10e-07 ***
#   NumCompaniesWorked                2.009e-01  4.393e-02   4.572 4.84e-06 ***
#   JobInvolvement                   -6.312e-01  1.390e-01  -4.539 5.64e-06 ***
#   JobSatisfaction                  -3.914e-01  9.162e-02  -4.272 1.94e-05 ***
#   BusinessTravelTravel_Frequently   1.860e+00  4.660e-01   3.992 6.56e-05 ***
#   YearsSinceLastPromotion           1.864e-01  4.763e-02   3.913 9.13e-05 ***
#   DistanceFromHome                  4.357e-02  1.213e-02   3.593 0.000327 ***
#   WorkLifeBalance                  -4.772e-01  1.400e-01  -3.409 0.000651 ***
#   MaritalStatusSingle               1.332e+00  3.982e-01   3.345 0.000822 ***
#   YearsInCurrentRole               -1.475e-01  5.046e-02  -2.924 0.003458 **
#   
#   YearsAtCompany                    1.103e-01  4.332e-02   2.545 0.010922 *
#   JobRoleLaboratory Technician      1.282e+00  5.146e-01   2.492 0.012707 *
#   YearsWithCurrManager             -1.237e-01  4.987e-02  -2.480 0.013147 *
#   TotalWorkingYears                -8.269e-02  3.362e-02  -2.460 0.013906 *
#   BusinessTravelTravel_Rarely       9.590e-01  4.290e-01   2.235 0.025389 *
#   JobRoleSales Representative       3.098e+00  1.427e+00   2.171 0.029930 *
#   TrainingTimesLastYear            -1.773e-01  8.312e-02  -2.133 0.032923 *
#   JobRoleResearch Director         -2.824e+00  1.341e+00  -2.106 0.035174 *
#   RelationshipSatisfaction         -1.937e-01  9.257e-02  -2.092 0.036394 *




#now rerun model with subset of variables to see if it improves prediction
model <- glm(formula = AttInt ~  OverTime+EnvironmentSatisfaction + NumCompaniesWorked + JobInvolvement + JobSatisfaction +
               BusinessTravel + YearsSinceLastPromotion + DistanceFromHome + WorkLifeBalance + MaritalStatus + YearsInCurrentRole + 
               YearsAtCompany + JobRole + TrainingTimesLastYear + RelationshipSatisfaction
               , family = binomial(link = "logit"), data = DFTrain)
summary(model)

# Null deviance: 1031.5  on 1169  degrees of freedom
# Residual deviance:  720.9  on 1145  degrees of freedom
# AIC: 770.9
# Number of Fisher Scoring iterations: 7

# OverTimeYes                      1.739247   0.205852   8.449  < 2e-16 ***
#   MaritalStatusSingle              1.553598   0.301229   5.158 2.50e-07 ***
#   JobInvolvement                  -0.634243   0.131545  -4.821 1.42e-06 ***
#   EnvironmentSatisfaction         -0.428613   0.089319  -4.799 1.60e-06 ***
#   JobRoleSales Representative      2.149064   0.506922   4.239 2.24e-05 ***
#   JobSatisfaction                 -0.380161   0.086988  -4.370 1.24e-05 ***
#   YearsSinceLastPromotion          0.171688   0.044230   3.882 0.000104 ***
#   BusinessTravelTravel_Frequently  1.668351   0.445594   3.744 0.000181 ***
#   YearsInCurrentRole              -0.163831   0.047607  -3.441 0.000579 ***
#   JobRoleLaboratory Technician     1.430406   0.439415   3.255 0.001133 ** 
#   DistanceFromHome                 0.035770   0.011450   3.124 0.001784 ** 
#   WorkLifeBalance                 -0.407168   0.133690  -3.046 0.002322 ** 
#   NumCompaniesWorked               0.113572   0.037950   2.993 0.002765 ** 
#   TrainingTimesLastYear           -0.160952   0.080259  -2.005 0.044921 * 

#Categorical not really in first run
# JobRoleResearch Director        -2.541734   1.177914  -2.158 0.030941 *  
# JobRoleSales Executive           0.925635   0.436833   2.119 0.034093 *  
#   MaritalStatusMarried             0.655343   0.298569   2.195 0.028167 *  
#   BusinessTravelTravel_Rarely      0.853431   0.412635   2.068 0.038617 * 
#Dropped off of importance
#   YearsAtCompany                  -0.002331   0.032048  -0.073 0.942016    
#   RelationshipSatisfaction        -0.171521   0.087902  -1.951 0.051025 .


#what happens if you add Happiness
#first to the whole model
DFTrain<- subset(DFTrain, select=-c(RelationshipSatisfaction, EnvironmentSatisfaction, JobSatisfaction)) #remove columns from dataset - it confuses the model
DFTrain<- subset(DFTrain, select=-c(YearsTotal, Happiness))
model <- glm(formula = AttInt ~ ., family = binomial(link = "logit"), data = DFTrain)
summary(model)
# Null deviance: 1031.5  on 1169  degrees of freedom
# Residual deviance:  679.0  on 1126  degrees of freedom
# AIC: 767
#now try it with just the important variables
model <- glm(formula = AttInt ~  OverTime + NumCompaniesWorked + JobInvolvement +
               BusinessTravel + YearsSinceLastPromotion + DistanceFromHome + WorkLifeBalance + MaritalStatus + YearsInCurrentRole + 
               YearsAtCompany + JobRole + TrainingTimesLastYear + Happiness
             , family = binomial(link = "logit"), data = DFTrain)
summary(model)
# Null deviance: 1031.48  on 1169  degrees of freedom
# Residual deviance:  725.76  on 1147  degrees of freedom
# AIC: 771.76

#Now Let's break the dataset into JobLevels, 1-2 & 3-5
#first to the whole model
model <- glm(formula = AttInt ~ ., family = binomial(link = "logit"), data = Job12)
summary(model)
#Null deviance: 418.17  on 427  degrees of freedom
#Residual deviance: 238.17  on 386  degrees of freedom
#AIC: 322.17
#   OverTimeYes                       2.895e+00  4.373e-01   6.620 3.59e-11 ***
# JobSatisfaction                  -6.226e-01  1.780e-01  -3.499 0.000468 ***
# NumCompaniesWorked                2.922e-01  8.733e-02   3.346 0.000819 ***
# YearsSinceLastPromotion           3.306e-01  1.023e-01   3.232 0.001228 ** 
# EnvironmentSatisfaction          -5.363e-01  1.722e-01  -3.114 0.001845 ** 
# YearsInCurrentRole               -3.315e-01  1.118e-01  -2.964 0.003039 ** 
# RelationshipSatisfaction         -4.645e-01  1.695e-01  -2.741 0.006124 ** 
# BusinessTravelTravel_Frequently   2.470e+00  1.027e+00   2.406 0.016121 *  
# DistanceFromHome                  4.766e-02  2.204e-02   2.162 0.030584 *  
# GenderMale                        6.779e-01  3.842e-01   1.765 0.077647 .  
# JobInvolvement                   -6.606e-01  2.620e-01  -2.521 0.011687 *  

#This doesn't run but it looks like JobLevel is within each role???
Job35<- subset(Job35, select=-c(RelationshipSatisfaction, EnvironmentSatisfaction, JobSatisfaction, Age, DailyRate, Gender, HourlyRate, MonthlyIncome, MonthlyRate))
Job35<- subset(Job35, select=-c(Age, DailyRate, Gender, HourlyRate, MonthlyIncome, MonthlyRate))
model <- glm(formula = AttInt ~ ., family = binomial(link = "logit"), data = Job35)
summary(model)


#now try it with just the important variables
model <- glm(formula = AttInt ~  OverTime + NumCompaniesWorked + JobInvolvement +
               BusinessTravel + YearsSinceLastPromotion + DistanceFromHome + WorkLifeBalance + MaritalStatus + YearsInCurrentRole + 
               YearsAtCompany + JobRole + TrainingTimesLastYear + Happiness
             , family = binomial(link = "logit"), data = DFTrain)
summary(model)
# Null deviance: 1031.48  on 1169  degrees of freedom
# Residual deviance:  725.76  on 1147  degrees of freedom
# AIC: 771.76


#```