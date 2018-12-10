
#Analysis of missing/unneccesary data
#```{r Data Cleaning}
EmptyCols <- colSums(is.na(DFTrain))#Check for NA - zero NA fields

#Remove columns that are irrelevant or have only one value
DFTrain<- subset(DFTrain, select=-c(ID, EmployeeCount,EmployeeNumber,Over18,StandardHours, Rand ))
DFTest<- subset(DFTest, select=-c(ID, EmployeeCount,EmployeeNumber,Over18,StandardHours, Rand ))

#```

#```{r Create New Derived Variables for Analysis}
#Composite Happiness Scores

#Create Composite Happiness (NO Job Involvement)
#Using a total rather than an average because the Satisfaction scores are not collinear (surprisingly) so a very low score in one area does not mean they will all be low.
#for that reason, IF they are all low (minimum possible total is 4, that means nothing is redeaming the role - looking for risk factors of VERY low scores.)
#Logistic Regression implies Happiness is the same variable with or without Job Involvement
DFTrain$Happiness <- DFTrain$EnvironmentSatisfaction+DFTrain$JobSatisfaction+DFTrain$RelationshipSatisfaction+DFTrain$WorkLifeBalance 
DFTest$Happiness <- DFTest$EnvironmentSatisfaction+DFTest$JobSatisfaction+DFTest$RelationshipSatisfaction+DFTest$WorkLifeBalance 

DFTotal <- rbind(DFTrain, DFTest)


#Create Composite Happiness (WITH Job Involvement)
#Logistic Regression implies Happiness is the same variable with or without Job Involvement, we will se if they help with kNN or NB
DFTrain$HappinessWJI <- DFTrain$Happiness+DFTrain$JobInvolvement 
DFTest$HappinessWJI <- DFTest$Happiness+DFTest$JobInvolvement 

#Create Composite Years
#THE YEARS features were highly collinear, not surprisingly.  During feature selection, different combinations of these popped up.  
#Trying both an AVG and a TOTAL to capture them and see if these work better than the individual ones
#Logistic Regression says that these variables are not significant, we will se if they help with kNN or NB
DFTrain$YearsTotal <- DFTrain$YearsAtCompany+DFTrain$YearsSinceLastPromotion+DFTrain$YearsWithCurrManager+DFTrain$YearsInCurrentRole
DFTrain$YearsAvg <- DFTrain$YearsTotal/4
DFTest$YearsTotal <- DFTest$YearsAtCompany+DFTest$YearsSinceLastPromotion+DFTest$YearsWithCurrManager+DFTest$YearsInCurrentRole
DFTest$YearsAvg <- DFTest$YearsTotal/4
#```


DFTrain %>%
  filter(Attrition == "Yes") %>% 
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
DFTrain %>%
  filter(Attrition == "No") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()



#```{r Percentage Analysis}
#Percentage Analysis
AttrY <- DFTrain[grep("Yes", DFTrain$Attrition), ]
AttrN <- DFTrain[grep("No", DFTrain$Attrition), ]

######
###Interesting Stats for Yes
#more than half of them are single
#all singles have stock option level = 0
#for other Y, max stock option level is 3, mean is .52 and median is 0
#Happiness minimum is 4 and median is 10, mean is 10.14
#Median monthly income is 3482, median hourly is 67, but the mean us 66.7, so extreme low value is pulling down
#median for job level is 1 and mean is 1.67
#numcompanies worked median is 1.5 but mean is 3
#overtime 100, versus 88no
#years in current role and years with current manager are very similar

#Contrast to No
#Median monthly income is 5167, median hourly is 66, but the mean us 65.7, so the same hourly but making much more money, are the Yes group underemplioyed?
#only 1 person with Happiness of 4 - Happiness under 10
Happiness4 <- DFTrain[which(DFTotal$Happiness== 4),]

Job13Test <- DFTest[which(DFTest$JobLevel== 1:2),]
Job35Test <- DFTest[which(DFTest$JobLevel== 3:5),]


#DFTrain <- transform(DFTrain, Job13= ifelse(JobLevel== 1:2, 1, 2))
DFTrain <- transform(DFTrain, Year1= ifelse(YearsAtCompany== 1, 1, 2))
DFTest <- transform(DFTest, Year1= ifelse(YearsAtCompany== 1, 1, 2))

#DFTrain$JobInt <- as.integer(DFTrain$JobRole)
#DFTrain <- transform(DFTrain, SalesJob= ifelse(JobInt== 8:9, 1, 2))

DFTrain$MSInt <- as.integer(DFTrain$MaritalStatus)
DFTrain <- transform(DFTrain, Single= ifelse(MSInt== 1, 1, 2))
DFTest$MSInt <- as.integer(DFTest$MaritalStatus)
DFTest <- transform(DFTest, Single= ifelse(MSInt== 1, 1, 2))

DFTrain<- subset(DFTrain, select=-c(Happy ))
DFTrain <- transform(DFTrain, Happy= ifelse(Happiness == 1:9, 1, 2))



pAttrY <- dim(AttrY)[1] / (dim(AttrY)[1] + dim(AttrN)[1])

AttrYS <- AttrY[grep("Single", AttrY$MaritalStatus), ] #check out AttrYSingles
AttrYSN <- AttrY[-grep("Single", AttrY$MaritalStatus), ] #check out AttrY Married or Divorced

AttrNS <- AttrN[grep("Single", AttrN$MaritalStatus), ] #check out AttrNSingles
AttrNSN <- AttrN[-grep("Single", AttrN$MaritalStatus), ] #check out AttrN Married or Divorced




#versus AttrY with Stock Level 0
AttrY0 <- AttrY[AttrY$StockOptionLevel ==0] #check out AttrY Stock Option Level = 0
AttrY14 <- AttrY[-grep("Single", AttrY$MaritalStatus), ] #check out AttrY Married or Divorced



pAttrN <- 1 - pAttrY
#OverTime
pOTYGivenAttY <- length(grep("Yes",AttrY$OverTime))/dim(AttrY)[1]
pOTYGivenAttN <- length(grep("Yes",AttrN$OverTime))/dim(AttrN)[1]
#Environment Satisfaction
pES1GivenAttY <- length(grep(1,AttrY$EnvironmentSatisfaction))/dim(AttrY)[1]
pES1GivenAttN <- length(grep(1,AttrN$EnvironmentSatisfaction))/dim(AttrN)[1]
pES2GivenAttY <- length(grep(2,AttrY$EnvironmentSatisfaction))/dim(AttrY)[1]
pES2GivenAttN <- length(grep(2,AttrN$EnvironmentSatisfaction))/dim(AttrN)[1]
pES3GivenAttY <- length(grep(3,AttrY$EnvironmentSatisfaction))/dim(AttrY)[1]
pES3GivenAttN <- length(grep(3,AttrN$EnvironmentSatisfaction))/dim(AttrN)[1]
pES4GivenAttY <- length(grep(4,AttrY$EnvironmentSatisfaction))/dim(AttrY)[1]
pES4GivenAttN <- length(grep(4,AttrN$EnvironmentSatisfaction))/dim(AttrN)[1]
```


#no multicolinearity in our continuous variables - keeping this because I know some of the code works on training data and I want ot be sure I can reproduce that
training_data[c(24,27:30)]<- NULL #YEARS Variables
training_data[c(7,9,12,13,15,19,22:24)]<- lapply(training_data[c(7,9,12,13,15,19,22:24)], as.factor)
training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)]<- lapply(training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)], as.integer)
# chisq.test(training_data[c(3,5,7,8,9,10,12,13,14,15,16,19,20,22:24)])
chisq.test(training_data[,c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)],training_data[,3])
ggpairs(training_data[c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)])
##corrplot(training_data[c(5,7,8,9,10,12,13,14,15,16,19,20,22:24)], method = 'number')
training_data <- training_data[,-26]
training_data <- training_data[,-c(4,17,18)]




