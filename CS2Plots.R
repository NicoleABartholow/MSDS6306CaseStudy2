#Correlation Plots
#Not surprisingly, all the Years and Age are correlated
#Surprisingly, the Satisfactions do not show any correlation

DFTrain$EducationField <- as.integer(DFTrain$EducationField)
DFTrain$JobRole <- as.integer(DFTrain$JobRole)
DFTrain$OverTime <- as.integer(DFTrain$OverTime)
DFTrain$MaritalStatus <- as.integer(DFTrain$MaritalStatus)
DFTrain$BusinessTravel <- as.integer(DFTrain$BusinessTravel)
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

#Create barplot for Department counts
#x must be continuous here, try again
p <- ggplot(DFTotal, aes(x = Department))+geom_bar(fill="#3182bd", colour="black")
p + scale_fill_brewer(palette = "Blues") + 
  ggtitle("Distribution of Sierra Employees by Department")+ 
  theme( plot.title = element_text(hjust = 0.5))


#Create a table of JobRoles by level
#RoleLevelCount <- data.frame(table(DFTrain$JobRole, DFTrain$JobLevel), decreasing=TRUE)
RoleLevelCount <- data.frame(table(DFTotal$JobRole, DFTotal$JobLevel))
colnames(RoleLevelCount)<-c("JobRole", "JobLevel", "Freq")#set column names to improve readability
RoleLevelCount
RoleLevelCount <- dcast(RoleLevelCount, JobRole ~ JobLevel)
#RoleLevelCount%>%
#  arrange_at(2:5, desc)

colnames(RoleLevelCount) <- c("JobRole", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
RoleLevelCount$Total <- RoleLevelCount$`Level 1`+RoleLevelCount$`Level 2`+RoleLevelCount$`Level 3`+RoleLevelCount$`Level 4`+RoleLevelCount$`Level 5`

RoleLevelCount%>%
  arrange(match(JobRole, c("Sales Representative", "Human Resources", "Laboratory Technician", "Research Scientist", "Sales Executive", "Manufacturing Director", "Healthcare Representative", "Research Director", "Manager")))%>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


#Lets just do a straight stacked graph without proportions on JobRole
RoleAttritionCount <- data.frame(table(DFTotal$JobRole, DFTotal$Attrition))
colnames(RoleAttritionCount)<- c("JobRole", "Attrition","NumberOfEmployees")
p <- ggplot(RoleAttritionCount, aes(x=reorder(JobRole, -NumberOfEmployees), y=NumberOfEmployees, fill=Attrition))+geom_bar(stat="identity", colour="black")
p + scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  ggtitle("Employee Population by Job Role with Attrition Count")+ 
  theme( axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  xlab("JobRoles") + ylab("Number of Employees") 


#Let's get proportional stacked graphs to work!!!
#first you need a summarized view of the data
#RoleCount <- data.frame(sort(table(DFTotal$JobRole))) 
RoleLevelCount <- data.frame(table(DFTotal$JobRole, DFTotal$JobLevel))
#create a summary table for Attrtion by JobRole
RoleAttritionCount <- data.frame(table(DFTotal$JobRole, DFTotal$Attrition))
#Calculate percent attrition by role and add it as a new column
RA <- ddply(RoleAttritionCount, "Var1", transform,
            percent_attrition = Freq / sum(Freq) * 100)
colnames(RA)<-c("JobRole", "Attrition", "Freq", "PercentAttrition")#set column names to improve readability
RA$Order<- c(13, 14, 5, 6, 3, 4, 15, 16, 11, 12, 17, 18, 9, 10, 7, 8, 1, 2)
RA <- RA[order(RA$Order),]
rownames(RA) <- 1:nrow(RA)

#Proportional Stacked Bar Graph of Attrition by Job Role
ggplot(RA, aes(x=reorder(JobRole, Order), y=PercentAttrition, fill=Attrition)) +
    geom_bar(stat="identity", colour="black") +
    guides(fill=guide_legend(reverse=FALSE)) + 
    theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
    ggtitle("Percentage Attrition by JobRole")+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+ 
    xlab("Job Roles") + ylab("Percent Attrition") 

#scale_fill_manual(values=c("#9ecae1", "#3182bd"))+ 
#scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))

#Now let's look at that by Levels
DFTotal$JobLevel <- as.factor(DFTotal$JobLevel)
LevelAttritionCount <- data.frame(table(DFTotal$JobLevel, DFTotal$Attrition))
colnames(LevelAttritionCount)<- c("JobLevel", "Attrition","NumberOfEmployees")
p <- ggplot(LevelAttritionCount, aes(x=reorder(JobLevel, -NumberOfEmployees), y=NumberOfEmployees, fill=Attrition))+geom_bar(stat="identity", colour="black")
p + scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  ggtitle("Employee Population by Job Level with Attrition Count")+ 
  theme( axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  xlab("Job Levels") + ylab("Number of Employees") 

LA <- ddply(LevelAttritionCount, "JobLevel", transform,
            percent_attrition = NumberOfEmployees / sum(NumberOfEmployees) * 100)
colnames(LA)<-c("JobLevel", "Attrition", "Freq", "PercentAttrition")#set column names to improve readability

#Proportional Stacked Bar Graph of Attrition by Job Level
ggplot(LA, aes(x=JobLevel, y=PercentAttrition, fill=Attrition)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage Attrition by JobLevel")+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+ 
  xlab("Job Levels") + ylab("Percent Attrition") 


#Plot of Happiness for Entire Company by Job Role
qplot(JobRole, Happiness, geom="boxplot", data=DFTotal)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments")+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))

bp<-ggplot2::ggplot(DFTotal, aes(x=JobRole, y=Happiness)) +
  geom_boxplot()+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments")+
  scale_fill_manual(values="3182bd")
bp

#scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))

#Lets just do a straight stacked graph without proportions on Department
DeptAttritionCount <- data.frame(table(DFTotal$Department, DFTotal$Attrition))
colnames(DeptAttritionCount)<- c("Department", "Attrition","NumberOfEmployees")
p <- ggplot(DeptAttritionCount, aes(x=reorder(Department, -NumberOfEmployees), y=NumberOfEmployees, fill=Attrition))+geom_bar(stat="identity", colour="black")
p + scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  ggtitle("Employee Population by Department with Attrition Count")+ 
  theme( axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  xlab("Department") + ylab("Number of Employees") 


#Let's get proportional stacked graphs to work!!!
#first you need a summarized view of the data
#RoleCount <- data.frame(sort(table(DFTotal$JobRole))) 
RoleLevelCount <- data.frame(table(DFTotal$JobRole, DFTotal$JobLevel))
#create a summary table for Attrtion by JobRole
RoleAttritionCount <- data.frame(table(DFTotal$JobRole, DFTotal$Attrition))
#Calculate percent attrition by role and add it as a new column
DA <- ddply(DeptAttritionCount, "Department", transform,
            percent_attrition = NumberOfEmployees / sum(NumberOfEmployees) * 100)
colnames(DA)<-c("Department", "Attrition", "Freq", "PercentAttrition")#set column names to improve readability
#RA$Order<- c(13, 14, 5, 6, 3, 4, 15, 16, 11, 12, 17, 18, 9, 10, 7, 8, 1, 2)
#RA <- RA[order(RA$Order),]
#rownames(RA) <- 1:nrow(RA)

#Proportional Stacked Bar Graph of Attrition by Department
ggplot(DA, aes(x=reorder(Department, Freq), y=PercentAttrition, fill=Attrition)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage Attrition by Department")+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+ 
  xlab("Department") + ylab("Percent Attrition") 




#Correlations between Attrition and Predictors 
#Attrtion and Overtime- 0.246
DFTrain$Attrition<-as.integer(DFTrain$Attrition)
DFTotal$OverTime<-as.integer(DFTotal$OverTime)
AttOT <-cor(DFTotal$Attrition, DFTotal$OverTime, use = "complete.obs") 

#Attrition and Time with Company <2 years - AttYears = -0.181
AttYears <-cor(DFTrain$Attrition, DFTrain$Year1, use = "complete.obs") 

#Attrition and  Marital Satus - 0.162
DFTrain$MaritalStatus<-as.integer(DFTrain$MaritalStatus)
AttMS <-cor(DFTrain$Attrition, DFTrain$MaritalStatus, use = "complete.obs") 
#Derived Marital Status
AttMSI <-cor(DFTrain$Attrition, DFTrain$MSInt, use = "complete.obs") # - 0.18

#Attrition and NCW - AttJobInvolvement = -0.150
AttJI <-cor(DFTrain$Attrition, DFTrain$JobInvolvement, use = "complete.obs") 

#Attrition and EnvSat - AttES = -0.126
AttES <-cor(DFTrain$Attrition, DFTrain$EnvironmentSatisfaction, use = "complete.obs") 


#Attrition and Happiness - AttSales = this is not important -0.038
AttHappy <-cor(DFTrain$Attrition, DFTrain$Happy, use = "complete.obs") 

#Attrition and JobLevel - AttSales = this is not important -0.064
AttJob13 <-cor(DFTrain$Attrition, DFTrain$Job13, use = "complete.obs") 

#Attrition and EnvSat - AttES = -0.126
AttES <-cor(DFTrain$Attrition, DFTrain$EnvironmentSatisfaction, use = "complete.obs") 

#Attrition and EnvSat - AttJS = -0..094
AttJS <-cor(DFTrain$Attrition, DFTrain$JobSatisfaction, use = "complete.obs") 

#Attrition and NCW - AttJS = 0.058
AttNW <-cor(DFTrain$Attrition, DFTrain$NumCompaniesWorked, use = "complete.obs") 

#Attrition and NCW - AttJI = -0.150
AttJI <-cor(DFTrain$Attrition, DFTrain$JobInvolvement, use = "complete.obs") 

#Attrition and BusinessTravel - AttBT = -.01 not important
DFTrain$BusinessTravel<-as.integer(DFTrain$BusinessTravel)
AttBT <-cor(DFTrain$Attrition, DFTrain$BusinessTravel, use = "complete.obs") 

#Attrition and YearsSinceLastPromo - AttBT = -.007 not important
AttYSP <-cor(DFTrain$Attrition, DFTrain$YearsSinceLastPromotion, use = "complete.obs") 



RAN <- RA[which(RA$Attrition=="Yes"),]
ggplot(RAN, aes(x=JobRole, y=PercentAttrition)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage Attrition within JobRole, fig. X")+
  scale_fill_brewer()






#create a summary table for Attrtion by JobLevel
LevelAttritionCount <- data.frame(table(DFTotal$JobLevel, DFTotal$Attrition))
#Calculate percent attrition by role and add it as a new column
LA <- ddply(LevelAttritionCount, "Var1", transform,
            percent_attrition = Freq / sum(Freq) * 100)
colnames(LA)<-c("JobLevel", "Attrition", "Freq", "PercentAttrition")#set column names to improve readability
#Proportional Stacked Bar Graph of Attrition by Job Role
ggplot(LA, aes(x=JobLevel, y=PercentAttrition, fill=Attrition)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage Attrition by JobLevel, fig. X")+
  scale_fill_brewer()
#Just graph the Yes because it highlights the differences better
LAN <- LA[which(LA$Attrition=="Yes"),]
ggplot(LAN, aes(x=JobLevel, y=PercentAttrition)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage Attrition within JobLevel, fig. X")+
  scale_fill_brewer()

#This is too messy - find a different image
RoleLevelCount <- data.frame(table(DFTotal$JobRole, DFTotal$JobLevel))
RL <- ddply(RoleLevelCount, "Var1", transform,
            percent_attrition = Freq / sum(Freq) * 100)
colnames(RL)<-c("JobRole", "JobLevel", "Freq", "PercentofEachLevel")#set column names to improve readability
#Proportional Stacked Bar Graph of Attrition by Job Role
ggplot(RL, aes(x=JobRole, y=PercentofEachLevel, fill=JobLevel)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=FALSE)) + 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Percentage of Level by Job Role, fig. X")+
  scale_fill_brewer()

AttrY <- DFTrain[grep("Yes", DFTrain$Attrition), ]
AttrN <- DFTrain[grep("No", DFTrain$Attrition), ]

ggplot(AttrY, aes(x=JobRole)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")



#Compare Attrition Population to Total Population
RoleCount <- data.frame(sort(table(DFTotal$JobRole), decreasing=TRUE))
colnames(RoleCount)<- c("JobRole", "NumberOfEmployees")
p <- ggplot(RoleCount, aes(JobRole, NumberOfEmployees, fill=JobRole))+geom_bar(stat="identity", colour="black")
p + scale_fill_brewer(palette = "Blues") + 
  ggtitle("Distribution of Total Employee Population by JobRole, fig. X")+ 
  theme(legend.position = "none", axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))

#can we make this a stacked bar graph?
RoleAttritionCount <- data.frame(table(DFTotal$JobRole, DFTotal$Attrition))
colnames(RoleAttritionCount)<- c("JobRole", "Attrition","NumberOfEmployees")
RoleAttritionCount$Order<- c(5, 9, 3, 6, 4, 8, 2, 1, 7, 5, 9, 3, 6, 4, 8, 2, 1, 7)
RoleAttritionCount <- RoleAttritionCount[order(RoleAttritionCount$Order),]
RoleAttritionCount

p <- ggplot(RoleAttritionCount, aes(x=reorder(JobRole, -NumberOfEmployees), y=NumberOfEmployees, fill=Attrition))+geom_bar(stat="identity", colour="black")
p + scale_fill_brewer(palette = "Blues") + 
  ggtitle("Distribution of Employee Population by JobRole with Attrition Count")+ 
  theme(legend.position = "none", axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  xlab("Number of Employees") + ylab("JobRoles") 

#Now just for Attrtion Population
RoleCount <- data.frame(sort(table(AttrY$JobRole), decreasing=TRUE))
colnames(RoleCount)<- c("JobRole", "NumberOfAttrition")
p <- ggplot(RoleCount, aes(JobRole, NumberOfAttrition, fill=JobRole))+geom_bar(stat="identity", colour="black")
p + scale_fill_brewer(palette = "Blues") + 
  ggtitle("Distribution of Attrition Employees by JobRole, fig. X")+ 
  theme(legend.position = "none", axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  xlab("Number of Employees") + ylab("JobRoles") 



#RACOrder<-cbind.data.frame(RoleAttritionCount$JobRole,RoleAttritionCount$Attrition, RoleAttritionCount$NumberOfEmployees )
#colnames(RACOrder)<- c("JobRole", "Attrition","NumberOfEmployees")

# #levels(RoleAttritionCount$JobRole) <- c("1 Sales Executive", "2 Research Scientist", "3 Laboratory Technician",  
#                                         "4 Manufacturing Director",  "5 Healthcare Representative", "6 Manager", 
#                                         "7 Sales Representative",  "8 Research Director", 
#                                         "9 Human Resources")
# 
# #RoleAttritionCount <- within(RoleAttritionCount, 
#                              JobRole <- factor(JobRole, 
#                                                levels=c("Sales Executive", "Research Scientist", "Laboratory Technician",  
#                                                         "Manufacturing Director",  "Healthcare Representative", "Manager", 
#                                                         "Sales Representative",  "Research Director", 
#                                                         "Human Resources")))
# 
# #RoleAttritionCount%>%
#   arrange(match(JobRole, c("Sales Executive", "Research Scientist", "Laboratory Technician",  
#                            "Manufacturing Director",  "Healthcare Representative", "Manager", 
#                            "Sales Representative",  "Research Director", 
#                            "Human Resources")))
# 



#Plot of Happiness for Entire Company by Department
qplot(Department, Happiness, geom="boxplot", data=DFTrain)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments, fig. X")+
  scale_fill_brewer()

#Plot of Happiness for Entire Company by Job Role
qplot(JobRole, Happiness, geom="boxplot", data=DFTrain)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments, fig. X")+
  scale_fill_brewer()

#Plot of Happiness for AttritionY by Department
qplot(Department, Happiness, geom="boxplot", data=AttrY)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments, fig. X")+
  scale_fill_brewer()

#Plot of Happiness for AttritionY by Job Role
qplot(JobRole, JobSatisfaction, geom="boxplot", data=AttrY)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments, fig. X")+
  scale_fill_brewer()

AttrY$JobLevel <- as.factor(AttrY$JobLevel)
#Plot of Happiness for AttritionY by Level
qplot(JobLevel, Happiness, geom="boxplot", data=AttrY)+ 
  theme(axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5))+ 
  ggtitle("Cumulative Happiness Score Across Departments, fig. X")+
  scale_fill_brewer()


#Plot of Number of people in each role
gplot(DFTrain, aes(JobRole, NumberOfBeers, fill=Style)) + 
  geom_bar(stat = "identity") + ggtitle("Total Beers Produced by Style, fig. 3") + 
  theme(legend.position = "none", axis.text.x=element_text(angle = 90,vjust = 0), plot.title = element_text(hjust = 0.5)) + ylab("Total Beers by Style")


#Confirm that there is not correlation between Satisfactions
DFCorr <- subset(DFTrain, select=c(RelationshipSatisfaction, EnvironmentSatisfaction, JobSatisfaction, JobInvolvement))
DFCorr %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

ggpairs(DFTrain[c(3,37)])#WTF is Rand - does it mater? Doesn't look like it.

ggpairs(DFTrain[c(3,33)])#Attrition with Happiness
#Happiness is a lumpy plot, median Yes Happiness equals the bottom quartile of the No attrition
#Create a dissecting variable for Happiness under 10

ggpairs(DFTrainFS[c(2,3,4, 6)])#Travel No, Department Sales Maybe,  Education No
ggpairs(DFTrainFS[c(2,9,11,14)])#Hourly No, Job Level Yes, Marital Status Yes
ggpairs(DFTrainFS[c(2,11,14, 15)])#Monthly Rate No
ggpairs(DFTrainFS[c(2,11,14,16)])#NumCompaniesWorked yes if > 3 
ggpairs(DFTrainFS[c(2,11,14,17)])#Overtime Yes
ggpairs(DFTrainFS[c(2,17, 24)])#Overtime versus Worklife balance
ggpairs(DFTrainFS[c(2,21, 17, 30, 31)])#Stock Option Level Yes if =<1, Happiness<12, Years<2 


ggpairs(DFTrainFS[c(2,21, 14)])#Stock Option Level Yes if =<1, Marital Status, Years<2 

#```{r Attrition with Income/Rates Graphs}
## The following plots compare Income rates given with Attrition
g1 <- ggplot(DFTrainRel, aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#fdb462"))
g2 <- ggplot(DFTrainRel, aes(x = MonthlyRate, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#fdb462"))
g3 <- ggplot(DFTrainRel, aes(x = HourlyRate, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#fdb462"))
g4 <- ggplot(DFTrainRel, aes(x = DailyRate, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#fdb462"))
OTg <- ggplot(DFTrainRel, aes(x = OTInt, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#fdb462"))
## Box Plot of Job Roles compared to OT grouped by Attrition 
##DFTrainRel$OTInt <- as.integer(DFTrainRel$OverTime)
###OTg2 <- ggplot(DFTrainRel, aes(x = JobRole, y = OTInt, fill = Attrition)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("#386cb0", "#ffa500")) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)
#```

#```{r Attrition More Graphs}
## Total Counts of Attrition Yes VS No
g6 <- ggplot(DFTrainRel, aes(x = Attrition)) + geom_histogram(stat="count", position = "stack", fill = c("#386cb0","#ffa500"))
## Distance from home related to Attrition Density
g1 <- ggplot(DFTrainRel, aes(x = DistanceFromHome, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
## Job Satisfaction related to Attrition Density
g2 <- ggplot(DFTrainRel, aes(x = JobSatisfaction, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
## Percentage of Salary Hike related to Attrition Density
g3 <- ggplot(DFTrainRel, aes(x = PercentSalaryHike, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
## Environment Satisfaction related to Attrition Density
g4 <- ggplot(DFTrainRel, aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
## Work Life Balance related to Attrition Density
g7 <- ggplot(DFTrainRel, aes(x = WorkLifeBalance, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
## Years at the company related to Attrition Density
g8 <- ggplot(DFTrainRel, aes(x = YearsAtCompany, fill = Attrition)) + geom_density(alpha = 0.5) + scale_fill_manual(values = c("#386cb0", "#ffa500"))
grid.arrange(g1, g2, g3, g4, g7, g8, ncol = 2, nrow = 3)
## Count of Attrition Stacked by Job Role
g5 <- ggplot(DFTrainRel, aes(x = JobRole, fill = Attrition)) + geom_histogram(stat="count", position = "stack") + scale_fill_manual(values = c("#386cb0", "#ffa500")) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
## Box Plot of Job Roles compared to Monthly Income grouped by Attrition 
ggplot(DFTrainRel, aes(JobRole, MonthlyIncome,  fill = Attrition)) + geom_boxplot(alpha = 0.5) + coord_flip() + scale_fill_manual(values = c("#386cb0", "#ffa500"))
grid.arrange(g5, g6, ncol = 2, nrow = 1)

ggplot(DFTrainRel, aes(JobRole, MonthlyIncome,  fill = Attrition)) + geom_boxplot(alpha = 0.5) + coord_flip() + scale_fill_manual(values = c("#386cb0", "#ffa500"))
grid.arrange(g5, g6, ncol = 2, nrow = 1)



#```

#Boxplots
#```{r Plots}
plot_str(DFTrainRel)
plot_histogram(DFTrainRel)
#Boxplots of each continuous variable against Attrition
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = Age)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = JobSatisfaction)) +
  geom_boxplot() #JobSatisfaction of 1
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = DailyRate)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = DistanceFromHome)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = Education)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = EnvironmentSatisfaction)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = Gender)) +
  geom_boxplot() #EnvoronmentSatisfaction=1
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = HourlyRate)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = JobInvolvement)) +
  geom_boxplot()
ggplot(data = CS2Data, mapping = aes(x = Attrition, y = JobLevel)) +
  geom_boxplot()#level 1
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = MonthlyIncome)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = MonthlyRate)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = NumCompaniesWorked)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = PercentSalaryHike)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = PerformanceRating)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = RelationshipSatisfaction)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = StockOptionLevel)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = TotalWorkingYears)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = TrainingTimesLastYear)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = WorkLifeBalance)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = YearsAtCompany)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = YearsInCurrentRole)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = YearsSinceLastPromotion)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = YearsWithCurrManager)) +
  geom_boxplot()
ggplot(data = DFTrainRel, mapping = aes(x = Attrition, y = OverTime)) +
  geom_boxplot()
DFTrainCorr <- cbind.data.frame(DFTrainRel$Attrition, DFTrainRel$EnvironmentSatisfaction, DFTrainRel$JobInvolvement, DFTrainRel$JobLevel, DFTrainRel$JobRole, DFTrainRel$JobSatisfaction, DFTrainRel$PerformanceRating, DFTrainRel$RelationshipSatisfaction, DFTrainRel$StockOptionLevel, DFTrainRel$WorkLifeBalance)
#not working
#featurePlot(x = DFTrainCorr[, 2:5], y = DFTrainCorr$Attrition, plot = "pairs",
## Add a key at the top auto.key = list(columns = 3))
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
```