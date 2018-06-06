##Final Dataset script
###Open the dataset
##Load the dataset
setwd("/Users/dndengue/Documents/IS698")
mydata = read.csv("DFC_SOCRATA_FAC_DATA.csv")
##Drop unwanted columns
mydata1<- mydata[,-c(1:4,7:13, 16, 22:25,67:69)]

##Have a first glance of our dataset
summary(mydata1)

##remove rows where transfusion code are either 199 201 or 258
temp_data <- subset(mydata1, (!(mydata1$Patient.transfusion.data.availability.Code) %in% 199))

temp_data1 <- subset(temp_data, (!(temp_data$Patient.transfusion.data.availability.Code) %in% 201))

temp_data <- subset(temp_data1, (!(temp_data1$Patient.transfusion.data.availability.Code) %in% 258))


##take a look at the data with NAs Mortality rate to understand what is going on
#emptyMortality <- subset(temp_data, is.na(temp_data$Standardized.mortality.ratio))

##look at the number of case where hemoglobin 10 and 12 are not missing

hemoglo <- subset(temp_data, (!(temp_data$Hgb..10.data.availability.code) %in% 199))
hemoglo1 <- subset(hemoglo, (!(hemoglo$Hgb..10.data.availability.code) %in% 201))
hemoglo2 <- subset(hemoglo1, (!(hemoglo1$Hgb..10.data.availability.code) %in% 258))

##There are only 15 rows with NAs for mortality ratio, so we can delete them
summary(hemoglo2$Standardized.mortality.ratio)

CleanData <- subset(hemoglo2, !is.na(hemoglo2$Standardized.mortality.ratio))
##Check we dont have NA in mortality rate
summary(CleanData$Standardized.mortality.ratio)

##Summary of our clean dataset
summary(CleanData)

##List the columns with in descending order
##   Five.star 117
#Percent.of.adult.HD.patients.with.Kt.V....1.2  109
#Percentage.of.adult.PD.PTS.with.Kt.V....1.7  4044
#Percentage.of.pediatric.HD.patients.with.Kt.V....1.2  5246
##Percentage.of.patients.with.arteriovenous.fistulae.in.place  77
##Percentage.of.patients.with.vascular.catheter.in.use.for.90.days.or.longer 77
# Lists.the.percentage.of.adult.patients.with.hypercalcemia..serum.calcium.greater.than.10.2.mg.dL...facility. 10
#Lists.the.percentage.of.adult.patients.with.serum.phosphorus.less.than.3.5.mg.dL..facility. 13
# Lists.the.percentage.of.adult.patients.with.serum.phosphorus.between.3.5.4.5.mg.dL..facility. 13
#Lists.the.percentage.of.adult.patients.with.serum.phosphorus.between.4.6.5.5.mg.dL..facility. 13
# Lists.the.percentage.of.adult.patients.with.serum.phosphorus.between.5.6.7.0.mg.dL..facility. 13
#Lists.the.percentage.of.adult.patients.with.serum.phosphorus.greater.than.7.0.mg.dL..facility.  13
# SRR..upper.confidence.limit  14
#SRR..lower.confidence.limit 14
#Standardized.readmission.ratio 14
## It is interesting to look more in details for the facilities with 13, 14 and 77 NAs 
###############################################################
##We are going to drop the folowing columns
#Percentage.of.pediatric.HD.patients.with.Kt.V....1.2
#Percentage.of.adult.PD.PTS.with.Kt.V....1.7


###Remove columns that have many missing values
#this needs to be redone
CleanData<- CleanData[,-c(23,25)]
##remove the missing the 13 data that are missing 
temp <- subset(CleanData, !is.na(CleanData$Lists.the.percentage.of.adult.patients.with.serum.phosphorus.greater.than.7.0.mg.dL..facility.))
##look into the facilities that dont have any stars
CleanData <- temp
CleanData <- subset(CleanData, !is.na(CleanData$Five.star))
##look into the facilities that Standardized.readmission.ratio iis NA'
CleanData <- subset(CleanData, !is.na(CleanData$Standardized.readmission.ratio))

#Percentage.of.patients.with.arteriovenous.fistulae.in.place'
CleanData <- subset(CleanData, !is.na(CleanData$Percentage.of.patients.with.arteriovenous.fistulae.in.place))
#Lists.the.percentage.of.adult.patients.with.hypercalcemia..serum.calcium.greater.than.10.2.mg.dL...facility.
CleanData <- subset(CleanData, !is.na(CleanData$Lists.the.percentage.of.adult.patients.with.hypercalcemia..serum.calcium.greater.than.10.2.mg.dL...facility.))
##Percent.of.adult.HD.patients.with.Kt.V....1.2
CleanData <- subset(CleanData, !is.na(CleanData$Percent.of.adult.HD.patients.with.Kt.V....1.2))

summary(CleanData)

##Get training and test data 75% training 25% test
sub <- sample(nrow(CleanData), floor(nrow(CleanData) * 0.75))
training <- CleanData[sub, ]
testing <- CleanData[-sub, ]


####Perform a stepwise variable selection
null=lm(Standardized.mortality.ratio~1, data = training)
full=lm(training$Standardized.mortality.ratio~., data = training)
##forward variable selection
#install.packages("stargazer") 
#library(stargazer)
m1<-step(null, scope=list(lower=null, upper=full), direction="forward")
stargazer(m1, type = "text", out="models_x.txt")
###Backward selection didnt return any step(full, data=Housing, direction="backward")
m2<- step(full, data=training, direction = "backward")
stargazer(m2, type = "text", out="models_1.txt")
###stepwise
#step(null, scope = list(upper=full), data=Housing, direction="both")
options(show.signif.stars=F) 
m3<-step(null, scope=list(upper=full), data=training, direction="both")
stargazer(m3, type = "text", out="models_2.csv")

#dataPair<- CleanData[,c(30,27,9,20,64,10,61,38,62)]
dataPair<- CleanData[,c(30,27,9,20,64,10)]
pairs(dataPair)
cor(dataPair)

#[1] "Number.of.pediatric.HD.patient.months.with.Kt.V.data"                                  
##[2] "Number.of.adult.PD.patients.with.Kt.V.data"                                            
#[3] "Offers.home.hemodialysis.training"                                                     
#[4] "Lists.the.number.of.patients.included.in.the.facility.s.transfusion.summary..facility."
#[5] "Standardized.hospitalization.ratio"                                                    
#[6] "Percentage.of.Medicare.patients.with.Hgb.10.g.dL"  
##Build the linear regression
lm.fit = lm(Standardized.mortality.ratio ~ 
              SMR..upper.confidence.limit..95.. +Number.of.patients.included.in.survival.summary  +
              Lists.the.number.of.patients.included.in.the.facility.s.serum.phosphorus.summary..facility. +
              Lists.the.number.of.patients.included.in.the.facility.s.transfusion.summary..facility. + 
              Lists.the.lower.confident.limit..2.5...for.standardized.transfusion.ratio..STRr.+
              Lists.the.facility.s.standardized.transfusion.ratio..facility.  + 
              Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.
              , data = CleanData)

summary(lm.fit)



tempData <- CleanData[,c(51,63,41,20,15,38,17)]
#Descriptive analysis
attach(CleanData)
summary(Lists.the.lower.confident.limit..2.5...for.standardized.transfusion.ratio..STRr.)
sd(Lists.the.lower.confident.limit..2.5...for.standardized.transfusion.ratio..STRr.)

#histogram

hist(Number.of.patients.included.in.survival.summary,main = "Number of Patients included in Survival Summary Frequency",xlab = "Number of Patients included in Survival Summary")

hist(Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.,main = "Number of Patients included in the facility's hypercalcemia Summary Frequency",xlab = "Number of Patients included in the facility's hypercalcemia Summary")


hist(Lists.the.lower.confident.limit..2.5...for.standardized.transfusion.ratio..STRr.,main = "List the Lower confident limit 2.5 for Standardized Transfusion Ratio")

##Get the correlation Matrices and Pairs Plots
tempData <- CleanData[,c(51,63,41,20,15,38,17,66)]
cor(tempData)
pairs(tempData)
names(lm.fit)
confint(lm.fit)

predicted_2 <- 2.345e-02 + (6.884e-01*1.38)+ (7.556e-04*416) + (1.061e-03*94) + (-1.392e-03*75) + (4.119e-01*0.39) + (-2.262e-01*0.83) + (-2.535e-03*88)

plot(lm.fit)

