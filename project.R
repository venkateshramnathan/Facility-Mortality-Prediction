#####Code to run

###Open the dataset
##Load the dataset
getwd()
setwd("C:/Users/user/Desktop/Project")
mydata = read.csv("DATASET.csv")


##Drop unwanted columns
mydata1<- mydata[,-c(1:4,7:13, 16, 22:25,67:69)]

##Have a first glance of our dataset
summary(mydata1)

##remove rows where transfusion code are either 199 201 and 258
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
###Remove and drop variables that are not significant

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
null=lm(Standardized.mortality.ratio~1, data = CleanData)
full=lm(CleanData$Standardized.mortality.ratio~., data = CleanData)
##forward variable selection
install.packages("stargazer") 
library(stargazer)
m1<-step(null, scope=list(lower=null, upper=full), direction="forward")
stargazer(m1, type = "text", out="models_x.txt")
###Backward selection didnt return any step(full, data=Housing, direction="backward")
m2<- step(full, data=CleanData, direction = "backward")
stargazer(m2, type = "text", out="models_1.txt")
###stepwise
#step(null, scope = list(upper=full), data=Housing, direction="both")
options(show.signif.stars=F) 
m3<-step(null, scope=list(upper=full), data=CleanData, direction="both")
stargazer(m3, type = "text", out="models_2.txt")
######## D2 ######
summary(CleanData$Offers.home.hemodialysis.training)
sd(CleanData$Offers.home.hemodialysis.training)
x=CleanData$Offers.home.hemodialysis.training
Offers.home.hemodialysis.training=table(x)
barplot(Offers.home.hemodialysis.training)

summary(CleanData$Standardized.hospitalization.ratio)
sd(CleanData$Standardized.hospitalization.ratio)
x=CleanData$Standardized.hospitalization.ratio
Standardized.hospitalization.ratio=table(x)
hist(Standardized.hospitalization.ratio), main ="Standardized Hospitalization Ratio 
     Frequency", xlab= "Standardized Hospitalization Ratio" 	
     
     summary(CleanData$Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.)
     sd(CleanData$Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.)
     x=CleanData$Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.
     Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.=table(x)
       hist(Lists.the.number.of.patients.included.in.the.facility.s.hypercalcemia.summary..facility.)
     
    barplot(Offers.home.hemodialysis.training)
     
          
          #####didier#####
    hist(CleanData$Number.of.hospitalizations.included.in.hospital.readmission..facility.,
         main="Number of Hospitalizations",
         xlab = "Number of Hospitalizations included in Hospital Readmission")
    
    Barplot(CleanData$Offers.home.hemodialysis.training,
    main = "offers home hemodialysis training frequency" , xlab = "offers home hemodialysis training ratio")
     
    barplot(table(CleanData$Offers.home.hemodialysis.training),
            main = "Facility Offering Home Hemodialysis Training",
            xlab = "Offers Home Hemodialysis Training")
    
    
    dataPair<- CleanData[,c(30,27,9,20,64,10)]
    cor(dataPair)
    


