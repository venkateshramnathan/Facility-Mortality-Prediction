##Final Dataset script
###Open the dataset
##Load the dataset
options(show.signif.stars=F) 
#install.packages("stargazer") 
library(stargazer)
library(ISLR)
library(boot)
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

CleanData <- subset(hemoglo2, !is.na(hemoglo2$Standardized.mortality.ratio))


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
CleanData1 <- CleanData
###Rename Columns
colnames(CleanData)[1] <- 'Rating'
colnames(CleanData)[2] <- 'Rating Code'
colnames(CleanData)[3] <- 'Profit/Non Profit'
colnames(CleanData)[4] <- 'Chain Owned'
colnames(CleanData)[5] <- 'Late Shift'
colnames(CleanData)[6] <- 'Nber Dialysis Stations'
colnames(CleanData)[7] <- 'Offers in Center Hemodialysis'
colnames(CleanData)[8] <- 'Offers in center peritoneal dialysis'
colnames(CleanData)[9] <- 'Offers Home Hemodialysis training'
colnames(CleanData)[10] <- '% of Medicare Patients with Hgb 10gdl'
colnames(CleanData)[11] <- 'HGB 10 data availability code'
colnames(CleanData)[12] <- '% of Medicare patients with hgb 12gdl'
colnames(CleanData)[13] <- 'HGB 12 data availability code'
colnames(CleanData)[14] <- 'Nber of dialysis patients with Hgb data'
colnames(CleanData)[15] <- 'Facilities Transfusion Ratio'
colnames(CleanData)[16] <- 'Transf Ratio Upper conf Limit 97.5 '
colnames(CleanData)[17] <- 'Transf Ratio Lower conf Limit 2.5'
colnames(CleanData)[18] <- 'Patient transf category'
colnames(CleanData)[19] <- 'number of patients in transf '
colnames(CleanData)[20] <- '% of adult HD Patients with Ktv 1.2'
colnames(CleanData)[21] <- 'Adult HD Kt/V data availab code'
colnames(CleanData)[22] <- 'Adult PD Kt/V data availab code'
colnames(CleanData)[23] <- 'Pedi HD Kt/V Data Availab Code'
colnames(CleanData)[24] <- 'Nber of adult HD patients w/ Ktv data'
colnames(CleanData)[25] <- 'Nber of adult HD patients months w/ Ktv data'
colnames(CleanData)[26] <- 'Nber of adult HD adult w/ Ktv data'
colnames(CleanData)[27] <- 'Nber of adult HD adult months w/ Ktv data'
colnames(CleanData)[28] <- 'Nber of adult HD pedi w/ Ktv data'
colnames(CleanData)[29] <- 'Nber of adult HD pedi months w/ Ktv data'
colnames(CleanData)[30] <- '% of patients w/ arteriovenous fistulae in place'
colnames(CleanData)[31] <- 'arteriovenous fistulae in place data availab code'
colnames(CleanData)[32] <- '% of patients w/ vascular cath in use for 90 days or longer'
colnames(CleanData)[33] <- 'Vascular cath data availab code'
colnames(CleanData)[34] <- 'Nber of adult patient inc in arterial venous fistula and cath'
colnames(CleanData)[35] <- 'numb of adult patient months inc in arterial venous fistula and cath'
colnames(CleanData)[36] <- 'hypercalcemia data availab'
colnames(CleanData)[37] <- 'numb of patient inc in the facility hypercalcemia sum facility'
colnames(CleanData)[38] <- 'numb of patient months inc in the facility hypercalcemia'
colnames(CleanData)[39] <- '% of adult patients w/ hypercalcemia serum calcium > 10.2mg/dl'
colnames(CleanData)[40] <- 'numb of patients inc in the facility serum phosp'
colnames(CleanData)[41] <- 'numb of patient months inc in the facility serum phosp'
colnames(CleanData)[42] <- 'serum phosp data availab code'
colnames(CleanData)[43] <- '% of adult patients with serum phosp < 3.5mg/dl'
colnames(CleanData)[44] <- '% of adult patients w/ serum phosp 3.5-4.5mg/dl'
colnames(CleanData)[45] <- '% of adult patients w/ serum phosp 4.6-5.5mg/dl'
colnames(CleanData)[46] <- '% of adult patients w/ serum phosp 5.6-7.0mg/dl'
colnames(CleanData)[47] <- '% of adult patients w/ serum phosp > 7.0mg/dl'
colnames(CleanData)[48] <- 'SHR upper confi limit 95'
colnames(CleanData)[49] <- 'SRR upper confi limit'
colnames(CleanData)[50] <- 'SMR upper confi limit 95'
colnames(CleanData)[51] <- 'SHR lower confi limit 5'
colnames(CleanData)[52] <- 'Stand Hospitalization Ratio Lower conf Limit 2.5'
colnames(CleanData)[53] <- 'Stand Readmission Ratio Lower conf Limit 2.5'
colnames(CleanData)[54] <- 'Stand Mortality Ratio Lower conf Limit 2.5'
colnames(CleanData)[55] <- 'Patient Hospitalization Category txt'
colnames(CleanData)[56] <- 'Patient Hospitalization Data Availability Code'
colnames(CleanData)[57] <- 'Hospital Readmission Category'
colnames(CleanData)[58] <- 'Patient Hospital Readmission data availability Code'
colnames(CleanData)[59] <- 'Patient Survival Category txt'
colnames(CleanData)[60] <- 'Patient Hospital Survival data availability Code'
colnames(CleanData)[61] <- 'Nber of patients included in hospitalization'
colnames(CleanData)[62] <- 'Nber of hospitalizations included in hospital readmission '
colnames(CleanData)[63] <- 'Nber of Patients included in Survival Summary'
colnames(CleanData)[64] <- 'Standardized Hospitalization Ratio'
colnames(CleanData)[65] <- 'Standardized Readmission Ratio'
colnames(CleanData)[66] <- 'Standardized Mortality Ratio'
summary(CleanData)

##Drop columns that do not gives meaningful result
NewData<- CleanData[,-c(11,13,16, 17, 21,22,23, 36, 42, 48, 49, 50, 51, 52, 53, 54, 56, 58, 60)]
##convert categorical text to numeric value
##`Profit/Non Profit`  Change Profit = 1  Non Profit = 0
CleanData$`Profit/Non Profit` <- ifelse(CleanData$`Profit/Non Profit` == 'Profit',1,0)
##`Chain Owned` Yes= 1   No=0

CleanData$`Chain Owned` <- ifelse(CleanData$`Chain Owned` == 'Yes',1,0)
##`number of patients in transf ` As Expected = 0  Worse than Expected = 1 Better than Expected = 3

CleanData$`number of patients in transf ` <- ifelse(CleanData$`number of patients in transf `=='As Expected', 0,(ifelse(CleanData$`number of patients in transf `=='Worse than Expected', 1,2)))

##`Patient Hospitalization Category txt`  As Expected = 0  Worse than Expected = 1 Better than Expected = 3

CleanData$`Patient Hospitalization Category txt` <- ifelse(CleanData$`Patient Hospitalization Category txt`=='As Expected', 0,(ifelse(CleanData$`Patient Hospitalization Category txt`=='Worse than Expected', 1,2)))

##`Hospital Readmission Category`     As Expected = 0  Worse than Expected = 1 Better than Expected = 3


CleanData$`Hospital Readmission Category` <- ifelse(CleanData$`Hospital Readmission Category`=='As Expected', 0,(ifelse(CleanData$`Hospital Readmission Category`=='Worse than Expected', 1,2)))

##`Patient Survival Category txt`    As Expected = 0  Worse than Expected = 1 Better than Expected = 3


CleanData$`Patient Survival Category txt` <- ifelse(CleanData$`Patient Survival Category txt`=='As Expected', 0,(ifelse(CleanData$`Patient Survival Category txt`=='Worse than Expected', 1,2)))

## need to factor here
CleanData$`Profit/Non Profit` <- as.factor(CleanData$`Profit/Non Profit`)
CleanData$`number of patients in transf ` <- as.factor(CleanData$`number of patients in transf `)
CleanData$`Chain Owned` <- as.factor(CleanData$`Chain Owned`)
CleanData$`Patient Hospitalization Category txt` <- as.factor(CleanData$`Patient Hospitalization Category txt`)
CleanData$`Hospital Readmission Category` <- as.factor(CleanData$`Hospital Readmission Category`)
CleanData$`Patient Survival Category txt` <- as.factor(CleanData$`Patient Survival Category txt`)

x <- c(1,2,3,4,5,6,7,8,9,10)
##a. the entire data set as the training data.
MSE.error = rep(0,10)
for (i in 1:10){lm.fit = lm(CleanData$`Standardized Mortality Ratio` ~ 
                              poly(CleanData$`SHR lower confi limit 5`,i) +
                              poly(CleanData$`Nber of Patients included in Survival Summary`,i) +
                              poly( CleanData$`% of patients w/ arteriovenous fistulae in place`,i) +
                              poly(CleanData$`% of adult HD Patients with Ktv 1.2`,i)  +
                              poly(CleanData$`Nber of patients included in hospitalization`,i)  +
                              poly(CleanData$`Standardized Hospitalization Ratio`,i)+
                              poly(CleanData$`Transf Ratio Lower conf Limit 2.5`,i) +
                              poly(CleanData$`Facilities Transfusion Ratio`,i)+
                              poly(CleanData$`SRR upper confi limit`,i)+
                              poly(CleanData$`Stand Hospitalization Ratio Lower conf Limit 2.5`,i)+
                              poly(CleanData$`Nber of dialysis patients with Hgb data`,i) , data = CleanData)
MSE.error[i] = mean((`Standardized Mortality Ratio` - predict(lm.fit,CleanData))^2)}

df4 <- data.frame(x, MSE.error)
plot(df4, xlab="Degree of Polynomial", ylab= "Mean Square Error", main="Dataset as Training")



###B- Validation Approach

set.seed (1)
train=sample(3780,1269)
MSE.error1 = rep(0,10)
for (i in 1:10){lm.fit = lm(CleanData$`Standardized Mortality Ratio` ~ 
                              poly(CleanData$`SHR lower confi limit 5`,i) +
                              poly(CleanData$`Nber of Patients included in Survival Summary`,i) +
                              poly( CleanData$`% of patients w/ arteriovenous fistulae in place`,i) +
                              poly(CleanData$`% of adult HD Patients with Ktv 1.2`,i)  +
                              poly(CleanData$`Nber of patients included in hospitalization`,i)  +
                              poly(CleanData$`Standardized Hospitalization Ratio`,i)+
                              poly(CleanData$`Transf Ratio Lower conf Limit 2.5`,i) +
                              poly(CleanData$`Facilities Transfusion Ratio`,i)+
                              poly(CleanData$`SRR upper confi limit`,i)+
                              poly(CleanData$`Stand Hospitalization Ratio Lower conf Limit 2.5`,i)+
                              poly(CleanData$`Nber of dialysis patients with Hgb data`,i) , data = CleanData,  subset = train)
MSE.error[i] = mean((`Standardized Mortality Ratio` - predict(lm.fit,CleanData))^2)}

df4 <- data.frame(x, MSE.error)
plot(df4, xlab="Degree of Polynomial", ylab= "Mean Square Error", main="Validation Approach")


# C- LOOCV Approach
cv.error=rep(0,10)

for (i in 1:10){glm.fit2 = glm(CleanData$`Standardized Mortality Ratio` ~ 
               poly(CleanData$`SHR lower confi limit 5`, i) +
               poly(CleanData$`Nber of Patients included in Survival Summary`,i) +
               poly( CleanData$`% of patients w/ arteriovenous fistulae in place`,i) +
               poly(CleanData$`% of adult HD Patients with Ktv 1.2`,i)  +
               poly(CleanData$`Nber of patients included in hospitalization`,i)  +
               poly(CleanData$`Standardized Hospitalization Ratio`,i)+
               poly(CleanData$`Transf Ratio Lower conf Limit 2.5`,i) +
               poly(CleanData$`Facilities Transfusion Ratio`,i)+
               poly(CleanData$`SRR upper confi limit`,i)+
               poly(CleanData$`Stand Hospitalization Ratio Lower conf Limit 2.5`,i)+
               poly(CleanData$`Nber of dialysis patients with Hgb data`,i), data = CleanData)
               cv.error[i]=cv.glm(CleanData,glm.fit2)$delta[1]}
df2 <- data.frame(x, cv.error)
plot(df2, xlab="Degree of Polynomial", ylab= "Mean Square Error", main="LOOCV Approach")



###D - K-Fold Cross Validation
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){glm.fit3 = glm(CleanData$`Standardized Mortality Ratio` ~ 
                                 poly(CleanData$`SHR lower confi limit 5`, i) +
                                 poly(CleanData$`Nber of Patients included in Survival Summary`,i) +
                                 poly( CleanData$`% of patients w/ arteriovenous fistulae in place`,i) +
                                 poly(CleanData$`% of adult HD Patients with Ktv 1.2`,i)  +
                                 poly(CleanData$`Nber of patients included in hospitalization`,i)  +
                                 poly(CleanData$`Standardized Hospitalization Ratio`,i)+
                                 poly(CleanData$`Transf Ratio Lower conf Limit 2.5`,i) +
                                 poly(CleanData$`Facilities Transfusion Ratio`,i)+
                                 poly(CleanData$`SRR upper confi limit`,i)+
                                 poly(CleanData$`Stand Hospitalization Ratio Lower conf Limit 2.5`,i)+
                                 poly(CleanData$`Nber of dialysis patients with Hgb data`,i), data = CleanData)
cv.error.10[i]=cv.glm(CleanData,glm.fit3 , K=10)$delta[1]}


df3 <- data.frame(x, cv.error.10)
plot(df3, xlab="Degree of Polynomial", ylab= "Mean Square Error", main="10-Fold Cross Validation")

##Bootstrapping

rsq <- function(formula, data, indices){
  d<- data[indices, ]
  fit <- lm(formula, data=d)
  return (summary(fit)$r.square)
}

##Bootstrapping with 1000 replications
results <- boot(data=CleanData, statistic = rsq, R=1000, formula =`Standardized Mortality Ratio`~ `SHR lower confi limit 5` +
                  `Nber of Patients included in Survival Summary` +
                  `% of patients w/ arteriovenous fistulae in place` +
                  `% of adult HD Patients with Ktv 1.2`  +
                  `Nber of patients included in hospitalization`  +
                  `Standardized Hospitalization Ratio`+
                  `Transf Ratio Lower conf Limit 2.5` +
                  `Facilities Transfusion Ratio`+
                  `SRR upper confi limit`+
                  `Stand Hospitalization Ratio Lower conf Limit 2.5`+
                  `Nber of dialysis patients with Hgb data` )
#View result
results
#plot the result
plot(results)

#Get a 95% confidence interval
boot.ci(results, type = "bca")

#lasso
install.packages("glmnet")
set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]
x=model.matrix(Standardized.mortality.ratio ~.,CleanData)[,-1]
y=CleanData$Standardized.mortality.ratio
lasso.mod =glmnet(x[train ,],y[train],alpha =1,lambda=seq(0,10,by=1))
plot(lasso.mod)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet (x,y,alpha =1, lambda =seq(0,10,by=1))
lasso.coef=predict(out,type="coefficients",s=bestlam )[1:66 ,]
lasso.coef




