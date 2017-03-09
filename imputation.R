library(xgboost)
library(Matrix)
library(ggplot2)
library(readr)
library(mice)

train <- read_csv("train.csv")
test <- read_csv("test.csv")

train.y <- train$Business_Sourced
train$Business_Sourced <- NULL

all <- rbind(train, test)
all$ID <- NULL

###Convert all date fields from character to dates.
all$Application_Receipt_Date <- as.Date(all$Application_Receipt_Date, "%m/%d/%Y")
all$Applicant_BirthDate <- as.Date(all$Applicant_BirthDate, "%m/%d/%Y")
all$Manager_DOJ <- as.Date(all$Manager_DOJ, "%m/%d/%Y")
all$Manager_DoB <- as.Date(all$Manager_DoB, "%m/%d/%Y")

###convert all factor fields from character to factor.
all$Applicant_Gender <- as.factor(all$Applicant_Gender)
all$Applicant_Marital_Status <- as.factor(all$Applicant_Marital_Status)
all$Applicant_Occupation <- as.factor(all$Applicant_Occupation)
all$Applicant_Qualification <- as.factor(all$Applicant_Qualification)
all$Manager_Joining_Designation <- as.factor(all$Manager_Joining_Designation)
all$Manager_Current_Designation <- as.factor(all$Manager_Current_Designation)
all$Manager_Status <- as.factor(all$Manager_Status)
all$Manager_Gender <- as.factor(all$Manager_Gender)
all$Manager_Grade <- as.factor(all$Manager_Grade)

### Convert date fields into numeric for ease of imputation

all$Application_Receipt_Date <- as.numeric(all$Application_Receipt_Date)
all$Applicant_BirthDate <- as.numeric(all$Applicant_BirthDate)
all$Manager_DOJ <- as.numeric(all$Manager_DOJ)
all$Manager_DoB <- as.numeric(all$Manager_DoB)

#Imputation of missing data                                                                                                          
impdata1 <- mice(subset(all, select = c(1:3, 5, 9, 15:21)), m = 5, maxit = 5, method = 'pmm')
impdata1 <- complete(impdata1, 5)
impdata2 <- mice(subset(all, select = c(4, 6:8, 10:14)), m = 5, maxit = 5, method = 'polyreg')
impdata2 <- complete(impdata2, 5)

newdata <- cbind(impdata1, impdata2)

### Convert back date fields
newdata$Application_Receipt_Date <- as.Date(newdata$Application_Receipt_Date, origin = "1970-01-01")
newdata$Applicant_BirthDate <- as.Date(newdata$Applicant_BirthDate, origin = "1970-01-01")
newdata$Manager_DOJ <- as.Date(newdata$Manager_DOJ, origin = "1970-01-01")
newdata$Manager_DoB <- as.Date(newdata$Manager_DoB, origin = "1970-01-01")


newdata$Applicant_Age <- as.numeric(round((newdata$Application_Receipt_Date - newdata$Applicant_BirthDate)/365, 0))
newdata$Manager_Age <- as.numeric(round((newdata$Application_Receipt_Date - newdata$Manager_DoB)/365, 0))
newdata$Manager_Experience <- as.numeric(round((newdata$Application_Receipt_Date - newdata$Manager_DOJ)/365, 0))
newdata$Age_Gap <- newdata$Manager_Age - newdata$Applicant_Age

newdata$Applicant_BirthDate <- NULL
newdata$Manager_DoB <- NULL
newdata$Manager_DOJ <- NULL
newdata$Application_Receipt_Date <- NULL

#Is the candidate from the Same City
#newdata$Samecity[newdata$Office_PIN == newdata$Applicant_City_PIN] <- 1
#newdata$Samecity[is.na(newdata$Samecity)] <- 0
#newdata$Samecity <- factor(newdata$Samecity, levels = c(1,0))

#Rate of hiring by the manager
newdata$Manager_Hiring_Rate <- newdata$Manager_Num_Coded/newdata$Manager_Num_Application
newdata$Manager_Hiring_Rate[is.nan(newdata$Manager_Hiring_Rate) == TRUE] <- 0
newdata$Manager_Hiring_Rate[newdata$Manager_Hiring_Rate == "Inf"] <- 0
newdata$Manager_Hiring_Rate <- round(newdata$Manager_Hiring_Rate, 2)

#Changes to Manager Data
newdata$Manager_Grade <- factor(newdata$Manager_Grade, levels = c("1","2",
                                                          "3","4",
                                                          "5","6",
                                                          "7","8",
                                                          "9","10"))
newdata$Manager_Prior_Experience[newdata$Manager_Joining_Designation != "Level 1"] <- 1
newdata$Manager_Prior_Experience[is.na(newdata$Manager_Prior_Experience) == TRUE] <- 0
newdata$Manager_Prior_Experience <- factor(newdata$Manager_Prior_Experience,
                                       levels = c(1,0))


#Amount of Business Sourced and Products Sold

newdata$SelfSourced <- newdata$Manager_Business - newdata$Manager_Business2
#newdata$SelfSourced[newdata$SelfSourced == 0] <- 0
#newdata$SelfSourced[newdata$SelfSourced < 0] <- -1
#newdata$SelfSourced[newdata$SelfSourced > 0] <- 1

#newdata$SelfSourced <- factor(newdata$SelfSourced, levels = c(1, 0, -1))
#levels(newdata$SelfSourced) <- c("Independent", "Dependence", "Extreme Dependence")

newdata$SelfSold <- newdata$Manager_Num_Products - newdata$Manager_Num_Products2
#newdata$SelfSold[newdata$SelfSold == 0] <- 0
#newdata$SelfSold[newdata$SelfSold < 0] <- -1
#newdata$SelfSold[newdata$SelfSold > 0] <- 1

#newdata$SelfSold <- factor(newdata$SelfSold, levels = c(1, 0, -1))
#levels(newdata$SelfSold) <- c("Independent", "Dependence", "Extreme Dependence")

### Applicant Analysis

newdata$Applicant_Occupation <- factor(newdata$Applicant_Occupation,
                                   levels = c("Salaried", "Business", 
                                              "Self Employed", "Student", 
                                              "Others"))


newdata$Applicant_Qualification <- as.character(newdata$Applicant_Qualification)
newdata$Applicant_Qualification[newdata$Applicant_Qualification == "Associate / Fellow of Institute of Chartered Accountans of India" |
                              newdata$Applicant_Qualification == "Associate/Fellow of Acturial Society of India" | 
                              newdata$Applicant_Qualification == "Associate/Fellow of Institute of Company Secretories of India" |
                              newdata$Applicant_Qualification == "Associate/Fellow of Institute of Institute of Costs and Works Accountants of India" |
                              newdata$Applicant_Qualification == "Associate/Fellow of Insurance Institute of India" |
                              newdata$Applicant_Qualification == "Certified Associateship of Indian Institute of Bankers"] <- "Graduate"
newdata$Applicant_Qualification <- factor(newdata$Applicant_Qualification,
                                          levels = c("Masters of Business Administration",
                                                     "Professional Qualification in Marketing",
                                                     "Graduate",
                                                     "Class XII", "Class X","Others"))


### Gender Dynamics
newdata$Gender_Dynamics <- paste0(as.character(newdata$Manager_Gender), 
                                  as.character(newdata$Applicant_Gender))
newdata$Gender_Dynamics <- as.factor(newdata$Gender_Dynamics)




#Checking correlation and listing out correlated variables                                                     

newdata2 <- data.frame(lapply(newdata, as.integer))
cor_v<-abs(cor(newdata2))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.90, arr.ind = T))

##Variables having high correlation
newdata$Manager_Current_Designation <- NULL # Has high correlationwith Manager_Grade
newdata$Manager_Business2 <- NULL #Has very high correlation with Manager_Business
newdata$Manager_Num_Products2 <- NULL #Correlated to Manager_Num_Products
newdata$Manager_Gender <- NULL ## Highly correlated with Gender_Dynamics
newdata$Office_PIN <- NULL ## Highly corelated with Applicant_City_Pin
newdata$Manager_Joining_Designation <- NULL
newdata$Applicant_Marital_Status <- NULL


##Strong Predictors:  
  # Applicant_City_PIN
  # Manager_Business
  # Age_Gap ##
  # Manager_Num_Products
  # Applicant_Age ##
  # Manager_Age ##
  # Manager_Num_Application
  # Manager_Hiring_Rate ##
  # Manager_Experience ##
  # Applicant_Occupation
  # SelfSourced ##
  # Manager_Num_Coded
  # SelfSold ##
  # Applicant_Qualification
  # Manager_Grade

 ## Weak Predictors
  # Applicant_Gender
  # Applicant_Marital_Status
  # Manager_Joining_Designation
  # Manager_Status
  # Manager_Prior_Experience ##
  # Gender_Dynamics ##












