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


all$Applicant_Age <- as.numeric(round((all$Application_Receipt_Date - all$Applicant_BirthDate)/365, 0))
all$Manager_Age <- as.numeric(round((all$Application_Receipt_Date - all$Manager_DoB)/365, 0))
all$Manager_Experience <- as.numeric(round((all$Application_Receipt_Date - all$Manager_DOJ)/365, 0))
all$Age_Gap <- all$Manager_Age - all$Applicant_Age

all$Applicant_BirthDate <- NULL
all$Manager_DoB <- NULL
all$Manager_DOJ <- NULL
all$Application_Receipt_Date <- NULL

#Is the candidate from the Same City
#all$Samecity[all$Office_PIN == all$Applicant_City_PIN] <- 1
#all$Samecity[is.na(all$Samecity)] <- 0
#all$Samecity <- factor(all$Samecity, levels = c(1,0))

#Rate of hiring by the manager
all$Manager_Hiring_Rate <- all$Manager_Num_Coded/all$Manager_Num_Application
all$Manager_Hiring_Rate[is.nan(all$Manager_Hiring_Rate) == TRUE] <- 0
all$Manager_Hiring_Rate[all$Manager_Hiring_Rate == "Inf"] <- 0
all$Manager_Hiring_Rate <- round(all$Manager_Hiring_Rate, 2)

#Changes to Manager Data
all$Manager_Grade <- factor(all$Manager_Grade, levels = c("1","2",
                                                                  "3","4",
                                                                  "5","6",
                                                                  "7","8",
                                                                  "9","10"))
all$Manager_Prior_Experience[all$Manager_Joining_Designation != "Level 1"] <- 1
all$Manager_Prior_Experience[is.na(all$Manager_Prior_Experience) == TRUE] <- 0
all$Manager_Prior_Experience <- factor(all$Manager_Prior_Experience,
                                           levels = c(1,0))


#Amount of Business Sourced and Products Sold

all$SelfSourced <- all$Manager_Business - all$Manager_Business2
#all$SelfSourced[all$SelfSourced == 0] <- 0
#all$SelfSourced[all$SelfSourced < 0] <- -1
#all$SelfSourced[all$SelfSourced > 0] <- 1

#all$SelfSourced <- factor(all$SelfSourced, levels = c(1, 0, -1))
#levels(all$SelfSourced) <- c("Independent", "Dependence", "Extreme Dependence")

all$SelfSold <- all$Manager_Num_Products - all$Manager_Num_Products2
#all$SelfSold[all$SelfSold == 0] <- 0
#all$SelfSold[all$SelfSold < 0] <- -1
#all$SelfSold[all$SelfSold > 0] <- 1

#all$SelfSold <- factor(all$SelfSold, levels = c(1, 0, -1))
#levels(all$SelfSold) <- c("Independent", "Dependence", "Extreme Dependence")

### Applicant Analysis

all$Applicant_Occupation <- factor(all$Applicant_Occupation,
                                       levels = c("Salaried", "Business", 
                                                  "Self Employed", "Student", 
                                                  "Others"))


all$Applicant_Qualification <- as.character(all$Applicant_Qualification)
all$Applicant_Qualification[all$Applicant_Qualification == "Associate / Fellow of Institute of Chartered Accountans of India" |
                                  all$Applicant_Qualification == "Associate/Fellow of Acturial Society of India" | 
                                  all$Applicant_Qualification == "Associate/Fellow of Institute of Company Secretories of India" |
                                  all$Applicant_Qualification == "Associate/Fellow of Institute of Institute of Costs and Works Accountants of India" |
                                  all$Applicant_Qualification == "Associate/Fellow of Insurance Institute of India" |
                                  all$Applicant_Qualification == "Certified Associateship of Indian Institute of Bankers"] <- "Graduate"

#Imputation of missing data                                                                                                          
impdata1 <- mice(subset(all, select = c(1,2,12:25)), m = 5, maxit = 5, method = 'pmm')
impdata1 <- complete(impdata1, 5)
impdata2 <- mice(subset(all, select = c(3:11)), m = 5, maxit = 5, method = 'polyreg')
impdata2 <- complete(impdata2, 5)

newdata <- cbind(impdata1, impdata2)

### Gender Dynamics
newdata$Gender_Dynamics <- paste0(as.character(newdata$Manager_Gender), 
                              as.character(newdata$Applicant_Gender))
newdata$Gender_Dynamics <- as.factor(newdata$Gender_Dynamics)

all$Applicant_Qualification <- factor(all$Applicant_Qualification,
                                      levels = c("Graduate Banking",
                                                 "Masters of Business Administration",
                                                 "Professional Qualification in Marketing",
                                                 "Graduate",
                                                 "Class XII", "Class X","Others"))


#Checking correlation and listing out correlated variables                                                     

newdata2 <- data.frame(lapply(newdata, as.integer))
cor_v<-abs(cor(newdata2))
diag(cor_v)<-0
cor_v[upper.tri(cor_v)] <- 0
cor_f <- as.data.frame(which(cor_v > 0.85, arr.ind = T))

##Variables having high correlation
newdata$Manager_Current_Designation <- NULL # Has high correlationwith Manager_Grade
newdata$Manager_Business2 <- NULL #Has very high correlation with Manager_Business
newdata$Manager_Num_Products2 <- NULL #Correlated to Manager_Num_Products
newdata$Manager_Gender <- NULL ## Highly correlated with Gender_Dynamics
newdata$Applicant_City_PIN <- NULL ## Highly corelated with Applicant_City_Pin

















