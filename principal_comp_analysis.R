####Principal Component Analysis

newdata2$Manager_Current_Designation <- NULL # Has high correlationwith Manager_Grade
newdata2$Manager_Business2 <- NULL #Has very high correlation with Manager_Business
newdata2$Manager_Num_Products2 <- NULL #Correlated to Manager_Num_Products
newdata2$Manager_Gender <- NULL ## Highly correlated with Gender_Dynamics
newdata2$Applicant_City_PIN <- NULL ## Highly corelated with Applicant_City_Pin

scaledata <- scale(newdata2)
pca_data <- prcomp(scaledata)
std_dev <- pca_data$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

p_data <- data.frame(pca_data$x)

train_new <- p_data[1:nrow(train), ]
test_new <- p_data[-(1:nrow(train)), ]







