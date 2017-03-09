# Fintro_Recruitment
Classification model for Fintro to hire the right financial agents.

The problem is from https://datahack.analyticsvidhya.com/contest/the-smart-recruits/ competition.

Fintro is looking for help from data scientists to help them provide insigths using their past recruitment data. They want to predict the target variable for each potential agent, which would help them identify the right agents to hire.

data for period Apr'2007 to Jan'2009;       https://datahack.analyticsvidhya.com/contest/the-smart-recruits/media/train_file/Train_pjb2QcD.csv
The training data for period Apr'2007 to 01-Jul-2008
      

Evaluation Metric is ROC - AUC.

Methodology:
Following considerable cleaning of data, it was found that there were several missing values, that needed to be imputed. Imputation was done using the MICE package, using 'Predictive mean matching' for numric data and 'Bayesian polytomous regression model' for categorical data.

Once the data was imputed and was ready for modeling, I looked into the option of Principal component analysis in order to get better boost. It did not improve the model.

I continued with gradient boosting using the xgboost package. 
Variable importance was considered and learning rate was controlled to optimize the model.

Final model was implemented and validated to get an ROC-AUC of 0.5995, ranking me at 42nd in the leaderboard.













      
      
