# Acquisition_Risk_Analytics

## Problem Statement

CredX, a credit card provider is facing a credit loss and wants to mitigate risk by acquiring the right customers. The objective is to identify the right customers using predictive models and techniques related to Acquisition Risk Analytics. We need to determine the factors affecting credit risk, create strategies to mitigate the acquisition risk and assess the financial benefit of the project.

## Business Understanding

The Credit card company wants to reduce the risk involved with its applicants for credit card.  
 Credit loss is of 2 types:  
 • Risky applicants given credit cards resulting in default in payments  
 • Non-risky applicants not given credit cards resulting in loss of revenue  
 The company wants to acquire the right customers based on this. This is a Classification problem.  
  
## Data Available
  
 We have 2 structured datasets:  
 • Demographic Data: It has 71295 observations and 12 variables. The variables are related to an applicant's demographic details like Gender, Marital Status, Income, etc. The target variable is ‘Performance Tag’. If its value is 1 then an applicant defaults on credit card, else he does not.  
 • Credit Bureau Data: It has 71295 observations and 19 variables. It consists of variables informing if the customers have defaulted in previous history of credit cards, about their trades, etc. The target variable is Performance Tag.  
 Issues resolved in Data Cleaning Stage:  
 • Presence of 3 duplicates in identifier variable  
 • Presence of NA values – removed all  
 • Presence of invalid values e.g. negative age  
 • Outliers  

Outliers were found in before 1st percentile of income, after 99th percentile of number of months in current company, after 99th percentile of number of trades, after 99th percentile of Outstanding balance and after 99th percentile of number of trades  

## Problem Solving Approach

After data cleaning and merging the two datasets, we performed the following steps:  
 ➢ Binning categorical values in Demographic data  
 ➢ Extensive Exploratory Data Analysis to detect important predictors and insights  
 ➢ Association rule mining for important predictors  
 ➢ Weight of Evidence and Information Value Analysis  
 ➢ Building an evaluation of Logistic Regression Model: It is a classification problem and this model gives us linear relationship  
 ➢ Building a Random Forest Model: We will feed the important predictors obtain through logistic regression to a Random Forest model to check for better performance  
 ➢ Creating an Application Scorecard  
 ➢ Assessing Financial benefit of the project using Gain-Lift chart  

 ## Exploratory Data Analysis Relationship between variables

• The instances of 90/60/30 days past due in the last 6 to 12 months are correlated with each other.  
• Outbalance is correlated with The Presence of home loan.  
• Number of trades inquired are correlated with the number of inquiries made.   
• Average Credit Utilization rises with an increase in theinstances if 90 days past due instances in the last six months.  

## EDA - Univariate Analysis

• Most credit card applicants have a Professional education, followed by Masters and Bachelors  
• Most applicants are in their current residence since the last 6 months  
• Most applicants belong to middle age group  

## EDA - Bivariate Analysis

• Income Group has a bearing on the number of trades- low income groups make the most number of trades.  
• Low income groups also have the highest credit utilization.  

## Predictors of Performance Tag

• Most applicants have a Professional Education, followed by Masters Education and Bachelors Education. Defaulters vary as per proportion.  
• Most people live in rented houses and hence majority of the customer behavior can be traced here  
• No clear linear trend is seen in the number of dependents and defaulting on a loan.  
• Those at the same place of residence for 6months- 1year are most likely to default.   
• Those at their company for less than an year are also more likely to default.   
• Low income is also a good predictor of default.  
• Young adults are the safest best to extend credit cards to.   
• As the number of DPD incidents increase, the chances of defaulting also increase.  
• The trends with days past due continues when observed over a 12 month window.  
• Those with average credit utilization between 30-90 are far more likely to default with others.  
 • Increasing number of PL trades increases the chances of defaulting.  
 • No clear linear trend of performance tag is seen against Inquiries made in the last six or twelve months, total number of trades or the outstanding balance.  
 
## Association Rule Mining

 • More than 3 years in current residence, Zero PL trades opened in last 12 months and Zero Inquiries in last 6 months excluding home & auto loans paired with Performance Tag 0 shows highest lift  
 • More than 3 years in current residence, Zero PL trades opened in last 6 months, Zero PL trades opened in last 12 months and Zero Inquiries in last 6 months excluding home & auto loans paired with Performance Tag 0 shows second highest lift  
 • More than 3 years in current residence, zero times 90 DPD or worse in last 12 months and Zero PL trades opened in last 12 months paired with Performance Tag 0 shows third highest lift  
  
 Note: Performance Tag 0 stands for non-defaulters  

 ## Weight of Evidence and Information Value

 After performing an analysis on WOE and IV, we found that No. of trades opened, average credit card utilization and number of inquiries of the last 12 months are the top 3 predictors.  

## Model Building

After a sample ML model with demographic data, we built ML models with 2 versions of data:  
 1. Data with values replaced by corresponding Weight of Evidence value (WOE data)  
 2. Data with regular values (Regular data) – For Comparison  

For WOE data:  
We developed a Logistic Regression model with an AUC score of 0.675.  
We developed a Random Forest model with an AUC score of 0.679  

For regular data:  
We developed a Logistic Regression model with an AUC score of 0.671  
We developed a Random Forest model, with an AUC score of 0.612  

## Final Model:
➢ We selected the Random Forest Model with WOE values as the Final Model.  
➢ It has an AUC score of 0.679 and Accuracy of 63.4%  
➢ The predictors for Credit card default present in our final model were:  
 • No. of times 30 DPD in the last 12 months  
 • Average Credit Card utilization  
 • No. of P/L Trades in the last 12 months  
 • No. of Inquiries in the last 12 months  

## Application Scores - calculation and cut off

 Application Scores Calculation:  
 We calculated Application Scores based on the formula:  
 Score = (Offset - Factor)*(predicted probabilities)  
 where,  
 Offset = (Target Score Value Factor)*log(Inverted Target Odds)  
 Factor = Points to double odds / log(2)  
   
 We considered the intersection between possible True Positives (Sensitivity), True Negatives (Specificity) and Accuracy of our final model as the cut-off %. Using this percentage (0.05%), we calculated the cut-off score as:  
0.05% of Total of unique scores, i.e. 70 (approx.)  
Looking at the scores across Good (not defaulting) and Bad (defaulting) Applicants, we concluded that:   
➢ Credit card applicants with an application score below 70 can be denied credit card  

## Project Outcome

### Financial Benefit of the project:
To assess the financial benefit of this project we prepared a Gain-Lift table, which shows that:  
 ➢ After implementation of this project, the firm can detect 75% of Bad Applicants by targeting 50% of the Applicants.  
 ➢ Roughly, in money terms, the firm will be saving an average potential credit loss of 8.23 million USD  
 
