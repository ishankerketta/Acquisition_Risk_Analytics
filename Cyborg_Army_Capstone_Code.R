# BFS Capstone - Risk Analytics #
#---------------------------------#
  
# Group Members:

# Ishan Savio Kerketta (Group Facilitator)
# Devanshi Kulshrestha
# Ayushi Gaur
# Ajay Sharma
  
# --------------------------- #
  
## Problem Statement

# CredX, a credit card provider is facing a credit loss 
# and wants to mitigate risk by acquiring the right customers. 
# The objective is to identify the right customers 
# using predictive models and techniques related to Acquisition Risk Analytics. 
# We need to determine the factors affecting credit risk, 
# create strategies to mitigate the acquisition risk and assess the financial benefit of the project.

## Business Understanding

# The Credit card company wants to reduce the risk involved with its applicants for credit card.
# Credit loss is of 2 types:
# Risky applicants given credit cards resulting in default in payments
#	Non-risky applicants not given credit cards resulting in loss of revenue
# The company wants to acquire the right customers based on this.

## ----------------------------------------------------------------------------------- ##

#setwd("")

# Installing and loading required packages

pkgs <- c("tidyverse", "ggplot2", "lattice", "caret", "corrplot",
          "cowplot", "caTools", "MASS", "ROCR","pROC" ,"lift", "car", "woeBinning", "woe",
          "DMwR", "caTools", "randomForest", "gridExtra", "Information", 
          "arulesViz", "trimcluster", "prabclus", "scorecard")

install.lib <- pkgs[!pkgs %in% installed.packages()]

sapply(install.lib, function(x) install.packages(x, dependencies = TRUE))

sapply(pkgs, require, character=TRUE)

# Installing 'ModelMetrics' package separately
# as we will not load it, but use it in line with code
# install.packages("ModelMetrics")

# Loading the data

demographic <- read.csv("Demographic data.csv", na.strings = c("","NA"))
credit_bureau <- read.csv("Credit Bureau data.csv", na.strings = c("","NA"))

# Looking at the demographic data
str(demographic)
summary(demographic)

# Renaming the variables for simplicity
names(demographic) <- c("appid", "age", "gender", "mar.st", "n.dep", "income", 
                        "edu", "prof", "type.resi","n.resi", "n.comp","tag")

str(demographic)

# It has 71295 observations and 12 variables. 
# The variables are related to an applicant's demographic details like 
# Gender, Marital Status, Income, etc. 
# The target variable is Performance Tag. 
# If its value is 1 then an applicant defaults on credit card, else he does not

# looking at the credit bureau data
str(credit_bureau)
summary(credit_bureau)

# Renaming the variables for simplicity
names(credit_bureau) <- c("appid", "n.90dpd.6mn", "n.60dpd.6mn", "n.30dpd.6mn", "n.90dpd.12mn", 
                      "n.60dpd.12mn", "n.30dpd.12mn", "avg.cc", "n.trades.6mn", "n.trades.12mn", 
                      "n.pltrades.6mn", "n.pltrades.12mn", "n.inq.6mn", "n.inq.12mn", "homeln", 
                      "outbal", "n.trades", "autoln", "tag")
str(credit_bureau)

# It has 71295 observations and 19 variables. 
# It consists of variables informing if the customers 
# have defaulted in previous history of credit cards, about their trades, etc. 
# The target variable is Performance Tag

## ------- DATA CLEANING ------ ##

# Checking for duplicates in Application ID
sum(duplicated(demographic$appid))
#there are 3 duplicates

#looking at the duplicates
View(demographic[which(duplicated(demographic$appid)),])

# is appid the unique identifier of both datasets?
temp_demo <-  demographic %>% 
  group_by(appid) %>% 
  filter(n() > 1)

temp_cred <- credit_bureau %>% 
  group_by(appid) %>% 
  filter(n() > 1)

sum(unique(temp_demo$appid) %in% unique(temp_cred$appid))
# The same app id is repeated for both these datasets. 

# Getting rid of six rows corresponding to these pairs of duplicates
demographic <- demographic[!(demographic$appid %in% unique(temp_demo$appid)),]
credit_bureau <- credit_bureau[!(credit_bureau$appid %in% unique(temp_cred$appid)),]

# Merging demographic data and credit bureau data using an inner join
main <- merge(demographic, credit_bureau, by = c("appid", "tag"))

# There are no missing values 
# as we have converted them into NA while extracting data

# Checking for NA values
sapply(main, function(x) length(which(is.na(x))))

# tag has 1425 NA values
# Since this is the target variable let's remove these rows

main <- main[-which(is.na(main$tag)),]

## Looking at NA values again
sapply(main, function(x) length(which(is.na(x))))

# Looking at the percentage of NAs in the dataset
round((sum(apply(main, 1, function(x) any(is.na(x))))/(nrow(main))*100),2)
# 1.68% of the data has NAs

# Let's keep these NAs for WOE imputation at a later stage

# Let's look at the distribution of tag
prop.table(table(main$tag)) %>% round(2)
# data is unbalanced

# Now that all rows with NAs have been taken care of
# Let's look at erronous values

# Looking the anomalies in age
summary(factor(main$age))

# There are invalid values of age like -3, 0
# and unnatural values of age like 15, 16, 17 for a credit account

View(main[which(main$age==c(15,16,17)),])

# We can also see that some of them are Married
# So definitely these are erronous ages

# Let's replace these invalid and unnatural age values with NA

main$age[which(main$age==-3)] <- NA
main$age[which(main$age==0)] <- NA
main$age[which(main$age==15)] <- NA
main$age[which(main$age==16)] <- NA
main$age[which(main$age==17)] <- NA

# Looking at the frequency of the NAs
length(which(is.na(main$age)))
# There are very few NAs

# Deleting all NAs
main <- data.frame(na.omit(main))

# Looking at the percentiles of quantitative variables

#----- Income -----#

quantile(main$income, seq(0,1,0.01))
# There is negative income and outliers in the initial values

#Let's floor the initial values of income at 1% i.e. 4.50
main$income[which(main$income<4.50)] <- 4.50

#Let's look at the percentiles again
quantile(main$income, seq(0,1,0.01))

# Looking at a box plot of income
boxplot(main$income)

# We can go forward with this distribution

#----- No. of months in current residence -----#
quantile(main$n.resi, seq(0,1,0.01))

# no outliers

#----- No.of months.in.current.company -----#
quantile(main$n.comp, seq(0,1,0.01))

# There is a suddent jump from 99% to 100%
# Let's cap the values at 99%

main$n.comp[which(main$n.comp>74.0)] <- 74.0

# Looking at box plot
boxplot(main$n.comp)

### ------------------------------------------------------------- ###

## Checking for Outliers in Credit Bureau variables

quantile(main$avg.cc, seq(0,1,0.01))
quantile(main$n.trades.6mn, seq(0,1,0.01))

quantile(main$n.trades.12mn, seq(0,1,0.01))
# there is a spike after the 99th percentile
# let's cap at 99%
main$n.trades.12mn[which(main$n.trades.12mn>21)] <- 21

quantile(main$n.pltrades.6mn, seq(0,1,0.01))
quantile(main$n.pltrades.12mn, seq(0,1,0.01))

quantile(main$outbal, seq(0,1,0.01))
# there is sudden peak after 99th percentile
# let's cap at 99%
main$outbal[which(main$outbal>4250657.64)] <- 4250657.64

quantile(main$n.trades, seq(0,1,0.01))
# there is sudden peak after 99th percentile
# let's cap at 99%
main$n.trades[which(main$n.trades>31)] <- 31

# Looking at the data structure again
str(main)
summary(main)

## ------------ END of DATA CLEANING ------------------- ##

## ----------------- DATA PREPARATION ------------------ ##

## Creating bins for required variables

# Income
summary(main$income)

# Creating a variable income_group binning values of income
main$income_group<-ifelse(main$income<14,"low_income",
                          ifelse(main$income>=14 & main$income<27,"lower_middle_income",
                                 ifelse(main$income>=27 & main$income<40,"higher_middle_income", "high_income")))


main$income_group<-factor(main$income_group)

summary(main$income_group)

# Deleting income variable as it will be correlated
main$income <- NULL

# n.resi
summary(main$n.resi)

main$n.resi<-ifelse(main$n.resi<7,"6_months",
                                      ifelse(main$n.resi>=7 & 
                                               main$n.resi<12,"less_than_1_yr",
                                             ifelse(main$n.resi>=12 & 
                                                      main$n.resi<37,"between_1_&_3_yrs", "more_than_3_yrs")))


main$n.resi<-factor(main$n.resi)

summary(main$n.resi)

# No.of.months.in.current.company
summary(main$n.comp)

main$n.comp<-ifelse(main$n.comp<12,"Less_than_a_yr",
                                    ifelse(main$n.comp>=12 & 
                                             main$n.comp<37,"1_to_3_yrs", "more_than_3_yrs"))


main$n.comp<-factor(main$n.comp)

summary(main$n.comp)

# age
summary(main$age)

main$age_group<-ifelse(main$age<25,"young_adult",
                       ifelse(main$age>=25 & 
                                main$age<40,"adult",
                              ifelse(main$age>=40 & 
                                       main$age<60,"middle_age", "senior_citizen")))


main$age_group<-factor(main$age_group)

summary(main$age_group)

# Deleting  age variable as it will be correlated
main$age <- NULL

#Looking at data structure
str(main)

# Converting No of dependents to factor
main$n.dep <- factor(main$n.dep)

## ----------------------------- END of DATA PREPARATION -------------------------------------- ##

## Exploratory Data Analysis ##

# Creating a Subset of data_final for data_exploration with NA Values removed
main_eda <- main[complete.cases(main), ]


# Looking at the distribution of categorical variables
ggplot(main_eda, aes(x=gender, fill = gender)) + geom_bar()
# Most customers are Male

ggplot(main_eda, aes(x=mar.st, fill = mar.st)) + geom_bar()
# most customers are Married 

ggplot(main_eda, aes(x=edu, fill = edu)) + geom_bar()
# most customers have professional education, 
# followed by customers with Master's Degree then Bachelor's Degree

ggplot(main_eda, aes(x=prof, fill = prof)) + geom_bar()
# Most customers have salaried jobs

ggplot(main_eda, aes(x=type.resi, fill = type.resi)) + geom_bar()
# Most customers live in rented houses

ggplot(main_eda, aes(x=income_group, fill = income_group)) + geom_bar()
# Customers of different income groups are almost equally distributed.
# However, low income group is slightly less

ggplot(main_eda, aes(x=n.resi, fill = n.resi)) + geom_bar()
# Most customers have been residing in the current address for 6 months
# followed by customers with residence for more than 3 years

ggplot(main_eda, aes(x=n.comp, fill = n.comp)) + geom_bar()
# Most customers have been in the current company for more than 3 years

ggplot(main_eda, aes(x=age_group, fill = age_group)) + geom_bar()
# Most customers belong to middle age group 

# Also, looking at n.dep as a categorical variable
ggplot(main_eda, aes(x=factor(n.dep), fill = factor(n.dep))) + geom_bar()
# Most customers have dependents between 1 and 3.

# Looking at cross tables of categorical variables and Performance Tag
# Also looking at the percentage distribution between Performance Tags across categorical variables

# gender
table(main_eda$gender, main_eda$tag)
ggplot(main_eda, aes(x=factor(gender), 
                     fill = factor(tag))) + geom_bar()

#prop.table(table(main_eda$gender, main_eda$tag),round(digits = 1)) %>%
#{. * 100} %>% round(2)
prop.table(round(table(main_eda$gender, main_eda$tag),digits = 1)) %>% {. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(gender), 
                     fill = factor(tag))) + geom_bar(position = "fill")

# Marital Status
table(main_eda$mar.st, main_eda$tag)
ggplot(main_eda, aes(x=factor(mar.st), 
                     fill = factor(tag))) + geom_bar()

#prop.table(table(main_eda$mar.st, main_eda$tag),round(digits = 1)) %>%
#{. * 100} %>% round(2)
prop.table(round(table(main_eda$mar.st, main_eda$tag),digits = 1)) %>%
  {. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(mar.st), 
                     fill = factor(tag))) + geom_bar(position = "fill")

# Education
table(main_eda$edu, main_eda$tag)
ggplot(main_eda, aes(x=factor(edu), 
                     fill = factor(tag))) + geom_bar()
#prop.table(table(main_eda$edu, main_eda$tag),round(digits = 1)) %>%
#{. * 100} %>% round(2)
prop.table(round(table(main_eda$edu, main_eda$tag),digits = 1)) %>%
  {. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(edu), 
                     fill = factor(tag))) + geom_bar(position = "fill")

# Profession
table(main_eda$prof, main_eda$tag)
ggplot(main_eda, aes(x=factor(prof), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$prof, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(prof), 
                     fill = factor(tag))) + geom_bar(position = "fill")
# Type of residence
table(main_eda$type.resi, main_eda$tag)
ggplot(main_eda, aes(x=factor(type.resi), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$type.resi, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(type.resi), 
                     fill = factor(tag))) + geom_bar(position = "fill")
# Income Group
table(main_eda$income_group, main_eda$tag)
ggplot(main_eda, aes(x=factor(income_group), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$income_group, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(income_group), 
                     fill = factor(tag))) + geom_bar(position = "fill")
# Months in Current Residence
table(main_eda$n.resi, main_eda$tag)
ggplot(main_eda, aes(x=factor(n.resi), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$n.resi, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(n.resi), 
                     fill = factor(tag))) + geom_bar(position = "fill")
# Months in Current Company
table(main_eda$n.comp, main_eda$tag)
ggplot(main_eda, aes(x=factor(n.comp), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$n.comp, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(n.comp), 
                     fill = factor(tag))) + geom_bar(position = "fill")
# Age Group
table(main_eda$age_group, main_eda$tag)
ggplot(main_eda, aes(x=factor(age_group), 
                     fill = factor(tag))) + geom_bar()
prop.table(round(table(main_eda$age_group, main_eda$tag),digits = 1)) %>%
{. * 100} %>% round(2)
ggplot(main_eda, aes(x=factor(age_group), 
                     fill = factor(tag))) + geom_bar(position = "fill")


# EDA: CREDIT BUREAU VARIABLES

# Univariate Analysis

s <- scale_y_continuous(labels = scales::percent)
t <- theme(axis.text.x = element_text(angle = 90, size = 6), axis.text.y = element_text(size = 6))
v <- "Percentage"

ggplot(main_eda) + geom_density(aes(x= outbal), fill = "grey67")+ labs(y = v, x=  "Outstanding Balance")
# Frequency of Outstanding Balance is highest at low amounts and keeps decreasing
# Again, there is a peak at around 3,000,000 and decreases after that

ggplot(main_eda) + geom_density(aes(x= n.trades), fill = "grey67")+
  labs(y = v, x= "Total Trades")
# Frequency of total number of trades is highest betweeen 0 and 10

grid.arrange(ggplot(main_eda) + geom_bar(aes(x= n.90dpd.6mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "90 DPD Instances in last 6 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.60dpd.6mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "60 DPD Instances in last 6 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.30dpd.6mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "30 DPD Instances in last 6 Months"))

grid.arrange(ggplot(main_eda) + geom_bar(aes(x= n.90dpd.12mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "90 DPD Instances in last 12 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.60dpd.12mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "60 DPD Instances in last 12 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.30dpd.12mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "30 DPD Instances in last 12 Months"))

grid.arrange(ggplot(main_eda) + geom_bar(aes(x= n.trades.6mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "Trades Opened in last 6 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.trades.12mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "Trades Opened in Last 12 Months"),
             ggplot(main_eda) + geom_bar(aes(x= n.pltrades.6mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "PL Trades Opened in Last 6 Months" ),
             ggplot(main_eda) + geom_bar(aes(x= n.pltrades.12mn, y = (..count..)/sum(..count..)))+
               labs(y = v, x= "PL Trades Opened in Last 12 Months"))

grid.arrange(ggplot(main_eda) + geom_bar(aes(x= factor(n.inq.6mn), y = (..count..)/sum(..count..)))+
               labs(y = v, x= "Inquiries in last 6 Months"),
             ggplot(main_eda) + geom_bar(aes(x= factor(n.inq.12mn), y = (..count..)/sum(..count..)))+
               labs(y = v, x= "Inquiries in last 12 Months"),
             ggplot(main_eda) + geom_density(aes(x= outbal), fill = "grey67")+
               labs(y = v, x=  "Outstanding Balance"),
            ggplot(main_eda) + geom_density(aes(x= avg.cc), fill = "grey67")+
               labs(y = v, x= "Average CC"))
             
# all of these graphs show that most of these variables have a value 0

# Bar charts
ggplot(main_eda, aes(x = factor(homeln), fill = factor(homeln))) +geom_bar()
ggplot(main_eda, aes(x = factor(autoln), fill = factor(autoln))) +geom_bar()

# EDA: OVERALL

#Income Group
ggplot(main_eda, aes(x=income_group)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=income_group)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=income_group)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()
# Applicants of high-income group have lesser median outstanding balance, but the balance is widespread
# Applicants of low-income group have the highest median number of trades
# Applicants of low-income use credit cards the most and those of high-income use credit cards the least

#Education
ggplot(main_eda, aes(x=edu)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=edu)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=edu)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()
#no trend

#Profession
ggplot(main_eda, aes(x=prof)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=prof)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=prof)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()
# no significant difference

#Type.of.residence
ggplot(main_eda, aes(x=type.resi)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=type.resi)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=type.resi)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()

# No. of months in current company
ggplot(main_eda, aes(x=n.comp)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=n.comp)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=n.comp)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()

# No. of months in current residence
ggplot(main_eda, aes(x=n.resi)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=n.resi)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=n.resi)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()

#age_group
ggplot(main_eda, aes(x=age_group)) + geom_boxplot(aes(y= outbal)) + coord_flip() 
ggplot(main_eda, aes(x=age_group)) + geom_boxplot(aes(y= n.trades)) + coord_flip()
ggplot(main_eda, aes(x=age_group)) + geom_boxplot(aes(y= avg.cc)) + coord_flip()


#Creating a correlation plot on the continous variables
corr.matrix <- cor(subset(main_eda, select = names(main_eda)[sapply(main_eda, is.numeric)]))
corrplot(corr.matrix, title = 'Correlation plot',
         method = "color", tl.cex = 0.7, type = "lower")

# MultiVariate Analysis

ggplot(main_eda) +
  geom_line(aes(x = avg.cc, y = n.pltrades.12mn, group=factor(tag), color=factor(tag)), 
            stat='summary', fun.y='mean') 

ggplot(main_eda) + 
  geom_line(aes(x=n.90dpd.6mn, y=avg.cc, group=factor(tag), color=factor(tag)), 
            stat='summary', fun.y='mean')  

ggplot(main_eda) + 
  geom_line(aes(x=n.trades, y= outbal, group=factor(tag), color=factor(tag)),
            stat='summary', fun.y='mean') 

ggplot(main_eda)+ 
  geom_line(aes(x= n.inq.12mn, y=n.trades , group=factor(tag), color=factor(tag)),
            stat='summary', fun.y='mean')

# Analysing Predictors of Performance Tag 

# Converting categorical variables to factor
main_eda$homeln <- as.factor(main_eda$homeln)
main_eda$autoln <- as.factor(main_eda$autoln)

# Binning credit bureau data variables

main_eda$avg.cc.grp <- cut(main_eda$avg.cc, 
                           seq(0, 120, 30), right = FALSE)

main_eda$n.trades.6mn.grp <- cut(main_eda$n.trades.6mn, 
                                 seq(0, 16, 4), right = FALSE)

main_eda$n.trades.12mn.grp <- cut(main_eda$n.trades.12mn,
                                  seq(0, 24, 4), right = FALSE)

main_eda$n.60dpd.6mn.grp <- cut(main_eda$n.60dpd.6mn, 
                                seq(0, 6, 2), right = FALSE)
main_eda$n.30dpd.6mn.grp <- cut(main_eda$n.30dpd.6mn, 
                                seq(0, 8, 2), right = FALSE)
main_eda$n.90dpd.12mn.grp <- cut(main_eda$n.90dpd.12mn, 
                                 seq(0, 6, 2), right = FALSE)
main_eda$n.60dpd.12mn.grp <- cut(main_eda$n.60dpd.12mn, 
                                 seq(0, 8, 2), right = FALSE)
main_eda$n.30dpd.12mn.grp <- cut(main_eda$n.30dpd.12mn, 
                                 seq(0, 10, 2), right = FALSE)             

main_eda$n.pltrades.6mn.grp <- cut(main_eda$n.pltrades.6mn, 
                                   seq(0, 8, 4), right = FALSE)

main_eda$n.pltrades.12mn.grp <- cut(main_eda$n.pltrades.12mn, 
                                    seq(0, 16, 4), right = FALSE)

main_eda$n.inq.6mn.grp <- cut(main_eda$n.inq.6mn, 
                              seq(0, 12, 4), right = FALSE)
main_eda$n.inq.6mn.grp1 <- ifelse(main_eda$n.inq.6mn == 0, "0",
                                 main_eda$n.inq.6mn.grp)

main_eda$n.inq.12mn.grp <- cut(main_eda$n.inq.12mn, 
                               seq(0, 24, 4), right = FALSE)

main_eda$n.trades.grp <- cut(main_eda$n.trades, 
                             seq(0, 35, 7), right = FALSE)

main_eda$outbal.grp <- cut(main_eda$outbal, seq(0, 5000000, 1000000),
                           right = FALSE)

# Creating a function for bar plots
cfn.plot<- function(x, y, w, z){
  aaa<- enquo(x)
  bbb<- enquo(y)
  main_eda %>% 
    group_by(!!aaa, !!bbb) %>% 
    summarise(n = n()) %>% 
    mutate(Percentage = n/sum(n) * 100) %>% 
    filter(!!bbb == 1) %>% 
    ggplot(aes(x= !!aaa, y = Percentage, fill = factor(!!bbb))) + 
    geom_bar(stat = "identity") +
    labs(y = z, x = w) + 
    theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6), legend.position = "none")
}

main_eda$homeln <- as.factor(main_eda$homeln)
main_eda$autoln <- as.factor(main_eda$autoln)

unique(main_eda$avg.cc.grp)
unique(main_eda$n.trades.6mn.grp)
unique(main_eda$n.trades.12mn.grp)
unique(main_eda$n.60dpd.6mn.grp) 
unique(main_eda$n.30dpd.6mn.grp)
unique(main_eda$n.90dpd.12mn.grp)
unique(main_eda$n.60dpd.12mn.grp)
unique(main_eda$n.30dpd.12mn.grp)
unique(main_eda$n.pltrades.6mn.grp)
unique(main_eda$n.pltrades.12mn.grp)
unique(main_eda$n.inq.6mn.grp)
unique(main_eda$n.inq.6mn.grp1)
unique(main_eda$n.inq.12mn.grp)
unique(main_eda$n.trades.grp)
unique(main_eda$outbal.grp)


# Plotting the predictors to show percentage that Tag = 1
# Here, 'Percentage' shows percentage of overall Tag

grid.arrange(cfn.plot(gender, tag, "Gender", "% of total"), 
             cfn.plot(mar.st, tag, "Marital Status" , "% of total"), 
             cfn.plot(edu, tag, "Education", "% of total"), 
             cfn.plot(prof, tag,"Profession","% of total"))

grid.arrange(cfn.plot(type.resi, tag, "Residence" , "% of total"),
             cfn.plot(homeln, tag, "Home Loan", "% of total"), 
             cfn.plot(autoln, tag, "Auto Loan","% of total"), 
             cfn.plot(n.dep, tag, "Number of Dependents", "% of total"))

grid.arrange(cfn.plot(n.resi, tag, "Years in Residence" , "% of total"),
             cfn.plot(n.comp, tag, "Years in Company" , "% of total"),   
             cfn.plot(income_group, tag, "Income" , "% of total"), 
             cfn.plot(age_group, tag, "Age Group" , "% of total"))

grid.arrange(cfn.plot(n.90dpd.6mn, tag, "90 DPD Instances in last 6 Months", "% of total"), 
             cfn.plot(n.60dpd.6mn.grp, tag, "60 DPD Instances in last 6 Months", "% of total"), 
             cfn.plot(n.30dpd.6mn.grp, tag, "30 DPD Instances in last 6 Months", "% of total"), 
             cfn.plot(n.90dpd.12mn.grp, tag, "90 DPD Instances in last 12 Months", "% of total"))

grid.arrange(cfn.plot(n.60dpd.12mn.grp, tag, "60 DPD Instances in last 12 Months", "% of total"),
             cfn.plot(n.30dpd.12mn.grp, tag, "30 DPD Instances in last 12 Months", "% of total"),
             cfn.plot(avg.cc.grp , tag, "Average CC", "% of total"),
             cfn.plot(n.trades.6mn.grp , tag, "Trades Opened in last 6 Months", "% of total"))

grid.arrange(cfn.plot(n.trades.12mn.grp , tag, "Trades Opened in Last 12 Months", "% of total"),
             cfn.plot(n.pltrades.6mn.grp , tag, "PL Trades Opened in Last 6 Months", "% of total"),
             cfn.plot(n.pltrades.12mn.grp, tag, "PL Trades Opened in Last 12 Months", "% of total"),
             cfn.plot(n.inq.6mn.grp , tag, "Inquiries in last 6 Months", "% of total"))

grid.arrange(cfn.plot(n.inq.12mn.grp, tag, "Inquiries in last 12 Months", "% of total"),
             cfn.plot(n.trades.grp, tag, "Total trades", "% of total"), 
             cfn.plot(outbal.grp, tag, "Outstanding Balance", "% of total"))

## ----------------------------------------------------------------------------- ##

#-------- Association Rule Mining - Lift Analysis ------------ #

# Removing appid
main_2 <- main[,-1]

#converting all variables into factor
main_2 <- data.frame(sapply(main, function(x) as.factor(x)))
main_2 <- as(main_2, "transactions")

itemFrequencyPlot(main_2,topN=10,type="absolute")

rules_main <- apriori(main_2, parameter = list(support = 0.1, confidence = 0.7))

top10_main <- head(rules_main, n=10, by= "confidence")
inspect(top10_main)

rules_Tag0_main <- subset(rules_main, subset = rhs %in% "tag=0" & lift > 1)
rules_Tag1_main <- subset(rules_main, subset = rhs %in% "tag=1" & lift > 1)

inspect(head(rules_Tag0_main, n=5, by="confidence"))

# lhs                                                                           rhs     support   confidence lift     count
# [1] {n.resi=more_than_3_yrs,n.pltrades.12mn=0,n.inq.6mn=0}                     => {tag=0} 0.1001370 0.9917749  1.035418 6873 
# [2] {n.resi=more_than_3_yrs,n.pltrades.6mn=0,n.pltrades.12mn=0,n.inq.6mn=0}    => {tag=0} 0.1001370 0.9917749  1.035418 6873 
# [3] {n.resi=more_than_3_yrs,n.90dpd.12mn=0,n.pltrades.12mn=0}                  => {tag=0} 0.1012734 0.9915835  1.035218 6951 
# [4] {n.resi=more_than_3_yrs,n.90dpd.12mn=0,n.pltrades.6mn=0,n.pltrades.12mn=0} => {tag=0} 0.1012734 0.9915835  1.035218 6951 
# [5] {n.resi=more_than_3_yrs,n.90dpd.6mn=0,n.90dpd.12mn=0,n.pltrades.12mn=0}    => {tag=0} 0.1012734 0.9915835  1.035218 6951 

inspect(head(rules_Tag1_main, n=5, by="confidence"))

# plotting the top 5 rules
plot(head(rules_Tag0_main, n=5, by = "confidence"),method="graph",engine = 'interactive',shading=NA)


## -------------- WOE & IV ANALYSIS --------------------- ##

#Loading datasets for WOE Analysis
woe_data <- main

str(woe_data)

# Removing Application ID
woe_data <- woe_data[,-1]

# Splitting woe_data into test and train

set.seed(101)

split_indices <- sample.split(woe_data$tag, SplitRatio = 0.70)

train_woe <- woe_data[split_indices, ]

test_woe <- woe_data[!split_indices, ]

nrow(train_woe)/nrow(woe_data)

nrow(test_woe)/nrow(woe_data)

#Looking at the data structure
str(train_woe)
str(test_woe)

IV_main <- create_infotables(data=train_woe, 
                             valid=test_woe,
                             y="tag",
                             parallel=FALSE)

# Looking at the top Information Values
knitr::kable(head(IV_main$Summary))

# Top variables are:
#  |   |Variable        |        IV|   PENALTY|     AdjIV|
#  |:--|:---------------|---------:|---------:|---------:|
#  |17 |n.trades.12mn   | 0.3121446| 0.0421720| 0.2699726|
#  |15 |avg.cc          | 0.3232310| 0.0593382| 0.2638928|
#  |21 |n.inq.12mn      | 0.2990086| 0.0485914| 0.2504172|
#  |19 |n.pltrades.12mn | 0.3016099| 0.0531713| 0.2484386|
#  |11 |n.30dpd.6mn     | 0.2586435| 0.0245893| 0.2340541|
#  |24 |n.trades        | 0.2490031| 0.0273583| 0.2216448|

woe_table <- data.frame(IV_main$Summary)

# Plotting the important variables as per IV
ggplot(data=woe_table, aes(x=reorder(Variable,AdjIV),y=AdjIV)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Woe- Important variables based on Information value analysis") +xlab("Variables")

## Analysing WOE Patterns

# The IV$Tables object returned by Information is simply a list of dataframes 
# that contains the WOE tables for all variables in the input dataset.
# the penalty and IV columns are cumulative.

knitr::kable(IV_main$Tables$n.trades.12mn)

## Plotting WOE Patterns

# Plotting WOE for months_current residence
plot_infotables(IV_main, "n.trades.12mn", show_values=TRUE)

# plotting multiple variables
plot_infotables(IV_main, IV_main$Summary$Variable[1:4], same_scales=TRUE)

## REPLACING DATA WITH WOE VALUES 

# Creating bins with the main dataset
bins <- woebin(dt = woe_data,y = "tag")

# Plotting the bins of a variable for checking (e.g. outbal)
woebin_plot(bins$outbal)

# Replacing the data with corresponding WOE values
woe_final <- data.frame(woebin_ply(woe_data,bins))

View(woe_final)

IV_values <- iv(dt = woe_data,y = "tag")
IV_values

# Checking for NA values
sapply(woe_final, function(x) length(which(is.na(x))))
# all NAs have been imputed

# changing the column names
names(woe_final) <- c('tag', 'gender', 'mar.st', 'n.dep', 'edu', 'prof', 
                      'type.resi', 'n.resi', 'n.comp', 'n.90dpd.6mn', 'n.60dpd.6mn', 'n.30dpd.6mn', 
                      'n.90dpd.12mn', 'n.60dpd.12mn', 'n.30dpd.12mn',
                      'avg.cc', 'n.trades.6mn', 'n.trades.12mn',
                      'n.pltrades.6mn', 'n.pltrades.12mn', 'n.inq.6mn',
                      'n.inq.12mn', 'homeln', 'outbal', 'n.trades', 'autoln', 'income_group', 'age_group')


##------------------------- MODEL BUILDING: DEMOGRAPHIC DATA (WOE VALUES) -------------------------##

# Subsetting demographic data from overall data
demo <- woe_final[,1:9]

## Splitting demograhic data into train and test data

set.seed(101)

split_indices <- sample.split(demo$tag, SplitRatio = 0.70)

train0 <- demo[split_indices, ]

test0 <- demo[!split_indices, ]

nrow(train0)/nrow(demo)

nrow(test0)/nrow(demo)

# ----------- Model: LOGISTIC REGRESSION --------- #

# Running a logistic regression with 'tag' as target variable
# and all other variables as independent variables

logistic_1 <- glm(tag ~ ., family = "binomial", data = train0)

summary(logistic_1)

#----------------------------#  

# Using stepwise algorithm for removing insignificant variables 

#logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain:
#logistic_2

logistic_2 <- glm(formula = tag ~ n.dep + prof + n.resi + n.comp, family = "binomial", 
                  data = train0)

# checking vif for logistic_2 

vif(logistic_2)
# All VIFs are below 2
# There is no significant Multicollinearity

summary(logistic_2)

# Profession is insignificant
# Let's remove it
logistic_3 <- glm(formula = tag ~ n.dep + n.resi + n.comp, family = "binomial", 
                  data = train0)

summary(logistic_3)

# No. of dependents is less significant
# Let's remove it to form a simpler model
logistic_4 <- glm(formula = tag ~ n.resi + n.comp, family = "binomial", 
                  data = train0)

summary(logistic_4)
# All variables are highly significant
# logistic_4 is our final model

log_model_demographic <- logistic_4

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_log_demo <- predict(log_model_demographic, newdata = test0[, -1], type = "response")
summary(predictions_log_demo)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's find out the optimal probalility cutoff 

# changing the levels of tag variable in test data
test0$tag <- as.factor(ifelse(test0$tag == 0, "Good", "Bad"))

# Creating a function to get different values of Accuracy, True Positive Rate (Sensitivity)
# and True Negative Rate (Specitivity)

perform_fn <- function(cutoff) 
  {
    predicted_response_demo <- factor(ifelse(predictions_log_demo >= cutoff, "Bad", "Good"))
    conf <- confusionMatrix(predicted_response_demo, test0$tag, positive = "Good")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting curves of Accuracy, Sensitivity and Specitivity together
# to arrive at optimal cutoff 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.4)][1]
cutoff
# 0.039 i.e. 3.97%


# Let's choose a cutoff value of 3.97% for final model

predicted_response_demo <- factor(ifelse(predictions_log_demo >= 0.039, "Bad", "Good"))

conf_final_demo <- confusionMatrix(predicted_response_demo, test0$tag, positive = "Good")

conf_final_demo

acc <- conf_final_demo$overall[1]

sens <- conf_final_demo$byClass[1]

spec <- conf_final_demo$byClass[2]

acc
# 49.12%

sens
# 48.55%

spec
# 62.09%

# ROC curve for model build using demographic data only
roc_tag1 <- ifelse(test0$tag == "Bad", 1,0)
roc(roc_tag1, predictions_log_demo, plot = TRUE, legacy.axes = TRUE, auc = TRUE, 
    print.auc = TRUE, percent = TRUE, xlab = "False Positive %", ylab = "True Positive %", col = "blue")

# AUC 57.8%

##-------------------------------------------------------------------------- ##

## ------------- MODEL BUILDING: DEMOGRAPHIC + CREDIT BUREAU DATA ---------- ##

#Looking at the data structure with overall variables 
str(woe_final)

## Splitting overall data into train and test data

set.seed(101)

split_indices <- sample.split(woe_final$tag, SplitRatio = 0.70)

train <- woe_final[split_indices, ]

test <- woe_final[!split_indices, ]

nrow(train)/nrow(woe_final)

nrow(test)/nrow(woe_final)

## ------ Model: LOGISTIC REGRESSION ---------- ##

# Running a logistic regression with 'tag' as target variable
# and all other variables as independent variables

logistic2_1 <- glm(tag ~ ., family = "binomial", data = train)

summary(logistic2_1)

# Let's remove the variables that we had removed
# during model building in demographic data

logistic2_1 <- glm(tag ~ n.comp + n.resi + n.90dpd.6mn + 
                     n.60dpd.6mn + n.30dpd.6mn + 
                     n.90dpd.12mn + n.60dpd.12mn + 
                     n.30dpd.12mn + avg.cc + 
                     n.trades.6mn + n.trades.12mn + 
                     n.pltrades.6mn + n.pltrades.12mn + 
                     n.inq.6mn + n.inq.12mn + 
                     homeln + outbal + n.trades + 
                     autoln + income_group + age_group, family = "binomial", data = train)

summary(logistic2_1)

#----------------------------#  

# Using stepwise algorithm for removing insignificant variables 

# logistic2_2 <- stepAIC(logistic2_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain_reg:
# logistic2_2

logistic2_2 <- glm(formula = tag ~ n.90dpd.12mn + n.30dpd.12mn + avg.cc + n.trades.12mn + 
                     n.pltrades.6mn + n.inq.12mn, family = "binomial", data = train)

# checking vif for logistic2_2 
vif(logistic2_2)

# Looking at p-values
summary(logistic2_2)

# n.pltrades.6mn has high VIF
# it is also insignificant as it has high p-value
# Let's remove it

logistic2_3 <- glm(formula = tag ~ n.90dpd.12mn + n.30dpd.12mn + avg.cc + n.trades.12mn + 
                     n.inq.12mn, family = "binomial", data = train)

# checking vif 
vif(logistic2_3)

# Looking at p-values
summary(logistic2_3)

# n.90dpd.12mn has slightly high VIF
# It is also less significant
# Let's remove it
logistic2_4 <- glm(formula = tag ~ n.30dpd.12mn + avg.cc + n.trades.12mn + 
                     n.inq.12mn, family = "binomial", data = train)

# checking vif 
vif(logistic2_4)
# n.trades.12mn and n.inq.12mn have slightly high VIF

# let's see if they are correlated
cor(train$n.trades.12mn, train$n.inq.12mn)
# There is high positive correlation

# Looking at p-values
summary(logistic2_4)

# Let's remove n.trades.12mn as p-value is higher than n.inq.12mn

logistic2_5 <- glm(formula = tag ~ n.30dpd.12mn + avg.cc + 
                     n.inq.12mn, family = "binomial", data = train)

vif(logistic2_5)
# No multicollinearity

summary(logistic2_5)
# all variables are highly significant

# Let's consider this as the final logistic model
log_model <- logistic2_5

# Predicting probabilities of responding for the test data
predictions_log <- predict(log_model, newdata = test[, -1], type = "response")
summary(predictions_log)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression

# Let's find out the optimal probalility cutoff 

# changing the levels and variable type of tag variable in test data
test$tag <- as.factor(ifelse(test$tag == 0, "Good", "Bad"))

# Creating a function to get different values of Accuracy, True Positive Rate (Sensitivity)
# and True Negative Rate (Specitivity)
perform_fn <- function(cutoff)
{
  predicted_response_log <- factor(ifelse(predictions_log >= cutoff, "Bad", "Good"))
  conf_log <- confusionMatrix(predicted_response_log, test$tag, positive = "Good")
  acc <- conf_log$overall[1]
  sens <- conf_log$byClass[1]
  spec <- conf_log$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting curves of Accuracy, Sensitivity and Specitivity together
# to arrive at optimal cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.2)][1]
cutoff
# 0.03969 i.e. 3.97%


# Let's choose a cutoff value of 3.97% for final model

predicted_response_log <- factor(ifelse(predictions_log >= 0.0397, "Bad", "Good"))

conf_final_log <- confusionMatrix(predicted_response_log, test$tag, positive = "Good")

conf_final_log

acc <- conf_final_log$overall[1]

sens <- conf_final_log$byClass[1]

spec <- conf_final_log$byClass[2]

acc
# 55%

sens
# 54%

spec
# 73%

## ------------------ Model: Random Forest ------------------- ##

set.seed(700)

# Building a Random Forest model using variables in the final logistic model
model.rf <- randomForest(tag ~ n.30dpd.12mn + avg.cc + 
                           n.inq.12mn, data=train, proximity=FALSE,
                         ntree=100, mtry=2, do.trace=TRUE,na.action=na.omit)
model.rf
pred_rf <- predict(model.rf, newdata=test[,-1], type = "response")
summary(pred_rf)

# Let's find out the optimal probalility cutoff 

# Creating a function to get different values of Accuracy, True Positive Rate (Sensitivity)
# and True Negative Rate (Specitivity)
perform_fn <- function(cutoff) 
{
  predicted_response_rf <- factor(ifelse(pred_rf >= cutoff, "Bad", "Good"))
  conf_rf <- confusionMatrix(predicted_response_rf, test$tag, positive = "Good")
  acc <- conf_rf$overall[1]
  sens <- conf_rf$byClass[1]
  spec <- conf_rf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting curves of Accuracy, Sensitivity and Specitivity together
# to arrive at optimal cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)][1]
cutoff
# 0.0496 i.e. 4.96%

# Let's choose a cutoff value of 4.96% for final model

predicted_response_rf <- factor(ifelse(pred_rf >= 0.0496, "Bad", "Good"))

conf_final_rf <- confusionMatrix(predicted_response_rf, test$tag, positive = "Good")

conf_final_rf

acc <- conf_final_rf$overall[1]

sens <- conf_final_rf$byClass[1]

spec <- conf_final_rf$byClass[2]

acc
# 63.39%

sens
# 63.46%

spec
# 61.75%

# ROC curve for models built using all data with WOE values
roc_tag2 <- ifelse(test$tag == "Bad", 1,0)
roc(roc_tag2,predictions_log , plot = TRUE, legacy.axes = TRUE, auc = TRUE, 
    print.auc = TRUE, percent = TRUE, xlab = "False Positive %", ylab = "True Positive %", col = "blue")
plot.roc(roc_tag2, pred_rf, percent = TRUE, col = "darkgreen", print.auc = TRUE, add = TRUE, print.auc.y = 40)
legend("bottomright", legend = c("Logistic Regression", "RandomForest"), col = c("blue", "darkgreen"), lwd = 4)

# AUC Scores
# Logistic Regression 67.5%
# Random Forest 67.9%

# The Random Forest model has a better AUC score of 67.9% 


## ------------------ Model Building using regular values for comparison --------------- ##

#Looking at the data structure
str(main)

#converting logical variables into factor
main$homeln <- as.factor(main$homeln)
main$autoln <- as.factor(main$autoln)

# creating dummy variables for factor attributes
main_cat <- main[, c('gender','mar.st','n.dep','edu','prof','type.resi',
                     'n.resi','n.comp','homeln','autoln','income_group','age_group')] #dataset with only cateorical variables

dummies <- data.frame(sapply(main_cat, 
                            function(x) data.frame(model.matrix(~x-1,data = main_cat))[,-1]))

# continuous variables in overall data
main_con <- main[, c('n.90dpd.6mn', 'n.60dpd.6mn', 'n.30dpd.6mn', 
                     'n.90dpd.12mn', 'n.60dpd.12mn', 'n.30dpd.12mn',
                     'avg.cc', 'n.trades.6mn', 'n.trades.12mn',
                     'n.pltrades.6mn', 'n.pltrades.12mn', 'n.inq.6mn',
                     'n.inq.12mn', 'outbal', 'n.trades')]

#merging the target variable with continuous and dummy variables
main_final <- cbind(main$appid, main$tag, main_con, dummies)

#renaming appid and tag
colnames(main_final)[1] <-"appid"
colnames(main_final)[2] <- "tag"

## Splitting main_reg data into train and test data

set.seed(101)

split_indices2 <- sample.split(main_final$tag, SplitRatio = 0.70)

train2 <- main_final[split_indices2, ]

test2 <- main_final[!split_indices2, ]

nrow(train2)/nrow(main_final)

nrow(test2)/nrow(main_final)

# storing appid for future use
train_appid <- train2$appid
test_appid <- test2$appid

#removing appid from train and test data
train2$appid <- NULL
test2$appid <- NULL

## ------ Model: LOGISTIC REGRESSION ---------- ##

# Running a logistic regression with tag as target variable
# and all other variables as independent variables

logistic3_1 <- glm(tag ~ ., family = "binomial", data = train2)

summary(logistic3_1)


#----------------------------#  

# Using stepwise algorithm for removing insignificant variables 

# logistic3_2 <- stepAIC(logistic3_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain_reg:
# logistic3_2

logistic3_2 <- glm(formula = tag ~ n.90dpd.6mn + n.90dpd.12mn + n.30dpd.12mn + 
                     avg.cc + n.pltrades.6mn + n.pltrades.12mn + n.inq.6mn + n.inq.12mn + 
                     outbal + n.trades + n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr + homeln, 
                   family = "binomial", data = train2)

# checking vif 
vif(logistic3_2)

# Looking at p-values
summary(logistic3_2)

# Outstanding Balance has extremely high VIF
# It also is insignificant
# Let's remove it

logistic3_3 <- glm(formula = tag ~ n.90dpd.6mn + n.90dpd.12mn + n.30dpd.12mn + 
                     avg.cc + n.pltrades.6mn + n.pltrades.12mn + n.inq.6mn + n.inq.12mn + 
                     n.trades + n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr + homeln, 
                   family = "binomial", data = train2)

# checking vif
vif(logistic3_3)

# Looking at p-values
summary(logistic3_3)

# No.of.times.DPD has values for 6 months as well as 12 months
# the values in 12 months include those in 6 months
# Hence, it leads to multicollinearity

# Let's remove all the variables reprensent last 6 months 
# which also have related 12 months variable 

logistic3_4 <- glm(formula = tag ~ n.90dpd.12mn + n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.trades + n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr + homeln, 
                   family = "binomial", data = train2)

# checking vif
vif(logistic3_4)

# Looking at p-values
summary(logistic3_4)

# n.trades has a high vif
# it is also less significant
# Let's remove it

logistic3_5 <- glm(formula = tag ~ n.90dpd.12mn + n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr + homeln, 
                   family = "binomial", data = train2)

# checking vif
vif(logistic3_5)

# Looking at p-values
summary(logistic3_5)

# n.90dpd.12mn and n.30dpd.12mn have a high vif
# n.90dpd.12mn is less significant
# Let's remove it

logistic3_6 <- glm(formula = tag ~ n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr + homeln, 
                   family = "binomial", data = train2)

# checking vif
vif(logistic3_6)
# multicollinearity is low (no two variables are highly correlated)

# Looking at p-values
summary(logistic3_6)

# homeln is highly insignificant
# let's remove
logistic3_7 <- glm(formula = tag ~ n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.dep.x2 + prof.xSE + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr, 
                   family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_7)

# Self Employed Profession is insignificant. Let's remove it
logistic3_8 <- glm(formula = tag ~ n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.dep.x2 + n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr, 
                   family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_8)

# Two Dependents is insignificant. Let's remove it
logistic3_9 <- glm(formula = tag ~ n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.resi.xless_than_1_yr + 
                     n.resi.xmore_than_3_yrs + n.comp.xLess_than_a_yr, 
                   family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_9)

# n.resi.xmore_than_3_yrs is less significant. Let's remove
logistic3_10 <- glm(formula = tag ~ n.30dpd.12mn + 
                     avg.cc + n.pltrades.12mn + n.inq.12mn + 
                     n.resi.xless_than_1_yr + 
                     n.comp.xLess_than_a_yr, 
                   family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_10)

# n.comp.xLess_than_a_yr is less significant. Let's remove it
logistic3_11 <- glm(formula = tag ~ n.30dpd.12mn + 
                      avg.cc + n.pltrades.12mn + n.inq.12mn + 
                      n.resi.xless_than_1_yr, 
                    family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_11)

# n.resi.xless_than_1_yr is less significant (two stars). Let's remove it
logistic3_12 <- glm(formula = tag ~ n.30dpd.12mn + 
                      avg.cc + n.pltrades.12mn + n.inq.12mn, 
                    family = "binomial", data = train2)

# Looking at p-values
summary(logistic3_12)
# all variables are highly significant


# Let's consider this as the final logistic model
log_model_reg <- logistic3_12

# Predicting probabilities of responding for the test data
predictions_log_reg <- predict(log_model_reg, newdata = test2[, -1], type = "response")
summary(predictions_log_reg)

#--------------------------------------------------------- 

## Model Evaluation: Logistic Regression (with regular values)

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

# converting target variable to factor with levels 'Good' and 'Bad'
test2$tag <- factor(ifelse(test2$tag == 0, "Good", "Bad"))

# Creating a function to get different values of Accuracy, True Positive Rate (Sensitivity)
# and True Negative Rate (Specitivity)
perform_fn <- function(cutoff) 
{
  predicted_response_log_reg <- factor(ifelse(predictions_log_reg >= cutoff, "Bad", "Good"))
  conf_log <- confusionMatrix(predicted_response_log_reg, test2$tag, positive = "Good")
  acc <- conf_log$overall[1]
  sens <- conf_log$byClass[1]
  spec <- conf_log$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting curves of Accuracy, Sensitivity and Specitivity together
# to arrive at optimal cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.4)][1]
cutoff
# 0.0298 i.e. 2.98%


# Let's choose a cutoff value of 2.98% for final model

predicted_response_log_reg <- factor(ifelse(predictions_log_reg >= 0.0298, "Bad", "Good"))

conf_final_log_reg <- confusionMatrix(predicted_response_log_reg, test2$tag, positive = "Good")

conf_final_log_reg

acc <- conf_final_log_reg$overall[1]

sens <- conf_final_log_reg$byClass[1]

spec <- conf_final_log_reg$byClass[2]

acc
# 46.64%

sens
# 45.13%

spec
# 80.99%

## ------------------ Model: Random Forest (Regular values) ------------------- ##

set.seed(700)

# Building a Random Forest model using variables in the final logistic model
model.rf_2 <- randomForest(tag ~ n.30dpd.12mn + 
                           avg.cc + n.pltrades.12mn + n.inq.12mn, data=train2, proximity=FALSE,
                         ntree=100, mtry=2, do.trace=TRUE, na.action=na.omit)
model.rf_2
pred_rf_2 <- predict(model.rf_2, newdata=test2[,-1], type = "response")
summary(pred_rf_2)

# Let's find out the optimal probalility cutoff 

# Creating a function to get different values of Accuracy, True Positive Rate (Sensitivity)
# and True Negative Rate (Specitivity)
perform_fn <- function(cutoff) 
{
  predicted_response_rf_2 <- factor(ifelse(pred_rf_2 >= cutoff, "Bad", "Good"))
  conf_rf <- confusionMatrix(predicted_response_rf_2, test2$tag, positive = "Good")
  acc <- conf_rf$overall[1]
  sens <- conf_rf$byClass[1]
  spec <- conf_rf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting curves of Accuracy, Sensitivity and Specitivity together
# to arrive at optimal cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)][1]
cutoff
# 0.0298 i.e. 2.98%

# Let's choose a cutoff value of 2.98% for final model

predicted_response_rf_2 <- factor(ifelse(pred_rf_2 >= 0.0298, "Bad", "Good"))

conf_final_rf_2 <- confusionMatrix(predicted_response_rf_2, test2$tag, positive = "Good")

conf_final_rf_2

acc <- conf_final_rf_2$overall[1]

sens <- conf_final_rf_2$byClass[1]

spec <- conf_final_rf_2$byClass[2]

acc
# 62.07%

sens
# 62.35%

spec
# 55.53%

# ROC curve for models built using all data with regular values
roc_tag3 <- ifelse(test2$tag == "Bad", 1,0)
roc(roc_tag3,predictions_log_reg , plot = TRUE, legacy.axes = TRUE, auc = TRUE, 
    print.auc = TRUE, percent = TRUE, xlab = "False Positive %", ylab = "True Positive %", col = "blue")
plot.roc(roc_tag3, pred_rf_2, percent = TRUE, col = "darkgreen", print.auc = TRUE, add = TRUE, print.auc.y = 40)
legend("bottomright", legend = c("Logistic Regression", "RandomForest"), col = c("blue", "darkgreen"), lwd = 4)

# AUC Scores
# Logistic Regression 67.1%
# Random Forest 61.2%

# Logistic Regression peforms better when we take regular values

#-------------------------------------------------------------------#

## FINAL MODEL ##

# We achieved the highest AUC score of 67.9%
# with a Random Forest model using WOE values

# Hence, this model will be our final model

final_model <- model.rf

# Plotting the ROC curve for the final model again
roc(roc_tag2, pred_rf, plot = TRUE, legacy.axes = TRUE, auc = TRUE, 
    print.auc = TRUE, percent = TRUE, xlab = "False Positive %", ylab = "True Positive %", col = "red", lwd = 4)


## -------------- APPLICATION SCORECARD ------------------ ##

# Building an application scorecard 
# with the good to bad odds of 10 to 1 
# at a score of 400 doubling every 20 points. 

str(test)
colnames(test)

logit = pred_rf

result = tibble(logit = logit
                , odds = exp(logit)
                , prob = odds / (odds + 1)
                , prob_ctrl = pred_rf_2)

pdo = 20   # points to double the odds 
points = 400   # Target Score Value
odds0 = 10   # Inverted Target Odds (to)

factor = pdo / log(2)
offset = (points - factor)*log(odds0)


result$score_ctrl = (offset - factor)*result$logit
result$score = round(result$score_ctrl)

summary(result$score)

result %>%
  arrange(desc(score))


# Evaluation of Application Scores #

test_eval <- cbind(test, result$score)
colnames(test_eval)
colnames(test_eval)[29] <- "score"

# looking at scores of customers predicted to default
rejected <- subset(test_eval, tag == "Bad") 

# Note: Lower the score, higher the chance of default

#Looking at minimum and maximum scores of predicted defaulters
min(rejected$score)  # 9
max(rejected$score)  # 90

# looking at scores of customers predicted NOT to default
accepted <- subset(test_eval, tag == "Good")

#Looking at minimum and maximum scores of predicted non-defaulters
min(accepted$score)  # 7
max(accepted$score)  # 90

# Calculation of cutoff score

# We have previously calculated a probability cutoff of 0.029 or 3%
# We can use this to calculate 3% of the total of distinct scores
# to find cutoff

# calculating total of distinct scores
total_score = sum(unique(test_eval$score))
total_score

# Cutoff for randomForest model
#cutoff = 0.0496 or,
cutoff = 0.05

# Cutoff score
cutoff*total_score
# 71 (approx)

# looking at all scores
summary(factor(test_eval$score))

# We have a score = 70

# Thus, cutoff score = 70

# This means, a Credit card applicant with
# a score below 70 can be rejected


#-------------------------------------------------------------------#

##----------------------- PROJECT ASSESSMENT ----------------------##

# Assessing the financial benefit of the project

# Our Business problem has a Dual objective
# To minimize credit loss by:
# 1. Not giving credit cards to Bad customers
# 2. Not denying credit cards to Good customers (if case arises)

# We will first tackle the first objective

# Creating a new dataframe "test_predictions"
# with  "appid", "tag" (actual response), "predicted_probs", "predicted_response",
# score and "outbal" (Outstanding Balance)

assess <- data.frame(cbind(test_appid, test$tag, pred_rf, predicted_response_rf, result$score, test2$outbal))
names(assess) <- c('appid', 'actual_tag', 'predicted_prob', 'predicted_tag', 'score', 'outbal')

str(assess)

# correcting the tag levels
assess$actual_tag <- ifelse(assess$actual_tag == 2, 0, 1)
assess$predicted_tag <- ifelse(assess$predicted_tag == 2, 0, 1)

View(assess)

# average outstanding balance
mean(assess$outbal)
# 1264281

# sorting the probabilities in decreasing order 
assess <- assess[order(assess$predicted_prob, decreasing = T), ]
head(assess)

# creating data objects for lift chart

# plotting the Lift chart

lift <- function(labels , predicted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Creating a Table of Cumulative Gain and Lift
# targeting Bad Customers

LG = lift(assess$actual_tag, assess$predicted_prob, groups = 10)

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of Bad Applicants")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="blue",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# View Table of Cumulative Gain and Lift
View(LG)

# This Gain chart shows that we can
# detect 75% of Bad Customers by targeting 50% of the Customers

# Roughly, in money terms, we will be saving
# an average potential credit loss of

mean(assess$outbal)*868*0.75   # (mean outstanding balance*Bad customers in test data*75%)
# 8.23 million USD by targeting 50% of total customers


# Now coming to the second business objective,
# i.e. Not denying credit cards to Good customers
# resulting in revenue for the firm

# Switching labels for tuning Lift Chart
actual_tag2 <- ifelse(assess$actual_tag == 1, "Bad", "Good")
actual_tag2 <- ifelse(actual_tag2 == "Bad", 0, 1)

predicted_prob2 <- (1-assess$predicted_prob)
# Creating a Table of Cumulative Gain and Lift 
# targeting Good Customers
LG2 <- lift(actual_tag2, predicted_prob2, groups = 10)

# Gain Chart 

plot(LG2$bucket,LG2$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of Good Applicants")

# Lift Chart 

plot(LG2$bucket,LG2$Cumlift,col="blue",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# View Table of Cumulative Gain and Lift
View(LG2)

# The Gain and Lift chart shows that
# Good customers are evenly distributed across the data

# Hence, no business action is suggested
# to decrease credit loss when targeting good customers

# Final Financial Benefit of the Project:

# This project helps prevent credit loss
# It identifies 75% of Bad Customers
# by targeting only 50% of overall customers
# roughly saving the firm 8.23 million USD.

## ----------------------- END --------------------------------- #




