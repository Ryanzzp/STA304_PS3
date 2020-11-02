#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set.
# Author: Zhipeng Zhou
# Data: 1 November 2020
# Contact: zhipeng.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
library(leaps)
setwd("C:/Users/zzpen/Desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("Inputs/Data/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         gender,
         race_ethnicity,
         hispanic,
         household_income,
         state,
         age)

# Split ages into age groups
reduced_data$age_group <- NA
reduced_data<- within(reduced_data, {
  age_group[age > 10 & age <= 20] <- "11 - 20 years old"
  age_group[age > 20 & age <= 30] <- "21 - 30 years old"
  age_group[age > 30 & age <= 40] <- "31 - 40 years old"
  age_group[age > 40 & age <= 50] <- "41 - 50 years old"
  age_group[age > 50 & age <= 60] <- "51 - 60 years old"
  age_group[age > 60 & age <= 70] <- "61 - 70 years old"
  age_group[age > 70 & age <= 80] <- "71 - 80 years old"
  age_group[age > 80 & age <= 90] <- "81 - 90 years old"
  age_group[age > 90] <- "90+ years old"
})
reduced_data <- subset(reduced_data, select = -c(age))
# Remove all NA obs
reduced_data <- na.omit(reduced_data)

# Since the vote result is not binary, we make it binary in two model for Trump 
# and Biden separately
Trump_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))
Trump_data <- subset(Trump_data, select = -c(vote_2020))

Biden_data<-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))
Biden_data <- subset(Biden_data, select = -c(vote_2020))

# Then we check the correlation by p-value for each indepandent variables
summary(glm(vote_trump ~ ., data = Trump_data, family= "binomial"))
summary(glm(vote_biden ~ ., data = Biden_data, family= "binomial"))

# The variable of "state" is not that important and it somehow relative to the 
# variable "census", so we remove it from our data set
Trump_data <- subset(Trump_data, select = -c(state))
Biden_data <- subset(Biden_data, select = -c(state))

# Then to find the best model, we are going to use regsubsets() function, which 
# performs best subset selection by identifying the best model that contains a 
# given number of predictors, where best is quantified using RSS.
# First, for Trump
regfit_Trump=regsubsets(vote_trump ~ ., Trump_data, really.big=T)
summary(regfit_Trump)

# Select the relative variable
Trump_data<-
  Trump_data %>%
  mutate(age_under_30 = 
           ifelse(age_group=="11 - 20 years old" | age_group=="21 - 30 years old" , 1, 0),
         income_in_range = 
           ifelse(household_income == "$200,000 to $249,999", 1, 0),
         mexican_or_not = 
           ifelse(hispanic == "Mexican", 1, 0),
         black_or_not = 
           ifelse(race_ethnicity == "Black, or African American", 1, 0),
         chinese_or_not = 
           ifelse(race_ethnicity == "Asian (Chinese)", 1, 0),
         other_race = 
           ifelse(race_ethnicity == "Some other race", 1, 0),
         sex = 
           ifelse(gender == "Male",  1, 0))
Trump_data <- subset(Trump_data, select = -c(age_group, household_income, race_ethnicity, gender, hispanic))

# Then for Biden
regfit_Biden=regsubsets(vote_biden ~ ., Biden_data, really.big=T)
summary(regfit_Biden)

# Select the relative variable
Biden_data<-
  Biden_data %>%
  mutate(income_in_range = 
           ifelse(household_income == "$200,000 to $249,999", 1, 0),
         mexican_or_not = 
           ifelse(hispanic == "Mexican", 1, 0),
         black_or_not = 
           ifelse(race_ethnicity == "Black, or African American", 1, 0),
         chinese_or_not = 
           ifelse(race_ethnicity == "Asian (Chinese)", 1, 0),
         japanese_or_not = 
           ifelse(race_ethnicity == "Asian (Japanese)", 1, 0),
         sex = 
           ifelse(gender == "Male",  1, 0))
Biden_data <- subset(Biden_data, select = -c(age_group, household_income, race_ethnicity, gender, hispanic))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(Trump_data, "outputs/survey_trump_data.csv")
write_csv(Biden_data, "outputs/survey_biden_data.csv")