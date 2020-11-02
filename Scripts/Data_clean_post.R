#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml 
# Author: Zhipeng Zhou
# Data: 1 November 2020
# Contact: zhipeng.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/zzpen/Desktop/PS3")
post_raw_data <- read_dta("Inputs/usa_00002.dta.gz")

# Add the labels
post_raw_data <- labelled::to_factor(post_raw_data)

# Just keep some variables that may be of interest
post_reduced_data <- 
  post_raw_data %>% 
  select(region,
         sex, 
         age,
         race, 
         hispan,
         ftotinc)

# Modify the relative variable
post_reduced_data$gender <- NA
post_reduced_data<- within(post_reduced_data, {
  gender[sex == "male"] <- 1
  gender[sex == "female"] <- 0
})
post_reduced_data$black_or_not <- 0
post_reduced_data<- within(post_reduced_data, {
  black_or_not[grepl("black", race)] <- 1
})
post_reduced_data$chinese_or_not <- 0
post_reduced_data<- within(post_reduced_data, {
  chinese_or_not[grepl("chinese", race)] <- 1
})
post_reduced_data$japanese_or_not <- 0
post_reduced_data<- within(post_reduced_data, {
  japanese_or_not[grepl("japanese", race)] <- 1
})
post_reduced_data$other_race <- 0
post_reduced_data<- within(post_reduced_data, {
  other_race[grepl("other", race)] <- 1
})
post_reduced_data$mexican_or_not <- 0
post_reduced_data<- within(post_reduced_data, {
  mexican_or_not[grepl("mexican", hispan)] <- 1
})
post_reduced_data <- post_reduced_data %>%
  mutate(age = replace(age, age == "less than 1 year old", 1)) %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>%
  filter(as.integer(age) > 17)
post_reduced_data$age_under_30 <- 0
post_reduced_data <- within(post_reduced_data, {
  age_under_30[as.integer(age) < 31] <- 1
})
post_reduced_data$income_in_range <- 0
post_reduced_data <- within(post_reduced_data, {
  income_in_range[200000 <= as.integer(ftotinc) & as.integer(ftotinc)< 249999] <- 1
})
post_reduced_data <- subset(post_reduced_data, select = -c(sex, race, hispan, ftotinc))
post_reduced_data <- rename(post_reduced_data, c("sex"="gender"))
post_reduced_data <- na.omit(post_reduced_data)

Trump_census_data <- 
  post_reduced_data %>%
  count(region, sex, black_or_not, chinese_or_not, other_race, mexican_or_not, age_under_30, income_in_range) %>%
  group_by(region, sex, black_or_not, chinese_or_not, other_race, mexican_or_not, age_under_30, income_in_range) %>%
  filter(n >= 10)

Biden_census_data <- 
  post_reduced_data %>%
  count(region, sex, japanese_or_not, black_or_not, chinese_or_not, other_race, mexican_or_not, age_under_30, income_in_range) %>%
  group_by(region, sex, japanese_or_not, black_or_not, chinese_or_not, mexican_or_not, income_in_range) %>%
  filter(n >= 10)

write_csv(Trump_census_data, "outputs/trump_census_data.csv")
write_csv(Biden_census_data, "outputs/biden_census_data.csv")
