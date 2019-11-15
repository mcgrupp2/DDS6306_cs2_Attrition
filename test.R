#these could be edits


#install.packages("tidyverse")
#install.packages("naniar")

library(plyr)
library(tidyverse)
library(naniar)

#Read the raw csv with attrition and salaries

raw_df <- read.csv("CaseStudy2-data.csv")
#raw_df 

#No missing vals
gg_miss_var(raw_df)

names(raw_df)
str(raw_df)

raw_df %>% ggplot(aes(x = MonthlyIncome, y = ..count..)) + geom_histogram(bins = 50)

maxMonthlyIncome <- max(raw_df$MonthlyIncome)

minMonthlyIncome <- min(raw_df$MonthlyIncome)

rng <- maxMonthlyIncome - minMonthlyIncome

rng

#raw_df$WorkLifeBalance

#Compare worklifebalance to monthly income

raw_df %>% ggplot(aes(x=WorkLifeBalance, y=MonthlyIncome)) + geom_jitter()


raw_df %>% ggplot(aes(x=WorkLifeBalance, y=JobSatisfaction)) + geom_jitter()


#Maybe do some k-means to find clusters?
















