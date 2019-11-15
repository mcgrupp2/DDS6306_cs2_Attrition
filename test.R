#these could be edits


#install.packages("tidyverse")
#install.packages("naniar")

library(plyr)
library(tidyverse)
library(naniar)

raw_df <- read.csv("CaseStudy2-data.csv")
#raw_df 

naniar::gg_miss_var(raw_df)

names(raw_df)
