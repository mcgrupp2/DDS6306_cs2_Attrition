#these could be edits


#install.packages("tidyverse")
#install.packages("naniar")

library(plyr)
library(tidyverse)
library(naniar)
library(GGally)

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

#Make shiny app with multiselect input to do

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive


## Quick example, with and without colour
data(flea)
ggpairs(flea, columns = 2:4)
pm <- ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))
p_(pm)


glly_df <- raw_df[,c(2, 5, 7, 13, 15, 16, 17, 19, 20, 21, 22, 23, 27, 28, 30)]
ggpairs(glly_df)


























#~~~~~~~~~~~~~~Dendrogram-------------------------



iris2 <- iris[,-5]

d_iris <- dist(raw_df) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "complete")
iris_species <- rev(levels(iris[,5]))
#install.packages("dendextend")
library(dendextend)
dend <- as.dendrogram(hc_iris)

#dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=2) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- raw_df$ID #paste(as.character(iris[,5])[order.dendrogram(dend)],
                      #"(",labels(dend),")", 
                      #sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
#legend("topleft", legend = iris_species, fill = rainbow_hcl(3))

#install.packages("circlize")
library(circlize)


circlize_dendrogram(dend)



plot(dend)



