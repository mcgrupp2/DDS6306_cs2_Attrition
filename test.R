#these could be edits


#install.packages("tidyverse")
#install.packages("naniar")
#install.packages("caret")
#install.packages('e1071', dependencies = TRUE)
#install.packages("mlbench")
#install.packages("fastDummies")
#install.packages("xgboost")


library(plyr)
library(tidyverse)
library(naniar)
library(GGally)
library(caret)
library(e1071)
library(mlbench)
library(fastDummies)
library(xgboost)


#Read the raw csv with attrition and salaries

raw_df <- read.csv("attritApp/data/CaseStudy2-data.csv")
raw_df 

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


#~~~~~~~~~~~~~~~~~~~~Feature Selection~~~~~~~~~~~~~~~~~~~~~~~~~~



set.seed(22)

fs_df <- cbind(raw_df[,2], raw_df[,4:36])

fs_df <- dummy_cols(fs_df)

fs_df

dim(fs_df)



names(fs_df)

f_df <- cbind(fs_df[,1],    
              fs_df[,3],
              fs_df[,5:6],   
              fs_df[,10],    
              fs_df[,12],
              fs_df[,13:14], 
              fs_df[,16],    
              fs_df[,19:20], 
              fs_df[,24:25],
              fs_df[,27:60],
              fs_df[,62:63]) 

dim(f_df) 

names(f_df)

names(f_df)[[1]] <- "Age"
names(f_df)[[2]] <- "DailyRate"
names(f_df)[[5]] <- "EnvironmentSatisfaction"
names(f_df)[[6]] <-  "HourlyRate"
names(f_df)[[9]] <- "JobSatisfaction"

#names(f_df)

#fs_df <- dummyVars(" ~ .", data = fs_df)

#fs_df

#f_df <- data.frame(predict(fs_df, newdata = fs_df))

#str(f_df)

#gg_miss_var(f_df)


#nums <- sapply(f_df, is.numeric)
#data.numeric <- f_df[ , nums]

#data.without_na <- na.omit(data.numeric)
#cor_matrix <- cor(data.without_na)


#~~~~~~~~~~~~~~~~~~~~~~~Remove Redundant Features

correlationMatrix <- cor(f_df)#use="pairwise.complete.obs")

gg_miss_var(correlationMatrix)

gg_miss_var(as.data.frame(correlationMatrix))

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)

print(highlyCorrelated)

print(correlationMatrix[highlyCorrelated,])


tmp_df <- cbind(f_df, raw_df[,])



#~~~~~~~~~~~~~~~~~~~~~Rank Features By Importance



control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(f_df, raw_df[,3], method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



#~~~~~~~~~~~~~~~~~~~~Feature Selection (Automatic)------LONG LOAD

set.seed(22)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(f_df, raw_df[,3], sizes=c(1:50), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

p_df <- f_df[,results$variables[1:26,4]]



names(f_df)

p_df


m_df <- data.matrix(p_df)


t_splt <- round(dim(p_df)[[1]] * .7)

#t_splt

train_data <- m_df[1:t_splt,]
#train_data

test_data <- m_df[-(1:t_splt),]
#test_data

#diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]

#dtrain <- xgb.D








