#these could be edits


#install.packages("tidyverse")
#install.packages("naniar")
#install.packages("caret")
#install.packages('e1071', dependencies = TRUE)
#install.packages("mlbench")
#install.packages("fastDummies")
#install.packages("xgboost")
#install.packages("clustMixType")
#install.packages("mlr")

library(plyr)
library(tidyverse)
library(naniar)
library(GGally)
library(caret)
library(e1071)
library(mlbench)
library(fastDummies)
library(xgboost)
library(clustMixType)
library(class)
library(mlr)


#Read the raw csv with attrition and salaries

raw_df <- read.csv("attritApp/data/CaseStudy2-data.csv")
raw_df 

#No missing vals
gg_miss_var(raw_df)

names(raw_df)
str(raw_df)

raw_df %>% ggplot(aes(x = MonthlyIncome, y = ..count..)) + geom_histogram(bins = 50)




#Compare worklifebalance to monthly income

raw_df %>% ggplot(aes(x=WorkLifeBalance, y=MonthlyIncome)) + geom_jitter()


raw_df %>% ggplot(aes(x=WorkLifeBalance, y=JobSatisfaction)) + geom_jitter()


#Maybe do some k-means to find clusters?

#Make shiny app with multiselect input to do

# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive







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

# Create dummy columns for categorical data

fs_df <- dummy_cols(fs_df)

fs_df

dim(fs_df)


names(fs_df)

# Remove categorical cols, replaced with dummies, result removed as well, can uncomment

f_df <- cbind(fs_df[,1],
              fs_df[,3],
              fs_df[,5:6],   
              fs_df[,10],    
              fs_df[,12],
              fs_df[,13:14], 
              fs_df[,16],    
              fs_df[,18:20], 
              fs_df[,23:25],
              fs_df[,27:60],
              fs_df[,62:63]) 


dim(f_df) 

names(f_df)

names(f_df)[[1]] <- "Age"
names(f_df)[[2]] <- "DailyRate"
names(f_df)[[5]] <- "EnvironmentSatisfaction"
names(f_df)[[6]] <-  "HourlyRate"
names(f_df)[[9]] <- "JobSatisfaction"

names(f_df)


#~~~~~~~~~~~~~~~~Scaling features


str(f_df)

str(raw_df)

fs_df %>% as.tibble()

str(f_df)



# divide by 1000

maxMonthlyIncome <- max(f_df$MonthlyIncome)

minMonthlyIncome <- min(f_df$MonthlyIncome)

MonthlyIncome_rng <- maxMonthlyIncome - minMonthlyIncome

maxMonthlyIncome

minMonthlyIncome

MonthlyIncome_rng


# Divide by 100

maxDailyRate <- max(f_df$DailyRate)

minDailyRate <- min(f_df$DailyRate)

DailyRate_rng <- maxDailyRate - minDailyRate

maxDailyRate

minDailyRate

DailyRate_rng


# divide by 1000


maxMonthlyRate <- max(f_df$MonthlyRate)

minMonthlyRate <- min(f_df$MonthlyRate)

MonthlyRate_rng <- maxMonthlyRate - minMonthlyRate

maxMonthlyRate

minMonthlyRate

MonthlyRate_rng


maxPercentSalaryHike <- max(raw_df$PercentSalaryHike)

minPercentSalaryHike <- min(raw_df$PercentSalaryHike)

PercentSalaryHike_rng <- maxPercentSalaryHike - minPercentSalaryHike

maxPercentSalaryHike

minPercentSalaryHike

PercentSalaryHike_rng


#colr <- c("DailyRate","MonthlyRate", "MonthlyIncome", "HourlyRate")

cola <- c("DailyRate", "DistanceFromHome", "HourlyRate", "MonthlyIncome", "MonthlyRate",      
          "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsInCurrentRole", 
          "YearsSinceLastPromotion", "YearsWithCurrManager")

#base_scaled_df <- f_df %>% mutate_at(vars(colr), list(~scale(.) %>% as.vector))

#base_scaled_df

all_scaled_df <- f_df %>% mutate_at(vars(cola), list(~scale(.) %>% as.vector))

all_scaled_df


#~~~~~~~~~~~~~~~~~~~~~~~Remove Redundant Features

correlationMatrix <- cor(all_scaled_df, use="pairwise.complete.obs")

#gg_miss_var(correlationMatrix)

gg_miss_var(as.data.frame(correlationMatrix))

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)#, names = TRUE)

#correlationMatrix[highlyCorrelated,]

print(highlyCorrelated)

print(all_scaled_df[,highlyCorrelated])

print(all_scaled_df[,-(highlyCorrelated)])

f_df <- all_scaled_df[,-(highlyCorrelated)]


#tmp_df <- cbind(f_df, raw_df[,])



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
results <- rfe(f_df, raw_df[,3], sizes=c(1:42), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)

p_inx <- predictors(results)

pd_df <- f_df[,p_inx]

dim(pd_df)

# plot the results
plot(results, type=c("g", "o"))

#p_df <- f_df[,results$variables[1:26,4]]



names(pd_df)

#p_df


ggpairs(pd_df)

# Retry with f_df, make model a function easy

#~~~~~~~~~~~~~~~~~Fxn~~~~~~~~~~~~~~

pd_mx <- data.matrix(pd_df)
#pd_mx <- data.matrix(f_df)
#pd_mx <- data.matrix(base_scaled_df)


t_splt <- sample(seq(1:(dim(pd_df)[[1]])), (round(dim(pd_df)[[1]] * .7)))
#t_splt <- sample(seq(1:(dim(f_df)[[1]])), (round(dim(f_df)[[1]] * .7)))
#t_splt <- sample(seq(1:(dim(base_scaled_df)[[1]])), (round(dim(base_scaled_df)[[1]] * .7)))

#t_splt

train_data <- pd_mx[t_splt,]

train_labels <- sapply(raw_df[t_splt, 3], function(x) ifelse(x == "Yes", 1, 0))


test_data <- pd_mx[-(t_splt),]

test_labels <- sapply(raw_df[-(t_splt), 3], function(x) ifelse(x == "Yes", 1, 0))


dtrain <- xgb.DMatrix(data = train_data, label = train_labels)

dtest <- xgb.DMatrix(data = test_data, label = test_labels)


positive_cases <- (sum(train_labels == 1) + sum(test_labels == 1))

negative_cases <- (sum(train_labels == 0) + sum(test_labels == 0))

print(paste("weight statistics: wpos=", positive_cases, "wneg=", negative_cases, "ratio=", negative_cases / positive_cases))


#~~~~~~~~~~~~ Normal XGBOOST

model_tuned <- xgboost(data = dtrain,            
                       max.depth = 5, # the maximum depth of each decision tree
                       nround = 200, # number of boosting rounds
                       early_stopping_rounds = 10, 
                       eval_metric = "auc",
                       objective = "binary:logistic",
                       print_every_n = 5,
                       scale_pos_weight = negative_cases/positive_cases)
#watchlist = list(train = dtrain)) # the objective function
# control for imbalanced classes

# generate predictions for our held-out testing data
pred <- predict(model_tuned, dtest)

pred

confusionMatrix(table(as.numeric(pred > 0.5), test_labels))

#as.numeric(pred > 0.5)

#mean(as.numeric(pred > 0.5))

#test_labels

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))


importance_matrix <- xgb.importance(names(pd_mx), model = model_tuned)

# and plot it!
xgb.plot.importance(importance_matrix)

#~~~~~~~~~~~~~~~~~~~~~~End Fxn~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~ HyperTuning XGBoost~~~~~~~~~~~~~~~~~~~~~~~~~~

# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/



hp_df <- pd_df
#pd_mx <- data.matrix(f_df)
#pd_mx <- data.matrix(base_scaled_df)


t_splt <- sample(seq(1:(dim(pd_df)[[1]])), (round(dim(pd_df)[[1]] * .7)))
#t_splt <- sample(seq(1:(dim(f_df)[[1]])), (round(dim(f_df)[[1]] * .7)))
#t_splt <- sample(seq(1:(dim(base_scaled_df)[[1]])), (round(dim(base_scaled_df)[[1]] * .7)))

#t_splt

hp_train_data_raw <- pd_df[t_splt,]

hp_train_labels <- sapply(raw_df[t_splt, 3], function(x) ifelse(x == "Yes", 1, 0))

hp_train_labels

hp_test_data_raw <- pd_df[t_splt,]

hp_test_labels <- sapply(raw_df[t_splt, 3], function(x) ifelse(x == "Yes", 1, 0))

hp_test_labels




hp_train_data <- as.data.frame(cbind(hp_train_data_raw, as.factor(hp_train_labels)))

names(hp_train_data_raw)



hp_test_data <- as.data.frame(cbind(hp_test_data_raw, as.factor(hp_test_labels)))

names(hp_test_data)




names(train_data)

cbind(train_data, train_labels)

names(hp_train_data)

names_to_fix <- c("JobRoleSalesRepresentative", 
                  "JobRole_ResearchDirector", 
                  "JobRole_ManufacturingDirector", 
                  "JobRoleSalesRepresentative", 
                  "EducationField_HumanResources", 
                  "train_labels")

cols_to_fix <- c(6, 15, 17, 20, 22, 24)

j <- 0

for (i in length(cols_to_fix)) {
  
  names(hp_train_data)[[i]] <- c(names_to_fix[[j]]) 
  names(hp_test_data)[[i]] <- c(names_to_fix[[j]])
  
    j <- j + 1
  
}


#names(hp_train_data)[[6]] <- c("JobRoleSalesRepresentative")
#
#names(hp_train_data)[[15]] <- c("JobRole_ResearchDirector")
#
#names(hp_train_data)[[17]] <- c("JobRole_ManufacturingDirector")
#
#names(hp_train_data)[[20]] <- c("JobRole_SalesExecutive")
#
#names(hp_train_data)[[22]] <- c("EducationField_HumanResources")
#
#names(hp_train_data)[[24]] <- c("train_labels")
#
#names(hp_test_data)[[6]] <- c("JobRoleSalesRepresentative")
#
#names(hp_test_data)[[15]] <- c("JobRole_ResearchDirector")
#
#names(hp_test_data)[[17]] <- c("JobRole_ManufacturingDirector")
#
#names(hp_test_data)[[20]] <- c("JobRole_SalesExecutive")
#
#names(hp_test_data)[[22]] <- c("EducationField_HumanResources")
#
#names(hp_test_data)[[24]] <- c("test_labels")

dim(hp_train_data)

traintask <- makeClassifTask (data = hp_train_data, target = "train_labels")


testtask <- makeClassifTask (data = hp_test_data, target = "test_labels")


makeTask 


#create learner
lrn <- makeLearner("classif.xgboost", predict.type = "response")

lrn$par.vals <- list( objective="binary:logistic", 
                      eval_metric="error", 
                      nrounds=100L, 
                      eta=0.1)

#set parameter space
params <- makeParamSet(makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                       makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                       makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",
                          stratify = T,
                          iters=10L)



#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc,
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)
mytune$y

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)

##~~~~~~~~~~~~ CV XGBOOST
#
#model_tuned_cv <- xgb.cv(data = dtrain, # the data           
#                       max.depth = 5, # the maximum depth of each decision tree
#                       nround = 200, # number of boosting rounds
#                       early_stopping_rounds = 10, 
#                       eval_metric = "auc",# if we dont see an improvement in this many rounds, stop
#                       objective = "binary:logistic",
#                       print_every_n = 5,
#                       scale_pos_weight = negative_cases/positive_cases)
##watchlist = list(train = dtrain)) # the objective function
#
#
#params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, max_depth=5)
#
#
## control for imbalanced classes
#cv <- xgb.cv(params = params, data = dtrain, nrounds = 200, nfold = 5, metrics = list("rmse","auc"), 
#             showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
#
#
##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
#
#
#print(cv)
#print(cv, verbose=TRUE)
#
## generate predictions for our held-out testing data
#pred_cv <- predict(cv, dtest)
#
#pred_cv
#
#confusionMatrix(table(as.numeric(pred_cv > 0.5), test_labels))
#
##as.numeric(pred > 0.5)
#
##mean(as.numeric(pred > 0.5))
#
##test_labels
#
## get & print the classification error
#err <- mean(as.numeric(pred_cv > 0.5) != test_labels)
#print(paste("test-error=", err))
#
#
#importance_matrix_cv <- xgb.importance(names(pd_mx), model = model_tuned_cv)
#
## and plot it!
#xgb.plot.importance(importance_matrix_cv)
#
#~~~~~~~~~~~~~~~~~~~~~~End Fxn~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



