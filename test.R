#these could be edits


# https://devblogs.microsoft.com/premier-developer/exploring-feature-weights-using-r-and-azure-machine-learning-studio/
# https://rpubs.com/bpr1989/HRAnalysis
# https://rstudio-pubs-static.s3.amazonaws.com/397345_30101161deeb4def9bf3570e8899d859.html
# https://www.kaggle.com/vsdwivedi/a-detailed-study-on-employee-attrition
# https://rpubs.com/Himeshme/Attrition
# https://rpubs.com/CJ_09/Emp_Attrition_Final

#install.packages("tidyverse")
#install.packages("naniar")
#install.packages("caret")
#install.packages('e1071', dependencies = TRUE)
#install.packages("mlbench")
#install.packages("fastDummies")
#install.packages("xgboost")
#install.packages("clustMixType")
#install.packages("mlr")
#install.packages("gridExtra")
#install.packages("corrplot")
install.packages("reshape2")

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
library(gridExtra)
library(forcats)
library(corrplot)
library(reshape2)



#Read the raw csv with attrition and salaries

raw_df <- read.csv("attritApp/data/CaseStudy2-data.csv")
#raw_df

# Employees w. Attrition

attr_df <- raw_df %>% filter(Attrition == "Yes")


# Employees w.o Atrrition

nttr_df <- raw_df %>% filter(Attrition == "No")



nttr_df %>% group_by(Department)

#summary(raw_df)

#summary(attr_df)

#summary(nttr_df)

#Check for missing vals


gg_miss_var(raw_df)

#No missing vals

names(raw_df)
str(raw_df)

raw_df %>% ggplot(aes(x = MonthlyIncome, y = ..count..)) + geom_histogram(bins = 50)

## divide by 1000
#
#maxMonthlyIncome <- max(f_df$MonthlyIncome)
#
#minMonthlyIncome <- min(f_df$MonthlyIncome)
#
#MonthlyIncome_rng <- maxMonthlyIncome - minMonthlyIncome
#
#maxMonthlyIncome
#
#minMonthlyIncome
#
#MonthlyIncome_rng
#
#
## Divide by 100
#
#maxDailyRate <- max(f_df$DailyRate)
#
#minDailyRate <- min(f_df$DailyRate)
#
#DailyRate_rng <- maxDailyRate - minDailyRate
#
#maxDailyRate
#
#minDailyRate
#
#DailyRate_rng
#
#
#
## divide by 1000
#
#
#maxMonthlyRate <- max(f_df$MonthlyRate)
#
#minMonthlyRate <- min(f_df$MonthlyRate)
#
#MonthlyRate_rng <- maxMonthlyRate - minMonthlyRate
#
#maxMonthlyRate
#
#minMonthlyRate
#
#MonthlyRate_rng
#
#
#maxPercentSalaryHike <- max(raw_df$PercentSalaryHike)
#
#minPercentSalaryHike <- min(raw_df$PercentSalaryHike)
#
#PercentSalaryHike_rng <- maxPercentSalaryHike - minPercentSalaryHike
#
#maxPercentSalaryHike
#
#minPercentSalaryHike
#
#PercentSalaryHike_rng
#
##raw_df$WorkLifeBalance
#
##Compare worklifebalance to monthly income
#
#raw_df
#
#names(raw_df)
#
#
#all_col_names <- c("ID", "Age", "Attrition", "BusinessTravel", "DailyRate", "Department", 
#                   "DistanceFromHome", "Education", "EducationField", "EmployeeCount", 
#                   "EmployeeNumber", "EnvironmentSatisfaction", "Gender", "HourlyRate", 
#                   "JobInvolvement" , "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus",
#                   "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", "Over18", "PercentSalaryHike", 
#                   "PerformanceRating", "RelationshipSatisfaction", "StandardHours", "StockOptionLevel", 
#                   "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany", 
#                   "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")    
#
#
#colnm_len <- 1:length(all_col_names)
#
#
#col_nm_df <- as.data.frame(x = all_col_names)
#
#
#cont_var_inx <- c(2, 3, 5, 6, 7, 13, 14, 20, 21, 22, 24, 25, 27, 29, 30, 32, 34, 35)
#
#ord_var_inx <- c(3, 4, 6, 8, 9, 12, 13, 15, 16, 18, 19, 25, 26, 28, 30, 31)


raw_df %>% ggplot(aes(x=WorkLifeBalance, y=MonthlyIncome)) + geom_jitter()

raw_df %>% ggplot(aes(x=WorkLifeBalance, y=JobSatisfaction)) + geom_jitter()

raw_df %>% ggplot(aes(x=EnvironmentSatisfaction, y=JobSatisfaction)) + geom_jitter()

raw_df %>% ggplot(aes(x=JobSatisfaction, y=JobLevel)) + geom_jitter()

raw_df[,ord_var_inx]


raw_df %>% ggplot(aes(x=reorder(JobLevel, MonthlyIncome), y=MonthlyIncome)) + 
                  geom_boxplot(aes(fill=Attrition)) 


raw_df %>% ggplot(aes(x=reorder(JobRole, MonthlyIncome), y=MonthlyIncome)) + 
                  geom_boxplot(aes(fill=Attrition)) 


raw_df %>% ggplot(aes(x=YearsAtCompany, y=YearsInCurrentRole)) + geom_point()

raw_df %>% ggplot(aes(x=JobRole, y=TrainingTimesLastYear, color = as.factor(JobLevel))) + 
                  geom_jitter()


roleMeans <- aggregate(raw_df[, "JobLevel"], list(raw_df$JobRole), mean) %>% arrange(x)

roleMeans

typeof(roleMeans[[1]])

dept_list <- as.character(unique(raw_df$Department))


ord <- as.character(roleMeans[[1]])
#c("Sales Representative",
#       "Research Scientist",
#          "Human Resources",
#    "Laboratory Technician",
#          "Sales Executive",
#   "Manufacturing Director",
#"Healthcare Representative",
#        "Research Director",
#                  "Manager")



ord2 <- c("Sales Representative",
          "Sales Executive",
       "Research Scientist",
    "Laboratory Technician",
"Healthcare Representative",
   "Manufacturing Director",
        "Research Director",
          "Human Resources",
                  "Manager")

raw_df %>% ggplot(aes(x=JobRole, y=JobLevel, color = Department)) + 
                  geom_jitter() + scale_x_discrete(limits = ord2) +
                  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 7))


raw_df %>% ggplot(aes(x=JobRole, y=JobLevel, color = Attrition)) + 
                  geom_jitter() + scale_x_discrete(limits = ord) +
                  theme(axis.text.x = element_text(angle=45, hjust = 1))


attr_df %>% ggplot(aes(x=JobRole, y=JobLevel, color = Department)) + 
                  geom_jitter() + scale_x_discrete(limits = ord) +
                  theme(axis.text.x = element_text(angle=45, hjust = 1))


nttr_df %>% ggplot(aes(x=JobRole, y=JobLevel, color = Department)) + 
                  geom_jitter() + scale_x_discrete(limits = ord) +
                  theme(axis.text.x = element_text(angle=45, hjust = 1))


df_count  <-  raw_df %>% count(JobRole, JobLevel) %>% mutate(prop = round((df_count$n/sum(df_count$n))*100, 2))


attr_count  <-  attr_df %>% count(JobRole, JobLevel, Department) 

attr_count  <- attr_count %>% mutate(prop = round((attr_count$n/sum(attr_count$n))*100, 2))

attr_count_labels <- attr_count

attr_count_labels[9,] <- c("Manager", 5, "Research & Development", 3, 2.14)

attr_count_labels[10,] <- c("Manager", 5, "Sales", 3, 2.14)


nttr_count  <-  nttr_df %>% count(JobRole, JobLevel, Department) %>% mutate(prop = round((nttr_count$n/sum(nttr_count$n))*100, 2))

nttr_count <- nttr_count %>% mutate(prop = round((nttr_count$n/sum(nttr_count$n))*100, 2))

attr_count_labels[12,] <- c("Manager", 4, "Research & Development", 24, 3.03)

attr_count_labels[13,] <- c("Manager", 5, "Sales", 3, 2.14)

attr_count_labels[9,] <- c("Manager", 5, "Research & Development", 3, 2.14)




raw_df %>% 
  ggplot(aes(x=JobRole, y=JobLevel)) +
  geom_count() + scale_x_discrete(limits = ord) +
  geom_text(data = df_count, 
    aes(df_count$JobRole, 
        df_count$JobLevel, 
        label = df_count$n), 
    size = 3,
    nudge_y = 0.1,
    color = 'red') +
  geom_text(data = df_count, 
    aes(df_count$JobRole, 
        df_count$JobLevel, 
        label = paste(df_count$prop, "%")), 
    size = 3,
    nudge_y = -0.1,
    color = 'blue') +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


attr_df %>% 
  ggplot(aes(x=JobRole, y=JobLevel)) +
  geom_count(na.rm = TRUE) + scale_x_discrete(limits = ord) +
  geom_text(data = attr_count, 
     aes(attr_count$JobRole, 
       attr_count$JobLevel, 
       label = attr_count_labels$n), 
     size = 3.5,
     nudge_y = 0.1,
     color = 'red') +
  geom_text(data = attr_count, 
     aes(attr_count$JobRole, 
       attr_count$JobLevel, 
       label = paste(attr_count_labels$prop, "%")), 
     size = 3.5,
     nudge_y = -0.1,
     color = 'red') +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(title="Attrition", xlab="JobRole", ylab="JobLevel")

nttr_df %>% 
  ggplot(aes(x=JobRole, y=JobLevel)) +
  geom_count(na.rm = TRUE) + scale_x_discrete(limits = ord) +
  geom_text(data = nttr_count, 
    aes(nttr_count$JobRole, 
        nttr_count$JobLevel, 
       label = nttr_count$n), 
    size = 3,
    nudge_y = 0.1,
    color = 'red') +
  geom_text(data = nttr_count, 
    aes(nttr_count$JobRole, 
        nttr_count$JobLevel, 
        label = paste(nttr_count$prop, "%")), 
    size = 3,
    nudge_y = -0.1,
    color = 'blue') +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


attr_jl1_tot <- attr_df %>% filter(JobLevel == 1) %>% count()

attr_jl1_ot <-attr_df %>% filter(JobLevel == 1 & OverTime == "Yes") %>% count()

attr_ot_prop <- (attr_jl1_ot/attr_jl1_tot) * 100 


nttr_jl1_tot <- nttr_df %>% filter(JobLevel == 1) %>% count()

nttr_jl1_ot <- nttr_df %>% filter(JobLevel == 1 & OverTime == "Yes") %>% count()

nttr_ot_prop <- (nttr_jl1_ot/nttr_jl1_tot) * 100 

all_tot <- raw_df %>% count()

all_ot <- raw_df %>% filter(JobLevel != 1 & OverTime == "Yes") %>% count()

all_ot_prop <- (all_ot/all_tot) * 100

#se_tot <- nttr_df %>% filter(JobRole == "Sales Executive" & JobLevel == 2) %>% count()
#
#se_ot <- nttr_df %>% filter(JobRole == "Sales Executive" & OverTime == "Yes" & JobLevel == 2) %>% count()
#
#se_ot_prop <- (se_ot/se_tot) * 100 
#
#
#
#se2_tot <- nttr_df %>% filter(JobRole == "Sales Executive") %>% count()
#
#se2_ot <- nttr_df %>% filter(JobRole == "Sales Executive" & OverTime == "Yes") %>% count()
#
#se2_ot_prop <- (se2_ot/se2_tot) * 100 





which(nttr_count$n == max(nttr_count$n))



#raw_df %>% ggplot(aes(x=JobRole, y=JobLevel)) + 
#                  geom_contour(aes(z = raw_df$MonthlyIncome)) + scale_x_discrete(limits = ord)







attr_df %>% group_by(Department) %>% summarise(JobRol = n())

aggregate(attr_df$JobLevel, by=list(Role = attr_df$JobRole), FUN = sum)



sum(attr_count %>% arrange(desc(n)) %>% head(n=10) %>% select(n))

attr_rs1 <- attr_df %>% filter(JobRole == "Research Scientist" & JobLevel == 1)

mean(attr_rs1$YearsAtCompany)

mean(attr_rs1$YearsInCurrentRole)

mean(attr_rs1$YearsSinceLastPromotion)

mean(attr_rs1$TotalWorkingYears)

nttr_rs1 <- nttr_df %>% filter(JobRole == "Research Scientist" & JobLevel == 1)

mean(nttr_rs1$YearsAtCompany)

mean(nttr_rs1$YearsInCurrentRole)

mean(nttr_rs1$YearsSinceLastPromotion)

mean(nttr_rs1$TotalWorkingYears)


dim(attr_rs1)

dim(nttr_rs1)

attr_rs1_ot <- attr_rs1 %>% filter(OverTime == "Yes")

nttr_rs1_ot <- nttr_rs1 %>% filter(OverTime == "Yes")

str(attr_rs1_ot)

summary(attr_rs1_ot)

summary(nttr_rs1_ot)

dim(nttr_rs1 %>% filter(OverTime == "Yes"))

summary(attr_df)

summary(nttr_df)

ynFlip <-  factor(raw_df$Attrition,levels(raw_df$Attrition)[c(2, 1)])

raw_df %>% ggplot(aes(Attrition, Age, fill= ynFlip)) + geom_boxplot()

raw_df %>% ggplot(aes(Attrition, MonthlyIncome, fill= ynFlip)) + geom_boxplot()



raw_df %>% ggplot(aes(BusinessTravel, fill=ynFlip)) + geom_bar()


raw_df %>% 
  ggplot(aes(Department,fill = ynFlip)) + 
  geom_bar() + 
  scale_x_discrete(limits = c("Human Resources", "Sales", "Research & Development")) +
  geom_text(aes(x = "Sales", y = 200, label = "Hi\nDude!"))


raw_df %>% 
  ggplot(aes(x=EducationField, y=Education, color = Department)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 7))





jlOrg <-  factor(attr_df$JobLevel,levels(as.factor(attr_df$JobLevel))[c(5:1)])

raw_df %>% ggplot(aes(x=JobRole, fill = jlOrg)) +
  geom_bar() + scale_x_discrete(limits = ord2) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#attr_df %>% ggplot(aes(x=JobRole, fill = jlOrg)) +
#  geom_bar() + scale_x_discrete(limits = ord2) +
#  theme(axis.text.x = element_text(angle=45, hjust = 1))


raw_df %>% 
  ggplot(aes(Gender,fill=ynFlip))+
  geom_bar()



raw_df %>% ggplot(aes(MaritalStatus, fill=ynFlip)) + geom_bar() + scale_x_discrete(limits = c("Single", "Married", "Divorced"))



raw_df %>% ggplot(aes(NumCompaniesWorked,fill=ynFlip))+geom_bar()


raw_df %>% ggplot(aes(OverTime,fill = ynFlip)) + geom_bar()


raw_df %>% ggplot(aes(PerformanceRating,fill = ynFlip))+geom_bar()

raw_df %>% ggplot(aes(StockOptionLevel,fill = ynFlip))+geom_bar()


raw_df %>% ggplot(aes(TotalWorkingYears,fill = ynFlip))+geom_bar()


raw_df %>% ggplot(aes(YearsAtCompany,fill = ynFlip)) + geom_bar()
raw_df %>% ggplot(aes(YearsInCurrentRole,fill = ynFlip)) + geom_bar()
raw_df %>% ggplot(aes(YearsSinceLastPromotion,fill = ynFlip)) + geom_bar()
raw_df %>% ggplot(aes(YearsWithCurrManager,fill = ynFlip)) + geom_bar()









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


preScl_f_df <- f_df







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#dat <- data.frame(x = rnorm(10, 30, .2), y = runif(10, 3, 5))
#scaled.dat <- scale(dat)
#
## check that we get mean of 0 and sd of 1
#colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
#apply(scaled.dat, 2, sd)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(dplyr)

#set.seed(1234)
#dat <- data.frame(x = rnorm(10, 30, .2), 
#                  y = runif(10, 3, 5),
#                  z = runif(10, 10, 20))
#dat


# Plug names in here


#colr <- c("DailyRate","MonthlyRate", "MonthlyIncome", "HourlyRate")

cola <- c("DailyRate", "DistanceFromHome", "HourlyRate", "MonthlyIncome", "MonthlyRate",      
          "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsInCurrentRole", 
          "YearsSinceLastPromotion", "YearsWithCurrManager")

#base_scaled_df <- f_df %>% mutate_at(vars(colr), list(~scale(.) %>% as.vector))

#base_scaled_df

all_scaled_df <- f_df %>% mutate_at(vars(cola), list(~scale(.) %>% as.vector))

all_scaled_df


#Scale additional features

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#EDIT 1 (2016): Addressed Julian's comment: the output of scale is Nx1 matrix so ideally we should add an as.vector to convert the matrix type back into a vector type. Thanks Julian!

#EDIT 2 (2019): Quoting Duccio A.'s comment: For the latest dplyr (version 0.8) you need to change dplyr::funcs with list, like dat %>% mutate_each_(list(~scale(.) %>% as.vector), vars=c("y","z"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Opt for function below

#zVar <- (myVar - mean(myVar)) / sd(myVar)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
#
##x<-rnorm(10,14,2)
##y<-rnorm(10,7,3)
##z<-rnorm(10,18,5)
##df<-data.frame(x,y,z)
#
#normFun_df <- f_df %>% mutate_at(vars(colm), list(~normFunc(.) %>% as.vector))
#
#normFun_df

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(caret)
# Assuming goal class is column 10
#preObj <- preProcess(f_df, method=c("center", "scale"))


#newData <- predict(preObj, data[, -10])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#df.scaled <- as.data.frame(scale(df))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data.Normalization (x,type="n0",normalization="column")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#fs_df <- dummyVars(" ~ .", data = fs_df)

#fs_df

#f_df <- data.frame(predict(fs_df, newdata = fs_df))

#str(f_df)

#gg_miss_var(f_df)


#nums <- sapply(f_df, is.numeric)
#data.numeric <- f_df[ , nums]

#data.without_na <- na.omit(data.numeric)
#cor_matrix <- cor(data.without_na)


#~~~~~~~~~~~~~~~~~~~~~~~Remove Redundant Features + Corrplot

correlationMatrix <- cor(all_scaled_df, use="pairwise.complete.obs")

correlationMatrix

correlationMatrix[lower.tri(correlationMatrix, diag = TRUE)] <- NA          # lower tri and diag set to NA
corr_df <- as.data.frame(subset(melt(correlationMatrix, na.rm = TRUE), value > .45))

corr_df %>% filter(Var1 == "MonthlyIncome")

# JobLevel 0.9516400
# TotalWorkingYears 0.7785112


corrplot(correlationMatrix, tl.cex = 0.5)

#gg_miss_var(correlationMatrix)

gg_miss_var(as.data.frame(correlationMatrix))

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)#, names = TRUE)

#correlationMatrix[highlyCorrelated,]

print(highlyCorrelated)

print(all_scaled_df[,highlyCorrelated])

print(all_scaled_df[,-(highlyCorrelated)])

f_df <- all_scaled_df[,-(highlyCorrelated)]

preScl_f_df <- preScl_f_df[,-(highlyCorrelated)]


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


#label <- as.numeric(dtrain[[33]])

#data <- as.matrix(dtrain[2:31])

#weight <- as.numeric(dtrain[[32]]) * testsize / length(label)

#sumwpos <- sum(weight * (label==1.0))

#sumwneg <- sum(weight * (label==0.0))



#sum(test_labels == 1)/length(test_labels)



test_data <- pd_mx[-(t_splt),]

test_labels <- sapply(raw_df[-(t_splt), 3], function(x) ifelse(x == "Yes", 1, 0))

test_labels



dtrain <- xgb.DMatrix(data = train_data, label = train_labels)


dtest <- xgb.DMatrix(data = test_data, label = test_labels)

#model_tuned <- xgboost(data = dtrain, # the data           
#                       max.depth = 5, # the maximum depth of each decision tree
#                       nround = 200, # number of boosting rounds
#                       early_stopping_rounds = 10, 
#                       eval_metric = "auc",# if we dont see an improvement in this many rounds, stop
#                       objective = "binary:logistic") # the objective function
#                       #scale_pos_weight = negative_cases/postive_cases) # control for imbalanced classes


negative_cases <- pd_mx %>% filter()




positive_cases <- (sum(train_labels == 1) + sum(test_labels == 1))

negative_cases <- (sum(train_labels == 0) + sum(test_labels == 0))

print(paste("weight statistics: wpos=", positive_cases, "wneg=", negative_cases, "ratio=", negative_cases / positive_cases))


#/(length(train_labels) + length(test_labels))











#~~~~~~~~~~~~ Normal XGBOOST

model_tuned <- xgboost(data = dtrain, # the data           
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

names(hp_train_data)[[6]] <- c("JobRoleSalesRepresentative")

names(hp_train_data)[[15]] <- c("JobRole_ResearchDirector")

names(hp_train_data)[[17]] <- c("JobRole_ManufacturingDirector")

names(hp_train_data)[[20]] <- c("JobRole_SalesExecutive")

names(hp_train_data)[[22]] <- c("EducationField_HumanResources")

names(hp_train_data)[[24]] <- c("train_labels")

names(hp_test_data)[[6]] <- c("JobRoleSalesRepresentative")

names(hp_test_data)[[15]] <- c("JobRole_ResearchDirector")

names(hp_test_data)[[17]] <- c("JobRole_ManufacturingDirector")

names(hp_test_data)[[20]] <- c("JobRole_SalesExecutive")

names(hp_test_data)[[22]] <- c("EducationField_HumanResources")

names(hp_test_data)[[24]] <- c("test_labels")

dim(hp_train_data)

traintask <- makeClassifTask (data = hp_train_data, target = "train_labels")


testtask <- makeClassifTask (data = hp_test_data, target = "test_labels")


#makeTask 


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

##set parallel backend
#library(parallel)
#library(parallelMap) 
#parallelStartSocket(cpus = detectCores())
 
#parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc,
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)
#mytune$y


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



#~~~~~~~~~~~~~~HYPERTUNING knn.cv, something funny about model, only got 84%, but sensitivity is jacked up, need to 
#~~~~~~~~~~~~Pull tuning loop out to cycle xgboost


#~~~~~~~~~~~~~~~~~~~~~~~~pd_df


set.seed(22)
iterations = 500
numks = 20
splitPerc = .7
masterAcc = matrix(nrow = iterations, ncol = numks)
masterSens = matrix(nrow = iterations, ncol = numks)
masterSpec = matrix(nrow = iterations, ncol = numks)
for(j in 1:iterations)
{
  k_inx = sample(1:dim(pd_df)[1],round(splitPerc * dim(pd_df)[1]))
  
  k_trn_data <- pd_df[k_inx,]
  
  k_tst_data <- pd_df[-k_inx,]
  
  #cl = raw_df[k_inx, 3]
  
  for(i in 1:numks)
  {
    classifications = knn(k_trn_data, 
                          k_tst_data, 
                          raw_df[k_inx, 3], 
                          prob = TRUE, k = i)
    table(classifications,raw_df[-k_inx, 3])
    CM = confusionMatrix(table(classifications, raw_df[-k_inx, 3]))
    masterAcc[j,i] = CM$overall[1]
    masterSens[j,i] = CM$byClass[1]
    masterSpec[j,i] = CM$byClass[2]
  }
  
}
MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l", main = "Overall Accuracy", xlab = "Number of k(s)", ylab = "Mean Accuracy")
which.max(MeanAcc)
max(MeanAcc)

MeanSens = colMeans(masterSens)
plot(seq(1,numks,1),MeanSens, type = "l", main = "Overall Sensitivy", xlab = "Number of k(s)", ylab = "Mean Sensitivy")
which.max(MeanSens)
max(MeanSens)

MeanSpec = colMeans(masterSpec)
plot(seq(1,numks,1),MeanSpec, type = "l", main = "Overall Specificity", xlab = "Number of k(s)", ylab = "Mean Specificity")
which.max(MeanSpec)
max(MeanSpec)



##~~~~~~~~~~~~~~~~~~f_df
#
#set.seed(22)
#iterations = 500
#numks = 20
#splitPerc = .7
#masterAcc = matrix(nrow = iterations, ncol = numks)
#masterSens = matrix(nrow = iterations, ncol = numks)
#masterSpec = matrix(nrow = iterations, ncol = numks)
#for(j in 1:iterations)
#{
#  k_inx = sample(1:dim(f_df)[1],round(splitPerc * dim(f_df)[1]))
#  
#  k_trn_data <- f_df[k_inx,]
#  
#  k_tst_data <- f_df[-k_inx,]
#  
#  #cl = raw_df[k_inx, 3]
#  
#  for(i in 1:numks)
#  {
#    classifications = knn(k_trn_data, 
#                          k_tst_data, 
#                          raw_df[k_inx, 3], 
#                             prob = TRUE, k = i)
#    table(classifications,raw_df[-k_inx, 3])
#    CM = confusionMatrix(table(classifications, raw_df[-k_inx, 3]))
#    masterAcc[j,i] = CM$overall[1]
#    masterSens[j,i] = CM$byClass[1]
#    masterSpec[j,i] = CM$byClass[2]
#  }
#  
#}
#MeanAcc = colMeans(masterAcc)
#plot(seq(1,numks,1),MeanAcc, type = "l")
#which.max(MeanAcc)
#max(MeanAcc)
#
#MeanSens = colMeans(masterSens)
#plot(seq(1,numks,1),MeanSens, type = "l")
#which.max(MeanSens)
#max(MeanSens)
#
#MeanSpec = colMeans(masterSpec)
#plot(seq(1,numks,1),MeanSpec, type = "l")
#which.max(MeanSpec)
#max(MeanSpec)
#
#
##~~~~~~~~~~~~~~~~~~~~~~~~preScl_f_df
#
#
#set.seed(22)
#iterations = 500
#numks = 20
#splitPerc = .7
#masterAcc = matrix(nrow = iterations, ncol = numks)
#masterSens = matrix(nrow = iterations, ncol = numks)
#masterSpec = matrix(nrow = iterations, ncol = numks)
#for(j in 1:iterations)
#{
#  k_inx = sample(1:dim(preScl_f_df)[1],round(splitPerc * dim(preScl_f_df)[1]))
#  
#  k_trn_data <- preScl_f_df[k_inx,]
#  
#  k_tst_data <- preScl_f_df[-k_inx,]
#  
#  #cl = raw_df[k_inx, 3]
#  
#  for(i in 1:numks)
#  {
#    classifications = knn(k_trn_data, 
#                          k_tst_data, 
#                          raw_df[k_inx, 3], 
#                          prob = TRUE, k = i)
#    table(classifications,raw_df[-k_inx, 3])
#    CM = confusionMatrix(table(classifications, raw_df[-k_inx, 3]))
#    masterAcc[j,i] = CM$overall[1]
#    masterSens[j,i] = CM$byClass[1]
#    masterSpec[j,i] = CM$byClass[2]
#  }
#  
#}
#MeanAcc = colMeans(masterAcc)
#plot(seq(1,numks,1),MeanAcc, type = "l")
#which.max(MeanAcc)
#max(MeanAcc)
#
#MeanSens = colMeans(masterSens)
#plot(seq(1,numks,1),MeanSens, type = "l")
#which.max(MeanSens)
#max(MeanSens)
#
#MeanSpec = colMeans(masterSpec)
#plot(seq(1,numks,1),MeanSpec, type = "l")
#which.max(MeanSpec)
#max(MeanSpec)


raw_df

#proto_df <- cbind(raw)

# apply k prototyps

kpres <- kproto(fs_df, 5)

summary(kpres)

protoPrdct <- predict(kpres, fs_df)

protoPrdct

# Check for the optimal number of clusters given the data

mydata <- fs_df
wss<-vector()
for (i in 2:15){ wss[i] <- sum(kproto(fs_df, i)$withinss)}
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

## plots between Total and other numerical Attributes with clusters: 
#par(mfrow=c(1,2))
#
#for(i in 1: 1:6){
#  plot(pokemon[,c(5,5+i)], col=pokemon$cluster, main="K-prototypes")
#}


library(xgboost)
library(plyr)
library(DMwR)

fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 20,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)


HRAXGBmodel <- train(Attrition~., data = trainHRA,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)

HRAXGBprd <- predict(HRAXGBmodel,testHRA)
confusionMatrix(HRAXGBprd, testHRA$Attrition)
XGB.plot <- plot.roc (as.numeric(testHRA$Attrition), as.numeric(HRAXGBprd),lwd=2, type="b", print.auc=TRUE,col ="blue")


#~~~~~~~~~~~~~~~~~~~~~~~~~LINEAR

inx = sample(1:dim(f_df)[1],round(splitPerc * dim(f_df)[1]))
lm_train_data = f_df[inx,]
lm_test_data = f_df[-inx,]

# Scatter plot to inspect general trend
cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("Weight vs MPG")

# Use lm to create a linear regression model
fit <- lm(MPG~Weight, data=cars)

# Scatter plot with LR model overlay
cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("LR Model: Weight vs MPG") + geom_smooth(method = "lm")

# The long way to calculate the p-value
# Pull out intercept and slope values for later
beta_0_hat <- fit$coefficients[1]
beta_1_hat <- fit$coefficients[2]

# Pull out SE for intercept and slope
SE_beta_0_hat <- summary(fit)$coefficients[1,2]
SE_beta_1_hat <- summary(fit)$coefficients[2,2]

#Intercept
tstat <- beta_0_hat/SE_beta_0_hat #beta_0_hat / SE(beta_0_hat)
pvalue <- (1-pt(tstat,length(cars$MPG)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

#Slope
tstat <- beta_1_hat/SE_beta_1_hat #beta_1_hat / SE(beta_1_hat)
pvalue <- (pt(tstat,length(cars)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

# The easy way to get the p-values and confidence intervals
summary(fit)
confint(fit)

#lm_splt <- sample(seq(1:(dim(pd_df)[[1]])), (round(dim(pd_df)[[1]] * .7)))

#lm_train_data <- hp_train_data
#
#names(lm_train_data)[24] <- "Attrition"
#
#lm_test_data <-  hp_test_data
#
#names(lm_test_data)[24] <- "Attrition"

# https://github.com/ashwinbaldawa/Employee-Attrition-Prediction/blob/master/Employee%20Attrition.R

cls <- raw_df[t_splt, 3]

#lm_model <- glm(train_labels ~ OverTime_No + MonthlyIncome + StockOptionLevel + Age + 
#                 JobInvolvement + JobRoleSalesRepresentative + MaritalStatus_Single + 
#                 YearsWithCurrManager + MaritalStatus_Divorced + YearsInCurrentRole + 
#                 JobSatisfaction + WorkLifeBalance + EnvironmentSatisfaction + NumCompaniesWorked + 
#                 JobRole_ResearchDirector + EducationField_Marketing + JobRole_ManufacturingDirector + 
#                 PercentSalaryHike + YearsSinceLastPromotion + JobRole_SalesExecutive + EducationField_Medical + 
#                 EducationField_HumanResources + RelationshipSatisfaction, family = "binomial", data=lm_train_data)

lm_model <- lm(train_labels ~ ., data=lm_train_data)

lm_predict <- predict(lm_model, lm_test_data)

confusionMatrix(as.factor(as.numeric(lm_predict > 0.5)), as.factor(lm_test_data$Attrition))

#table(as.numeric(pred > 0.5)

summary(lm_model)

#train_labels ~ OverTime_No + MonthlyIncome + StockOptionLevel + Age + JobInvolvement + JobRoleSalesRepresentative + MaritalStatus_Single + YearsWithCurrManager + MaritalStatus_Divorced + YearsInCurrentRole + JobSatisfaction + WorkLifeBalance + EnvironmentSatisfaction + NumCompaniesWorked + JobRole_ResearchDirector + EducationField_Marketing + JobRole_ManufacturingDirector + PercentSalaryHike + YearsSinceLastPromotion + JobRole_SalesExecutive + EducationField_Medical + EducationField_HumanResources + RelationshipSatisfaction 
#> 


# Scatter plot to inspect general trend
#cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("Weight vs MPG")

# Use lm to create a linear regression model
fit <- lm(MonthlyIncome~JobLevel + TotalWorkingYears, data=f_df)

# Scatter plot with LR model overlay
#cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("LR Model: Weight vs MPG") + geom_smooth(method = "lm")

# The long way to calculate the p-value
# Pull out intercept and slope values for later
beta_0_hat <- fit$coefficients[1]
beta_1_hat <- fit$coefficients[2]

# Pull out SE for intercept and slope
SE_beta_0_hat <- summary(fit)$coefficients[1,2]
SE_beta_1_hat <- summary(fit)$coefficients[2,2]

#Intercept
tstat <- beta_0_hat/SE_beta_0_hat #beta_0_hat / SE(beta_0_hat)
pvalue <- (1-pt(tstat,length(f_df$MonthlyIncome)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

#Slope
tstat <- beta_1_hat/SE_beta_1_hat #beta_1_hat / SE(beta_1_hat)
pvalue <- (pt(tstat,length(f_df$MonthlyIncome)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

# The easy way to get the p-values and confidence intervals
summary(fit)
confint(fit)


pred_error_sq <- c(0)

for(i in 1:dim(f_df)[1]) {
  
  inc_train <- f_df[-i,]
  
  fit <- lm(MonthlyIncome ~ JobLevel + TotalWorkingYears, data = inc_train)
  
  mi_pred <- predict(fit, data.frame(JobLevel = f_df[i,8], TotalWorkingYears = f_df[i,17])) 
  
  pred_error_sq <- pred_error_sq + (f_df[i,10] - mi_pred)^2 
}

SSE = var(f_df$MonthlyIncome) * (869)

R_squared <- 1 - (pred_error_sq/SSE) 
R_squared

RMSE = sqrt(pred_error_sq/870)
RMSE







































#writing final submission file
#submission_select <- data.frame(PassengerId = test$PassengerId, Survived = testClean$Select)
#write.csv(submission_select, file = 'Titanic_select.csv', row.names = F)