# 1) Run random forest classification model to predict cluster membership or each of our sites
# 2) Use random forest model to investigate relative variable importance for determining cluster membership

# https://www.youtube.com/watch?v=dJclNIN-TPo
library(randomForest)
library(tidyverse)
library(tidymodels)

# Bring df in and format
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_tune_df_2.rds")

# Change variable names to be more clear and intuitive
names(dat)
dat <- dat %>%
  rename(Soil_Salinity = Salinity, Soil_pH = pH, Soil_Thickness = Thickness, Soil_OM_Content = OM_Content)

# Remove cluster results for clustering ran on non-flow normalized specific conductance data
dat <- dat %>%
  select(-c("Cluster_SC"))
dat <- dat %>%
  rename(Cluster = Cluster_SCQ)

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

set.seed(123)
dat %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n)) # ~73/27

# Split data into testing vs training datasets
set.seed(123)
splits <- initial_split(dat, strata = Cluster)
train <- training(splits)
test  <- testing(splits)

# Check that we kept the distribution of clusters
train %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n))
test  %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n))
# both still ~72/28

# run the model with default parameters
set.seed(222)
rf <- randomForest(Cluster ~., data = train) # assumes classification if dependent variable is a factor
print(rf)
# attribute_tune_df.rds OOB = 28.11%
# attribute_tune_df_1.rds OOB = 27.03%
# attribute_tune_df_2.rds OOB = 26.49%
# attribute_tune_df_3.rds OOB = 27.03%

# default mtry is sqrt(p) where p = number of predictors
attributes(rf)
rf$confusion


library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99% all the time

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# attribute_tune_df.rds prediction accuracy = 82%
# attribute_tune_df_1.rds prediction accuracy = 80%


plot(rf)
# attribute_tune_df.rds ntree = 400
# as ntree grows, OOB is on y axis

# Tune RF
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 400, # looking at plot(rf), we see that we can't improve our OOB after xxx trees, so that is why we picked this value
            trace = TRUE,
            improve = 0.05)

# go back to rf model setting new parameters
set.seed(222)
rf <- randomForest(Cluster~., data = train,
                   ntree = 400, 
                   mtry =5, 
                   importance = TRUE,
                   proximity = TRUE) # seems optional?

print(rf)
# attribute_tune_df.rds when mtry = 3, OOB = 27.57
# when mtry = 12, OOB = 27.57
# attribute_tune_df_1.rds, OOB = 27.03%
# attribute_tune_df_2.rds, OOB = 25.95%
# attribute_tune_df_3.rds, OOB = 26.49%

# Try predictions again
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99% all the time

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# attribute_tune_df.rds when mtry = 12, prediction accuracy 82%
# attribute_tune_df_2.rds, prediction accuracy 82%
# attribute_tune_df_3.rds, prediction accuracy 83%

# Variable importance
varImpPlot(rf)
# MeanDecreaseAccuracy, how does this variable contribute to the accuracy of the model
# MeanDecreaseGini, Gini is a measure of node purity, how does this variable contribute to node purity, 
# can make some plot specifications
varImpPlot(rf,
           sort = T,
           n.var = 30, # number of variables to show
           main = "Plot Title")

imp_df <- importance(rf) # gives values for importance

varUsed(rf) # which predictors are actually used in the rf? you get a value for each variable for how many times it was used
# should align with importance

# Partial dependence plot, 
# shows marginal effect of a vriable on the class probability for classification
partialPlot(rf, train, variable of interest, "class of interest")
partialPlot(rf, train, OpenWater_pct, "1")
partialPlot(rf, train, Soil_Salinity, "1")
partialPlot(rf, train, WBM_PRSNOW, "1")
partialPlot(rf, train, BFI_pct, "1")

library(pdp)
partial(rf, plot = TRUE, pred.var = "Soil_Salinity") # creates plot
partial(rf, plot = FALSE, pred.var = "Soil_Salinity") # gives values


# for a spectrum of values for that variable, for which part of the spectrum does it tend to predict each class most strongly?
# for a class with less accuracy and higher confusion, you may be able to see that in these plots