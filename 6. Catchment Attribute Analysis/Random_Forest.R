#################################################################################
# https://www.youtube.com/watch?v=dJclNIN-TPo
library(randomForest)
library(tidyverse)
library(tidymodels)

# Bring df in and format
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("attribute_df.rds")
dat <- readRDS("attribute_tune_df.rds")
# dat <- readRDS("attribute_df_add_nadp.rds")

# Change variable names to be more clear and intuitive
# dat <- dat %>%
#   rename(Soil_Salinity = Salinity, Soil_pH = pH, Soil_Thickness = Thickness, Soil_Permeability = Permeability, Soil_OM_Content = OM_Content)
# Not working?
dat <- dat %>%
  rename(Soil_Salinity = Salinity, Soil_pH = pH, Soil_Thickness = Thickness, Soil_OM_Content = OM_Content)


# remove the SiteID and COMID so it isn't used by the RF model (if you haven't already)
dat <- dat %>%
  select(-c("SiteID", "COMID"))
# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

set.seed(123)
dat %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n)) # ~70/30
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
# both still ~70/30

# run the model with default parameters
set.seed(222)
rf <- randomForest(Cluster ~., data = train) # assumes classification if dependent variable is a factor
print(rf)
# OOB 27.17
# 26.63

# default mtry is sqrt(p) where p = number of predictors
attributes(rf)
rf$confusion


library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99%

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# accuracy comes down a bit to ~75%
# 73

plot(rf)
# as ntree grows, OOB is on y axis

# Tune RF
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500, # looking at plot(rf), we see that we can't improve our OOB after 300 trees, so that is why we picked this value
            trace = TRUE,
            improve = 0.05)


# go back to rf model setting new parameters
set.seed(222)
rf <- randomForest(Cluster~., data = train,
                   ntree = 500, 
                   mtry =12, 
                   importance = TRUE,
                   proximity = TRUE) # seems optional?

print(rf)
# OOB 26.63%, so it improved
# WIth tuned df OOB incrased to 28.8%

# Try predictions again
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99%

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# accuracy improved to 77%

# Variable importance
varImpPlot(rf)
# MeanDecreaseAccuracy, how does this variable contribute to the accuracy of the model
# MeanDecreaseGini, Gini is a measure of node purity, how does this variable contribute to node purity, 
# can make some plot specifications
varImpPlot(rf,
           sort = T,
           n.var = 62, # number of variables to show
           main = "Plot Title")

importance(rf) # gives values for importance

varUsed(rf) # which predictors are actually used in the rf? you get a value for each variable for how many times it was used
# should align with importance

# Partial dependence plot, 
# shows marginal effect of a vriable on the class probability for classification
partialPlot(rf, train, variable of interest, "class of interest")
partialPlot(rf, train, OpenWater_pct, "1")
partialPlot(rf, train, Soil_Salinity, "1")
partialPlot(rf, train, WBM_PRSNOW, "1")
partialPlot(rf, train, BFI_pct, "1")

# for a spectrum of values for that variable, for which part of the spectrum does it tend to predict each class most strongly?
# for a class with less accuracy and higher confusion, you may be able to see that in these plots



































#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################











# Bring df back in for RF 
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df.rds")
# dat <- readRDS("attribute_tune_df.rds")
# dat <- readRDS("attribute_df_add_nadp.rds")
# dat <- readRDS("attribute_df_4cl.rds")

# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID"))

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

# TIDY MODELS WORKFLOW MODELING #############################################################################################
# Tidymodels workflow: 
library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(ranger)
library(vip)

# See distribution of clusters so we can ensure it will be the same in our training vs testing datasets
dat %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n)) # ~70/30

# Split data into testing vs training datasets
set.seed(123)
splits      <- initial_split(dat, strata = Cluster)

dat_other <- training(splits)
dat_test  <- testing(splits)

# Check that we kept the distribution of clusters
dat_other %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n))

dat_test  %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n))
# both still ~70/30

# Create validation set 
set.seed(234)
val_set <- validation_split(dat_other,
                            strata = Cluster,
                            prop = 0.80)
val_set

cores <- parallel::detectCores()
cores

# All predictors ####
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")
rf_mod

rf_recipe <-
  recipe(Cluster ~ ., data = dat_other)

rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

rf_mod

rf_mod %>%
  parameters()

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(val_set,
            grid = 45, # How do you chose this number?
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>%
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <-
  rf_res %>%
  select_best(metric = "roc_auc")
rf_best

rf_res %>%
  collect_predictions()

rf_auc <-
  rf_res %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(Cluster, .pred_1) %>%
  mutate(model = "Random Forest")

# the last model
last_rf_mod <-
  rand_forest(mtry = 21, min_n = 39, trees = 500) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# the last workflow
last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(splits)

last_rf_fit

last_rf_fit %>%
  collect_metrics()
# A tibble: 2 x 4
# .metric   .estimator     .estimate .config             
# <chr>     <chr>          <dbl>     <chr>               
# 1 accuracy binary         0.787    Preprocessor1_Model1
# 2 roc_auc  binary         0.828    Preprocessor1_Model1

# with Atmospheric Deposition added:
# A tibble: 2 x 4
# .metric     .estimator    .estimate .config             
# <chr>       <chr>          <dbl>     <chr>               
# 1 accuracy  binary         0.770     Preprocessor1_Model1
# 2 roc_auc   binary         0.807     Preprocessor1_Model1

last_rf_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 62) #num_features doesn't have to be all of them. It will show the top *num_features value you set* features.

last_rf_fit %>%
  collect_predictions() %>%
  roc_curve(Cluster, .pred_1) %>%
  autoplot()


# Fewer predictors and land use not combined ####
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")

rf_recipe <-
  recipe(Cluster ~ ., data = dat_other)

rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

rf_mod

rf_mod %>%
  parameters()

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(val_set,
            grid = 45, # How do you chose this number?
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>%
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <-
  rf_res %>%
  select_best(metric = "roc_auc")
rf_best

rf_res %>%
  collect_predictions()

rf_auc <-
  rf_res %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(Cluster, .pred_1) %>%
  mutate(model = "Random Forest")

# the last model
last_rf_mod <-
  rand_forest(mtry = 5, min_n = 7, trees = 500) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# the last workflow
last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(splits)

last_rf_fit

last_rf_fit %>%
  collect_metrics()
# A tibble: 2 x 4
# .metric    .estimator     .estimate .config             
# <chr>        <chr>          <dbl>    <chr>               
# 1 accuracy  binary         0.770    Preprocessor1_Model1
# 2 roc_auc   binary         0.749    Preprocessor1_Model1

last_rf_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 62) #num_features doesn't have to be all of them. It will show the top *num_features value you set* features.

last_rf_fit %>%
  collect_predictions() %>%
  roc_curve(Cluster, .pred_1) %>%
  autoplot()


# Fewer predictors and land use combined ####
rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")

rf_recipe <-
  recipe(Cluster ~ ., data = dat_other)

rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

rf_mod

rf_mod %>%
  parameters()

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(val_set,
            grid = 45, # How do you chose this number?
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>%
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <-
  rf_res %>%
  select_best(metric = "roc_auc")
rf_best

rf_res %>%
  collect_predictions()

rf_auc <-
  rf_res %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(Cluster, .pred_1) %>%
  mutate(model = "Random Forest")

# the last model
last_rf_mod <-
  rand_forest(mtry = 6, min_n = 2, trees = 500) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# the last workflow
last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(splits)

last_rf_fit

last_rf_fit %>%
  collect_metrics()
# A tibble: 2 x 4
# .metric     .estimator    .estimate .config             
# <chr>        <chr>          <dbl>   <chr>               
# 1 accuracy  binary         0.754   Preprocessor1_Model1
# 2 roc_auc   binary         0.762   Preprocessor1_Model1

last_rf_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 62) #num_features doesn't have to be all of them. It will show the top *num_features value you set* features.

last_rf_fit %>%
  collect_predictions() %>%
  roc_curve(Cluster, .pred_1) %>%
  autoplot()



# Validation Set #
# setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("attribute_df.rds")
# dat <- readRDS("attribute_tune_df.rds")
# # Now remove the SiteID and COMID so it isn't used by the RF model
# dat <- dat %>%
#   select(-c("SiteID", "COMID")) # might not be necessary depending on which .rds file you use
# 
# # Turn cluster into a factor
# sapply(dat, class)
# dat$Cluster <- as.factor(dat$Cluster)
# dat <- as_tibble(dat)
# # Might be able to get rid of this later
# levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 1")
# levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 2")
# dat$Cluster[dat$Cluster == "1"] <- "Class 1"
# dat$Cluster[dat$Cluster == "2"] <- "Class 2"
# dat$Cluster <- factor(dat$Cluster) # Get rid of unused factor levels
# 
# # See distribution of clusters so we can ensure it will be the same in our training vs testing datasets
# dat %>% 
#   count(Cluster) %>% 
#   mutate(prop = n/sum(n)) # ~70/30
# 
# # Split data into testing vs training datasets
# set.seed(123)
# splits      <- initial_split(dat, strata = Cluster)
# 
# dat_other <- training(splits)
# dat_test  <- testing(splits)
# 
# # Check that we kept the distribution of clusters
# dat_other %>% 
#   count(Cluster) %>% 
#   mutate(prop = n/sum(n))
# 
# dat_test  %>% 
#   count(Cluster) %>% 
#   mutate(prop = n/sum(n))
# # both still ~70/30
# 
# # Use validation set instead of 10-fold cross validation
# # NOTE: I think we may still want to use CV10 because our dataset is much smaller than the one used here 
# set.seed(234)
# val_set <- validation_split(dat_other, 
#                             strata = Cluster, 
#                             prop = 0.80)
# val_set
# 
# cores <- parallel::detectCores()
# cores
# 
# rf_mod <- 
#   rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
#   set_engine("ranger", num.threads = cores) %>% 
#   set_mode("classification")
# 
# rf_recipe <- 
#   recipe(Cluster ~ ., data = dat_other) 
# 
# rf_workflow <- 
#   workflow() %>% 
#   add_model(rf_mod) %>% 
#   add_recipe(rf_recipe)
# 
# rf_mod
# 
# rf_mod %>%    
#   parameters()  
# 
# set.seed(345)
# rf_res <- 
#   rf_workflow %>% 
#   tune_grid(val_set,
#             grid = 45, # How do you chose this number?
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))
# 
# rf_res %>% 
#   show_best(metric = "roc_auc")
# 
# autoplot(rf_res)
# 
# rf_best <- 
#   rf_res %>% 
#   select_best(metric = "roc_auc")
# rf_best
# 
# rf_res %>% 
#   collect_predictions()
# 
# rf_auc <- 
#   rf_res %>% 
#   collect_predictions(parameters = rf_best) %>% 
#   roc_curve(Cluster, `.pred_Class 1`) %>% 
#   mutate(model = "Random Forest")
# 
# # the last model
# last_rf_mod <- 
#   rand_forest(mtry = 13, min_n = 35, trees = 500) %>% 
#   set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
#   set_mode("classification")
# 
# # the last workflow
# last_rf_workflow <- 
#   rf_workflow %>% 
#   update_model(last_rf_mod)
# 
# # the last fit
# set.seed(345)
# last_rf_fit <- 
#   last_rf_workflow %>% 
#   last_fit(splits)
# 
# last_rf_fit
# 
# last_rf_fit %>% 
#   collect_metrics()
# 
# 
# last_rf_fit %>% 
#   pluck(".workflow", 1) %>%   
#   pull_workflow_fit() %>% 
#   vip(num_features = 47)
# 
# last_rf_fit %>% 
#   collect_predictions() %>% 
#   roc_curve(Cluster, `.pred_Class 1`) %>% 
#   autoplot()


# Cross Validation (10 fold) ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df.rds")
dat <- readRDS("attribute_tune_df.rds")
# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID")) # might not be necessary depending on which .rds file you use

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)
dat <- as_tibble(dat)
# Might be able to get rid of this later
levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 1")
levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 2")
dat$Cluster[dat$Cluster == "1"] <- "Class 1"
dat$Cluster[dat$Cluster == "2"] <- "Class 2"
dat$Cluster <- factor(dat$Cluster) # Get rid of unused factor levels

# See distribution of clusters so we can ensure it will be the same in our training vs testing datasets
dat %>% 
  count(Cluster) %>% 
  mutate(prop = n/sum(n)) # ~70/30

# Split data into testing vs training datasets
set.seed(123)
splits      <- initial_split(dat, strata = Cluster)

dat_train <- training(splits)
dat_test  <- testing(splits)

# Check that we kept the distribution of clusters
dat_train %>% 
  count(Cluster) %>% 
  mutate(prop = n/sum(n))

dat_test  %>% 
  count(Cluster) %>% 
  mutate(prop = n/sum(n))
# both still ~70/30

# Create model
rf_mod <- 
  rand_forest(trees = 500) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# Fit model to training data
set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(Cluster ~ ., data = dat_train)

rf_fit
# parsnip model object
# 
# Fit time:  121ms 
# Ranger result
# 
# Call:
#   ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~500,      num.threads = 1, verbose = FALSE, seed = sample.int(10^5,          1), probability = TRUE) 
# 
# Type:                             Probability estimation 
# Number of trees:                  500 
# Sample size:                      184 
# Number of independent variables:  62 
# Mtry:                             7 
# Target node size:                 10 
# Variable importance mode:         none 
# Splitrule:                        gini 
# OOB prediction error (Brier s.):  0.1820289 

# Estimate model performance
rf_training_pred <- 
  predict(rf_fit, dat_train) %>% 
  bind_cols(predict(rf_fit, dat_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(dat_train %>% 
              select(Cluster))

rf_training_pred %>%                # training set predictions
  roc_auc(truth = .pred_class, `.pred_Class 1`)
rf_training_pred %>%                # training set predictions
  accuracy(truth = .pred_class, .pred_class)
# Too good to be true (bc we are evaluating it based on the training data it was fit to)

# Evaluate performance on test set
rf_testing_pred <- 
  predict(rf_fit, dat_test) %>% 
  bind_cols(predict(rf_fit, dat_test, type = "prob")) %>% 
  bind_cols(dat_test %>% select(Cluster))

rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = .pred_class, `.pred_Class 1`)
# Still really good?? = 1
rf_testing_pred %>%                   # test set predictions
  accuracy(truth = Cluster, .pred_class)
# Went down a bit

# Try cross validation
set.seed(345)
folds <- vfold_cv(dat_train, v = 10)
folds

# Set up resampling
rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(Cluster ~ .)

set.seed(456) # results still change?
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

rf_fit_rs

collect_metrics(rf_fit_rs)
# A tibble: 2 x 6
# .metric  .estimator   mean     n   std_err .config             
# <chr>     <chr>      <dbl>   <int>   <dbl>  <chr>               
# 1 accuracy binary   0.713    10    0.0456  Preprocessor1_Model1
# 2 roc_auc  binary   0.766    10    0.0593  Preprocessor1_Model1

rf_fit_rs %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 10) #num_features doesn't have to be all of them. It will show the top *num_features value you set* features.

rf_fit_rs %>%
  collect_predictions() %>%
  roc_curve(Cluster, .pred_1) %>%
  autoplot()










# BLOPIG TUTORIAL ##############################
# https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/
# Set random seed to make results reproducible:
set.seed(17)
iris <- iris
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)

# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

#  I try to keep e^Nf < No (Nf = number of features, No = number of observations
# In this case, our training set has 75 observations, which suggests that using four features (e^4 ~ 54.6) is not entirely absurd.
# Classes are pretty even in this dataset, so we don't need to do any specific splitting to keep proportions the same
library(randomForest)
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
# ntree defines the number of trees to be generated. It is typical to test a range 
# of values for this parameter (i.e. 100,200,300,400,500) and choose the one that minimises the OOB estimate of error rate.
# The default value for mtry, when performing classification, is sqrt(number of features)
rf_classifier

# For more complicated data sets, i.e. when a higher number of features is present, a good idea 
# is to use cross-validation to perform feature selection using the OOB error rate (see rfcv from randomForest for more details).
varImpPlot(rf_classifier)

# MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance 
# when that particular variable is omitted from the training set. Caveat: if two variables 
# are somewhat redundant, then omitting one of them may not lead to massive gains in prediction performance, but would make the second variable more important.
# MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this, if you use this 
# feature to split the data, how pure will the nodes be? Highest purity means that each 
# node contains only elements of a single class. Assessing the decrease in GINI when that feature is omitted leads to an understanding of how important that feature is to split the data correctly.

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)

# Validation set assessment #2: ROC curves and AUC

# Needs to import ROCR package for ROC curve plotting:
library(ROCR)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

# SIMPLIFIED VERSION #################################################################
# Over simplified example:
# Random Forest # following this tutorial: https://www.listendata.com/2014/11/random-forest-with-r.html#What-is-Random-Forest-

# Run the RF model
library(randomForest)
set.seed(71)
rf <-randomForest(Cluster~.,data=dat, ntree=500, importance = TRUE)
print(rf)

# Select mtry value with minimum out of bag(OOB) error.
mtry <- tuneRF(dat[-1],dat$Cluster, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
rf <-randomForest(Cluster~.,data=dat, ntree=500, importance = TRUE, mtry = 7)
print(rf)

# It's really weird that three mtry values would have the same OOB, but let's keep it at 7 for now

importance <- importance(rf)
varImpPlot(rf)

pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], dat$Cluster)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# OOB = 23.27%

#################################################################################
# https://www.youtube.com/watch?v=dJclNIN-TPo
library(randomForest)
library(tidyverse)
library(tidymodels)

# Bring df back in for RF 
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("attribute_df.rds")
# dat <- readRDS("attribute_tune_df.rds")
dat <- readRDS("attribute_df_add_nadp.rds")
# dat <- readRDS("attribute_df_4cl.rds")
dat <- dat %>%
  rename(Soil_Salinity = Salinity, Soil_pH = pH, Soil_Thickness = Thickness, Soil_Permeability = Permeability, Soil_OM_Content = OM_Content)

# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID"))

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)
set.seed(123)

dat %>%
  count(Cluster) %>%
  mutate(prop = n/sum(n)) # ~70/30

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
# both still ~70/30

# split data with stratification
set.seed(222)

# run the model with default parameters
rf <- randomForest(Cluster ~., data = train) # assumes classification if dependent variable is a factor
print(rf)
# OOB 27.17
# OOB for 4 clusters 34.59
# default mtry is sqrt(p) where p = number of predictors
# prints OOB (~ accuracy is 100 - OOB) and confusion matrix (shows which classes have the most error in prediction)
attributes(rf)
rf$confusion
# rf$whatever attribute you want to look at

library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99%
# gives confusion matrix
# gives accuracy, 95% confidence interval (closer values are better)
# statistics by class
# this model has already seen all the data, hence mismatch in previous OOB and accuracy

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# accuracy comes down a bit to ~75%


# error rate
plot(rf)
# as ntree grows, OOB is on y axis

# model tuning example:
# t <- tuneRF(train[,-index for Cluster], train[,index for Cluster],
#        stepFactor = 1,
#        plot = TRUE,
#        ntreeTry = 300, # looking at plot(rf), we see that we can't improve our OOB after 300 trees, so that is why we picked this value
#        trace = TRUE,
#        improve = 0.05)

# model tuning NOTE: this makes our OOB 
# if it doesn't search very much, try smaller stepFactor, i.e. 0.5
# gives info on what mtry value to use
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500, # looking at plot(rf), we see that we can't improve our OOB after 300 trees, so that is why we picked this value
            trace = TRUE,
            improve = 0.05)


# go back to rf model
set.seed(222)
rf <- randomForest(Cluster~., data = train,
                   ntree = 500, 
                   mtry =8, 
                   importance = TRUE,
                   proximity = TRUE) # seems optional?
print(rf)
# OOB 25.54%, so it improved
# OOB for 4 clusters didn't change

# error should change
# classification error should change for each class
# you can go through p1 and p2 again
# accuracy of p2 should have improved slightly

p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99%
# gives confusion matrix
# gives accuracy, 95% confidence interval (closer values are better)
# statistics by class
# this model has already seen all the data, hence mismatch in previous OOB and accuracy

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)




# not sure what this is important for
# No. of nodes for the 300 trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "blue")

# variable importance
varImpPlot(rf)
# MeanDecreaseAccuracy, how does this variable contribute to the accuracy of the model
# MeanDecreaseGini, Gini is a measure of node purity, how does this variable contribute to node purity, 
# can make some plot specifications
varImpPlot(rf,
           sort = T,
           n.var = 10, # number of variables to show
           main = "Plot Title")

importance(rf) # gives values for importance

varUsed(rf) # which predictors are actually used in the rf? you get a value for each variable for how many times it was used
# should align with importance

# partial dependence plot, shows marginal effect of a vriable on the class probability for classification
partialPlot(rf, train, variable of interest, "class of interest")
partialPlot(rf, train, OpenWater_pct, "1")
partialPlot(rf, train, Salinity, "1")
# for a spectrum of values for that variable, for which part of the spectrum does it tend to predict each class most strongly?
# for a class with less accuracy and higher confusion, you may be able to see that in these plots

# not sure why we would need to do this
getTree(rf, 
        1, # which tree? 1 is the first
        labelVar = TRUE)

# multi dimensional scaling plot of proximity matrix
MDFplot(rf, train$Cluster)



