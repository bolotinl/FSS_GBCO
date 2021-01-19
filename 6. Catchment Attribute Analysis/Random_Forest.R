# PREP ####
# Bring df back in for RF 
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df.rds")

# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID"))

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

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
rf <-randomForest(Cluster~.,data=dat, ntree=500, importance = TRUE, mtry = 4)
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


# TIDY MODELS WORKFLOW MODELING #############################################################################################
# Tidymodels workflow: 
library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(ranger)
library(vip)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df.rds")

# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID"))

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)
dat <- as_tibble(dat)
levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 1")
levels(dat$Cluster) <- c(levels(dat$Cluster), "Class 2")
dat$Cluster[dat$Cluster == "1"] <- "Class 1"
dat$Cluster[dat$Cluster == "2"] <- "Class 2"
dat$Cluster <- factor(dat$Cluster)

# See distribution of clusters so we can ensure it will be the same in our training vs testing datasets
dat %>% 
  count(Cluster) %>% 
  mutate(prop = n/sum(n))

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


set.seed(234)
val_set <- validation_split(dat_other, 
                            strata = Cluster, 
                            prop = 0.80)
val_set

cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
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
            grid = 25,
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
  roc_curve(Cluster, `.pred_Class 1`) %>% 
  mutate(model = "Random Forest")

# the last model
last_rf_mod <- 
  rand_forest(mtry = 10, min_n = 35, trees = 1000) %>% 
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


last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 62)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(Cluster, `.pred_Class 1`) %>% 
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
