# 1) Run random forest classification model to predict cluster membership or each of our sites
# 2) Use random forest model to investigate relative variable importance for determining cluster membership

# https://www.youtube.com/watch?v=dJclNIN-TPo # Good info in this video
library(randomForest)
library(tidyverse)
library(tidymodels)

# Bring df in and format
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_tune_df_1.5.rds")

# Change variable names to be more clear and intuitive
names(dat)
# dat <- dat %>%
#   rename(Soil_Salinity = Salinity, Soil_pH = pH, Soil_Thickness = Thickness, Soil_OM_Content = OM_Content)

dat <- dat %>% # Specifically for any df's after attribute_tune_df.rds where LU classes are combined:
  rename(Open_Water_Cover = OpenWater2_pct, Temperature_Avg = WBM_TAVG, Precipitation_avg = WBM_PRCP, Precip_pct_Snow_avg = PRSNOW,nDams = NDAMS2013, 
         Fe2O3_Geol_Content = OLSON_Fe203, Atms_Dep_Mg = NADP_MG, AET_avg = WBM_AET, Elevation_avg = Elevation_Mean, Undeveloped_Land_Cover = Undeveloped_pct, 
         Soil_pH = pH, Soil_Salinity = Salinity, Agricultural_Land_Cover = Agriculture_pct, K2O_Geol_Content = OLSON_K2O, P2O5_Geol_Content = OLSON_P2O5,
         CaO_Geol_Content = OLSON_CaO, Geol_Erodibility = OLSON_CompressStrength, Geol_Hydro_Conductivity = OLSON_HydroConductivity,
         Soil_Sand_Fraction = Sand_avg, Soil_Organic_Matter = OM_Content, Soil_Clay_Fraction = Clay_avg, Irrigated_Agriculture_Cover = MIRAD_Irrig_Ag_Land_pct, Atms_Dep_Ca = NADP_CA,
         MgO_Geol_Content = OLSON_MgO, Developed_Land_Cover = Developed_pct, Soil_Silt_Fraction = Silt_avg, S_Geol_Content = OLSON_S, Baseflow_Index = BFI_pct, Soil_Moisture_Strg = WBM_SM_STRG)

# Remove cluster results for clustering ran on NON-flow normalized specific conductance data
dat <- dat %>%
  select(-c("Cluster_SC"))
dat <- dat %>%
  rename(Cluster = Cluster_SCQ)

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

# Check distribution of classes in the dataset
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
rf <- randomForest(Cluster ~., data = dat) # assumes classification if dependent variable is a factor

print(rf)
# attribute_tune_df.rds OOB = 28.11%
# attribute_tune_df_1.rds OOB = 27.03%
# attribute_tune_df_2.rds OOB = 26.49%
# attribute_tune_df_3.rds OOB = 27.03%
# attribute_tune_df_4.rds OOB = 28.11%
# attribute_tune_df_5.rds OOB = 26.49%
# attribute_tune_df_1.5.rds OOB = 25.95%
# atribute_tune_df_1.75.rds OOB = 27.03%

# default mtry is sqrt(p) where p = number of predictors
attributes(rf)
rf$confusion

# Prediction and Confusion Matrix
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99% all the time

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# attribute_tune_df.rds prediction accuracy = 82%
# attribute_tune_df_1.rds prediction accuracy = 80%
# attribute_tune_df_5.rds prediction accuracy = 83%
# attriute_tune_df_1.5.rds prediction accuracy = 85%
# attribute_tune_df_1.75.rds prediction accuracy = 87%


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
                   mtry = 5, 
                   importance = TRUE,
                   proximity = TRUE) # seems optional?

print(rf)
# attribute_tune_df.rds when mtry = 3, OOB = 27.57
# when mtry = 12, OOB = 27.57
# attribute_tune_df_1.rds, OOB = 27.03%
# attribute_tune_df_2.rds, OOB = 25.95%
# attribute_tune_df_3.rds, OOB = 26.49%
# attribute_tune_df_4.rds, OOB = 28.65%
# attribute_tune_df_5.rds, OOB = 28.11%
# attribute_tune_df_1.5.rds, OOB = 25.95% when ntree = 400 and mtry = 5

# rfcv(trainx = train[,2:34], trainy = train[,1], cv.fold = 5, recursive = TRUE)

# Try predictions again
p1 <- predict(rf, train)
confusionMatrix(p1, train$Cluster)
# accuracy is ~99% all the time

p2 <- predict(rf, test)
confusionMatrix(p2, test$Cluster)
# attribute_tune_df.rds when mtry = 12, prediction accuracy 82%
# attribute_tune_df_2.rds, prediction accuracy 82%
# attribute_tune_df_3.rds, prediction accuracy 83%
# attribute_tune_df_5.rds, prediction accuracy 82%
# attribute_tune_df_1.5.rds, prediction accuracy 85%

# Variable importance
# MeanDecreaseAccuracy, how does this variable contribute to the accuracy of the model
# MeanDecreaseGini, Gini is a measure of node purity, how does this variable contribute to node purity, 
varImpPlot(rf)

# can make some plot specifications
varImpPlot(rf,
           sort = T,
           n.var = 10, # number of variables to show
           main = "Plot Title")

imp_df <- importance(rf) # gives values for importance
imp_df <- as.data.frame(imp_df)
imp_df$var <- rownames(imp_df)

imp_df <- imp_df %>% arrange(desc(MeanDecreaseAccuracy))

library(gcookbook)
library(wesanderson)
library(hrbrthemes)

imp_df %>%
  ggplot(aes(fct_reorder(var, MeanDecreaseAccuracy), MeanDecreaseAccuracy))+
  geom_col(fill = wes_palette("Zissou1", 1, "discrete"))+
  coord_flip()+
  theme_ipsum()+
  labs(title = "Relative Variable Importance")+
  theme(axis.title.x = element_text(hjust = 0.5, size = 24, face = "bold"))+ 
  theme(axis.text.y = element_text(size = 22, face = "bold"))+
  theme(axis.text.x = element_text(size = 22))+
  theme(axis.title.y = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))

varUsed(rf) # which predictors are actually used in the rf? you get a value for each variable for how many times it was used
# should align with importance

# Partial dependence plot
# shows marginal effect of a vriable on the class probability for classification
# partialPlot(rf, train, variable of interest, "class of interest")
partialPlot(rf, train, Precipitation_avg, "1")
partialPlot(rf, train, Undeveloped_Land, "1")
partialPlot(rf, train, WBM_PRSNOW, "1")
partialPlot(rf, train, BFI_pct, "1")

library(pdp)
partial(rf, plot = TRUE, pred.var = "Precipitation_avg", which.class = 2) # creates plot

# Try plotting pdp for both classes on one plot
pdp_df_1 <- partial(rf, plot = FALSE, pred.var = "Precipitation_avg", which.class = 1) # gives values
pdp_df_2 <- partial(rf, plot = FALSE, pred.var = "Precipitation_avg", which.class = 2)

ggplot()+
  geom_line(dat = pdp_df_1, mapping = aes(x = Precipitation_avg, y = yhat))+
  geom_line(dat = pdp_df_2, mapping = aes(x = Precipitation_avg, y = yhat))

pdp_df_1 <- partial(rf, plot = FALSE, pred.var = "Open_Water_Cover", which.class = 1) # gives values
pdp_df_2 <- partial(rf, plot = FALSE, pred.var = "Open_Water_Cover", which.class = 2)

ggplot()+
  geom_line(dat = pdp_df_1, mapping = aes(x = Open_Water_Cover, y = yhat))+
  geom_line(dat = pdp_df_2, mapping = aes(x = Open_Water_Cover, y = yhat))
# The pdp of one class is just the inverse of the pdp for the other class
pdp_P_1 <- partial(rf, plot = FALSE, pred.var = "Precipitation_avg", which.class = 1) # gives values

pdp_OW_1 <- partial(rf, plot = FALSE, pred.var = "Open_Water_Cover", which.class = 1) # gives values
pdp_E_1 <- partial(rf, plot = FALSE, pred.var = "Elevation_avg", which.class = 1) # gives values
pdp_U_1 <- partial(rf, plot = FALSE, pred.var = "Undeveloped_Land_Cover", which.class = 1) # gives values
pdp_P_1 <- partial(rf, plot = FALSE, pred.var = "Precipitation_avg", which.class = 1) # gives values
pdp_A_1 <- partial(rf, plot = FALSE, pred.var = "AET_avg", which.class = 1) # gives values

theme_set(theme_ipsum()+
            theme(axis.title.x = element_text(hjust = 0.5, size = 24, face = "bold"))+ 
            theme(axis.text.y = element_text(size = 14, face = "bold"))+
            theme(axis.text.x = element_text(size = 22, face = "bold"))+
            theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold")))

p1 <- ggplot()+
  geom_line(dat = pdp_P_1, mapping = aes(x = Precipitation_avg, y = yhat))+
  theme(plot.margin = unit(c(1,0.5,0.5,0), "cm"))+
  labs(y = " ")

p2 <- ggplot()+
  geom_line(dat = pdp_OW_1, mapping = aes(x = Open_Water_Cover, y = yhat))+
  theme(plot.margin = unit(c(0.25,0.5,0.5,0), "cm"))+
  labs(y = " ")

p3 <- ggplot()+
  geom_line(dat = pdp_E_1, mapping = aes(x = Elevation_avg, y = yhat))+
  theme(plot.margin = unit(c(0.25,0.5,0.5,0), "cm"))+
  labs(y = "")+
  theme(axis.title.y = element_text(hjust = 0.5, size = 24, face = "bold"))

p4 <- ggplot()+
  geom_line(dat = pdp_U_1, mapping = aes(x = Undeveloped_Land_Cover, y = yhat))+
  theme(plot.margin = unit(c(0.25,0.5,0.5,0), "cm"))+
  labs(y = " ")

p5 <- ggplot()+
  geom_line(dat = pdp_A_1, mapping = aes(x = AET_avg, y = yhat))+
  theme(plot.margin = unit(c(0.25,0.5,0.5,0), "cm"))+
  labs(y = " ")

library(cowplot)

plot_grid(p1, p2, p3, p4, p5, align = "left", ncol = 1)



# for a spectrum of values for that variable, for which part of the spectrum does it tend to predict each class most strongly?
# for a class with less accuracy and higher confusion, you may be able to see that in these plots

# 
