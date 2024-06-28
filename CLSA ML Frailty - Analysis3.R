# load libraries ####
library(tidymodels) # machine learning meta-engine
library(tidyverse)  # general
library(broom)      # preparing correlation tables
library(discrim)    # niave bayes engine not included in tidymodels
library(ranger)     # RF package
library(klaR)       # Naive Bayes package [Masks dplyr::select] 
library(kknn)       # KNN package
library(xgboost)    # XGBoost package
library(nnet)       # Neural network package
library(boot)       # bootstrap CIs for logistic regression ORs

# set cores to max for machine learning and grid search
all_cores <- parallel::detectCores(logical = FALSE)
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# load data #### 
# change to "ADL_DCLS_F2 %in% c(1),1," for sensitivity analysis
CLSA.baseline.FU2<- read.csv("CLSA.baseline.followup2.csv") %>%
  mutate(Binary_IADL.ADL_F2 = if_else(ADL_DCLS_F2 %in% c(1,2), 1, 
                                      if_else(is.na(ADL_DCLS_F2), NA_integer_, 0)))

# list of numeric variable
numeric_vars <- c("HUP_FREE_MCQ", "GEN_DHDI",
                  "COG_REYI_SCORE","COG_REYII_SCORE",
                  "WLK_TIME_COM","GS_EXAM_AVG_COM",
                  "AGE_NMBR","K10_DSCORE_MCQ","WHC_HIP_CM_COM","VA_ETDRS_BOTH_RSLT_COM")

# Convert all other variables to factors except the pre-defined numeric variables
CLSA.baseline.FU2 <- CLSA.baseline.FU2 %>%
  mutate(across(
    .cols = -all_of(numeric_vars), # Select all columns except those in the numeric_vars list
    .fns = as.factor # Convert the selected columns to factors
  )) %>%
  dplyr::select(-c("X", "X.1", "entity_id","WGHTS_INFLATION"))

# data
CLSA.baseline.FU2<- CLSA.baseline.FU2 %>% dplyr::select(Binary_IADL.ADL_F2,HUP_FREE_MCQ,
              GEN_DHDI,RET_RTRD,
              CR1_FRHC,
              COG_REYI_SCORE,COG_REYII_SCORE,
              WLK_TIME_COM,GS_EXAM_AVG_COM,ADL_DCLS,PA2_WALK_MCQ,
              AGE_NMBR,K10_DSCORE_MCQ,WHC_HIP_CM_COM,
              CCT_F2, TRA_DSTATUS_MCQ.binary,
              SEX_ASK, VA_ETDRS_BOTH_RSLT_COM, CCC_URIINC)%>%
  drop_na(Binary_IADL.ADL_F2) 


# split data ####
set.seed(503)
CLSA_split <- initial_split(CLSA.baseline.FU2, prop=0.80, strata = Binary_IADL.ADL_F2)
CLSA_train <- training(CLSA_split)
CLSA_test <- testing(CLSA_split)

# 6813 / 7993 (85% are level 1 in training set)
# 27051 / 31,934 (85% are level 1 in test set)
CLSA_test %>% dplyr::select(Binary_IADL.ADL_F2)%>% group_by(Binary_IADL.ADL_F2) %>% count()
CLSA_train %>% dplyr::select(Binary_IADL.ADL_F2)%>% group_by(Binary_IADL.ADL_F2) %>% count()

# cross-validation 
set.seed(1001)
clsa.folds <- vfold_cv(CLSA_train, v=10)
keep.pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

### select models ####
# random forest
rf<- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# k-nearest neighbours
knn<- nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# naïve bayes
nb <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>% 
  set_engine("klaR") %>% 
  set_mode("classification")

# multinomial logistic regression
multinom <- multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

# logistic regression
logistic <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

# xgboost
xgboost <- boost_tree(mtry = tune(), trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), sample_size = tune())  %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Neural networks
nnet_spec <- mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet", MaxNWTs = 2600) %>%
  set_mode("classification")
# Update NN to have 27 layers (Kuhn & Johnson, 2013); tidy models book pg 238
nnet_param <- nnet_spec %>% 
  extract_parameter_set_dials() %>%
  update(hidden_units=hidden_units(c(1,27)))

### recipes and grid search ####
step.rec <- recipe(Binary_IADL.ADL_F2~HUP_FREE_MCQ+
                   GEN_DHDI+RET_RTRD+
                   CR1_FRHC+
                   COG_REYI_SCORE+COG_REYII_SCORE+
                   WLK_TIME_COM+GS_EXAM_AVG_COM+ADL_DCLS+
                   AGE_NMBR+K10_DSCORE_MCQ+WHC_HIP_CM_COM+
                   CCT_F2+ TRA_DSTATUS_MCQ.binary+
                   SEX_ASK+ PA2_WALK_MCQ+ VA_ETDRS_BOTH_RSLT_COM+ CCC_URIINC,
                  data = CLSA_train) %>%
  step_impute_bag(all_predictors(), seed_val = 123) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)%>% 
  step_normalize(all_predictors())

# recipe without dummy codes
rec <- recipe(Binary_IADL.ADL_F2~HUP_FREE_MCQ+
                GEN_DHDI+RET_RTRD+
                CR1_FRHC+
                COG_REYI_SCORE+COG_REYII_SCORE+
                WLK_TIME_COM+GS_EXAM_AVG_COM+ADL_DCLS+
                AGE_NMBR+K10_DSCORE_MCQ+WHC_HIP_CM_COM+
                CCT_F2+ TRA_DSTATUS_MCQ.binary+
                SEX_ASK+ PA2_WALK_MCQ+ VA_ETDRS_BOTH_RSLT_COM+ CCC_URIINC,
              data = CLSA_train) %>%
  step_impute_bag(all_predictors(), seed_val = 123) %>%
  step_impute_bag(all_outcomes(), seed_val = 123) 

# create workflow 
workflow <- workflow_set(
  preproc = list(simple=rec),
  models = list(RF = rf,
                NB=nb))

# xgboost workflow
xgboost.wkflow <- workflow_set(
  preproc = list(step.rec),
  models = list(LR = logistic, XG = xgboost, NN = nnet_spec, KNN = knn)) 

# combine workflow
all_workflows <- bind_rows(workflow , xgboost.wkflow) 

# Tuning and evaluating the models
grid_ctrl <- control_grid(save_pred = TRUE, parallel_over = "everything",
                          save_workflow = TRUE)

# grid search for all models
grid_results <- all_workflows %>% workflow_map(seed = 1503, resamples = clsa.folds,
                                                grid=25, control=grid_ctrl)

rank_results(grid_results, rank_metric = "roc_auc") %>% filter(wflow_id == "recipe_LR")

autoplot(grid_results, metric = "roc_auc")

best_models <- grid_results %>% 
  mutate(best_result = map(result, ~ select_best(.x, metric = "roc_auc"))) 

best_models$best_result
best_models$wflow_id

LR.best.pred<-best_models %>% filter(wflow_id == "recipe_LR") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model03")  
RF.best.pred<-best_models %>% filter(wflow_id == "simple_RF") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model09")  
NB.best.pred<-best_models %>% filter(wflow_id == "simple_NB") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model01")  
XG.best.pred<-best_models %>% filter(wflow_id == "recipe_XG") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model12")  
NN.best.pred<-best_models %>% filter(wflow_id == "recipe_NN") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model03")  


RF.best.pred <- RF.best.pred %>% mutate(model = "Random Forest")
KNN.best.pred <- KNN.best.pred %>% mutate(model = "KNN")
NB.best.pred <- NB.best.pred %>% mutate(model = "Naive Bayes")
XG.best.pred <- XG.best.pred %>% mutate(model = "XGBoost")
LR.best.pred <- LR.best.pred %>% mutate(model = "Logistic Regression")
NN.best.pred<- NN.best.pred %>% mutate(model = "Neural Network")

# Combine all predictions into a single dataframe
all_predictions <- bind_rows(RF.best.pred, NB.best.pred, XG.best.pred, LR.best.pred, NN.best.pred)

all_predictions %>% group_by(model)

# Calculate ROC curve data for each model
roc_data <- all_predictions %>%
  group_by(model) %>%
  roc_curve(truth = "Binary_IADL.ADL_F2", .pred_1, event_level = "second") %>%
  ungroup() %>%
  mutate(model = factor(model, levels = unique(model)))%>%
  filter(!is.infinite(.threshold) & !is.na(.threshold))

# Plot ROC curves
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  labs(x = "1 - Specificity (False Positive Rate)", y = "Sensitivity (True Positive Rate)", color = "Model") +
  ggtitle("ROC Curves for Binary IADL at Follow-up 2") +
  theme_minimal() 


### extract performance metrics ####
y<-LR.best.pred%>%
  dplyr::select(.pred_0, .pred_1,.pred_class, Binary_IADL.ADL_F2)

conf_mat(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
accuracy(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
sens(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
specificity(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)

# Extract metrics with optimal thresholds
predictions <-LR.best.pred

# Use yardstick to compute ROC curve. Assuming the class of interest is the positive class (e.g., "1")
roc_data <- roc_curve(predictions, truth = Binary_IADL.ADL_F2, .pred_0)

roc_data_filtered <- roc_data %>%
  filter(!is.infinite(.threshold) & !is.na(.threshold))

autoplot(roc_data_filtered)

# Find the optimal threshold
# Calculate the distance to the top-left corner (0,1) for each threshold
roc_data_filtered <- roc_data_filtered %>%
  mutate(distance = sqrt((1 - sensitivity)^2 + (specificity - 1)^2))

# Identify the threshold with the minimum distance
optimal_threshold <- roc_data_filtered %>%
  filter(distance == min(distance)) %>%
  pull(.threshold)

# Print the optimal threshold, something is inverse, take this number and subtract 1
print(optimal_threshold)

# XG sensitivty
optimal_threshold<- 1-optimal_threshold
optimal_threshold

predictions_with_threshold <- predictions %>%
  mutate(predicted_class = if_else(.pred_1 >= optimal_threshold, 1, 0),
         predicted_class = as.factor(predicted_class))

# Calculate new sensitivity and specificity
metrics <- predictions_with_threshold %>%
  conf_mat(truth = Binary_IADL.ADL_F2, estimate = predicted_class) %>%
  summary()

metrics

## fit final model to test data ####
# extract best model
best_results <- 
  grid_results %>%
  extract_workflow_set_result("recipe_LR") %>% #select best model from plot
  select_best(metric = "roc_auc")
# extract hyperperameters for best model
best_penalty <- best_results$penalty
best_mixture <- best_results$mixture
# add hyperperameters to logistic regression models
glmnet_spec <- logistic_reg(penalty = best_penalty, mixture = best_mixture) %>%
  set_engine("glmnet")
# add mode to workflow
lr_workflow <- workflow() %>%
  add_model(glmnet_spec) %>%
  add_recipe(step.rec)
# fit the final model to the training data
final_fit <- lr_workflow %>%
  fit(data = CLSA_test)
# extract model fit
fitted_model <- pull_workflow_fit(final_fit)
# remove target variable from test data (so below code works)
CLSA_test.mod <- CLSA_test %>% dplyr::select(-"Binary_IADL.ADL_F2")
# Prep the recipe on training data
trained_rec <- prep(step.rec, training = CLSA_train) 
# Prepare the test data using the same recipe
test_prepared <- bake(trained_rec, new_data = CLSA_test.mod)
# predictions <- predict(final_fit, newx = test_matrix, type = "class")
predictions <- predict(fitted_model, new_data = test_prepared, type = "prob")
# predict classes
predicted_classes <- ifelse(predictions$.pred_0 > 1-0.8620799, "0", "1")
# actual_outcomes
actual_outcomes <- as.factor(CLSA_test$Binary_IADL.ADL_F2)
# Convert outcomes and predictions to factors explicitly
actual_outcomes <- as.factor(actual_outcomes)
predicted_classes <- as.factor(predicted_classes)
# Create a tibble 
results_tibble <- tibble(truth = actual_outcomes, estimate = predicted_classes)
# summary performance metrics on test data 
conf_mat <- conf_mat(data = results_tibble, truth = truth, estimate = estimate) %>%
  summary()
conf_mat 

# model summary on training data####
# extract model fit
fitted_model <- pull_workflow_fit(final_fit)
# Assuming `fitted_model` is your glmnet model object from a tidymodels workflow
coefficients_tidy <- tidy(fitted_model, exponentiate = FALSE)
# View the coefficients
print(coefficients_tidy)
# Add a column for Odds Ratios by exponentiating the estimates
coefficients_tidy <- coefficients_tidy %>%
  mutate(odds_ratio = exp(estimate))
# View the coefficients with odds ratios
print(coefficients_tidy, n=38)



# descriptive table (full sample)####

continuous_vars<-c("COG_REYI_SCORE","COG_REYII_SCORE","WHC_HIP_CM_COM",
"AGE_NMBR","K10_DSCORE_MCQ", "WLK_TIME_COM", "MSDYY_MFS","GS_EXAM_AVG_COM", "HGT_HEIGHT_M_COM",
"VA_ETDRS_BOTH_RSLT_COM")

results <- CLSA.baseline.FU2 %>%
  dplyr::select(all_of(continuous_vars)) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n_missing = ~sum(is.na(.))
  )))


# baseline IADL 
CLSA.baseline.FU2 %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# chronic conditions 
CLSA.baseline.FU2 %>% group_by(CCT_F2) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# pain free
CLSA.baseline.FU2 %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# drivers licence
CLSA.baseline.FU2 %>% group_by(TRA_DSTATUS_MCQ.binary) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# urinary incontinence 
CLSA.baseline.FU2 %>% group_by(CCC_URIINC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# general health 
CLSA.baseline.FU2 %>% group_by(GEN_DHDI) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# walking frequency
CLSA.baseline.FU2 %>% group_by(PA2_WALK_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# sex
CLSA.baseline.FU2 %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# retired
CLSA.baseline.FU2 %>% group_by(RET_RTRD) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# formal care
CLSA.baseline.FU2 %>% group_by(CR1_FRHC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# strenuous sports
CLSA.baseline.FU2 %>% group_by(PA2_SSPRTHR_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# descriptive table (functionally dependent)####
CLSA.baseline.FU2.HighADL<-CLSA.baseline.FU2 %>% filter(Binary_IADL.ADL_F2==0)
str(CLSA.baseline.FU2.HighADL)
continuous_vars<-c("COG_REYI_SCORE","COG_REYII_SCORE","WHC_HIP_CM_COM", "AGE_NMBR","K10_DSCORE_MCQ", "WLK_TIME_COM", "GS_EXAM_AVG_COM", 
                   "VA_ETDRS_BOTH_RSLT_COM")

results <- CLSA.baseline.FU2.HighADL %>%
  dplyr::select(all_of(continuous_vars)) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n_missing = ~sum(is.na(.))
  )))


# baseline IADL 
CLSA.baseline.FU2.HighADL %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# chronic conditions 
CLSA.baseline.FU2.HighADL %>% group_by(CCT_F2) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# pain free
CLSA.baseline.FU2.HighADL %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# drivers licence
CLSA.baseline.FU2.HighADL %>% group_by(TRA_DSTATUS_MCQ.binary) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# urinary incontinence 
CLSA.baseline.FU2.HighADL %>% group_by(CCC_URIINC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# general health 
CLSA.baseline.FU2.HighADL %>% group_by(GEN_DHDI) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# walking frequency
CLSA.baseline.FU2.HighADL %>% group_by(PA2_WALK_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# sex
CLSA.baseline.FU2.HighADL %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# retired
CLSA.baseline.FU2.HighADL %>% group_by(RET_RTRD) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# formal care
CLSA.baseline.FU2.HighADL %>% group_by(CR1_FRHC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)


# descriptive table (functionally independent)####

CLSA.baseline.FU2.LowADL<-CLSA.baseline.FU2 %>% filter(Binary_IADL.ADL_F2==1)
str(CLSA.baseline.FU2.LowADL)

results <- CLSA.baseline.FU2.LowADL %>%
  dplyr::select(all_of(continuous_vars)) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n_missing = ~sum(is.na(.))
  )))

count(CLSA.baseline.FU2)
count(CLSA.baseline.FU2.HighADL)
count(CLSA.baseline.FU2.LowADL)

# baseline IADL 
CLSA.baseline.FU2.LowADL %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# chronic conditions 
CLSA.baseline.FU2.LowADL %>% group_by(CCT_F2) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# pain free
CLSA.baseline.FU2.LowADL %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# drivers licence
CLSA.baseline.FU2.LowADL %>% group_by(TRA_DSTATUS_MCQ.binary) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# urinary incontinence 
CLSA.baseline.FU2.LowADL %>% group_by(CCC_URIINC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# general health 
CLSA.baseline.FU2.LowADL %>% group_by(GEN_DHDI) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# walking frequency
CLSA.baseline.FU2.LowADL %>% group_by(PA2_WALK_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# sex
CLSA.baseline.FU2.LowADL %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# retired
CLSA.baseline.FU2.LowADL %>% group_by(RET_RTRD) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# formal care
CLSA.baseline.FU2.LowADL %>% group_by(CR1_FRHC) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# strenuous sports
CLSA.baseline.FU2.LowADL %>% group_by(PA2_SSPRTHR_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# medication use
CLSA.baseline.FU2.LowADL %>% group_by(MED_USE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
