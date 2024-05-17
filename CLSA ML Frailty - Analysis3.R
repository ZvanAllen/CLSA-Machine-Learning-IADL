# load libraries ####
library(tidymodels) # machine learning meta-engine
library(tidyverse)  # general
library(broom)      # preparing correlation tables
library(discrim)    # niave bayes engine not included in tidymodels
library(ranger)     # RF package
library(klaR)       # Naive Bayes package [Masks dplyr::select] ugh!
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
CLSA.baseline.FU2<- read.csv("CLSA.baseline.followup2.csv") %>%
  mutate(Binary_IADL.ADL_F2 = if_else(ADL_DCLS_F2 %in% c(1, 2), 1, 
                                      if_else(is.na(ADL_DCLS_F2), NA_integer_, 0)))

# list of numeric variable
numeric_vars <- c("PA2_DSCR2_MCQ","NUT_FBR_NB_COM","NUT_BRD_NB_COM","NUT_MEAT_NB_COM",
                  "NUT_MTOT_NB_COM","NUT_CHCK_NB_COM","NUT_FISH_NB_COM","NUT_SASG_NB_COM","NUT_PATE_NB_COM",
                  "NUT_SAUC_NB_COM","NUT_O3EG_NB_COM","NUT_EGGS_NB_COM","NUT_LEGM_NB_COM","NUT_NUTS_NB_COM",
                  "NUT_FRUT_NB_COM","NUT_GREEN_NB_COM","NUT_PTTO_NB_COM","NUT_FRIE_NB_COM","NUT_CRRT_NB_COM",
                  "NUT_VGOT_NB_COM","NUT_LWCS_NB_COM","NUT_CHSE_NB_COM","NUT_LWYG_NB_COM","NUT_YOGR_NB_COM",
                  "NUT_CALC_NB_COM","NUT_DAIR_NB_COM","NUT_SALT_NB_COM","NUT_DSRT_NB_COM","NUT_CHOC_NB_COM",
                  "NUT_BTTR_NB_COM","NUT_DRSG_NB_COM","NUT_CAJC_NB_COM","NUT_PURE_NB_COM","NUT_CAML_NB_COM",
                  "NUT_WHML_NB_COM","NUT_LFML_NB_COM","NUT_CADR_NB_COM","SLE_HOUR_NB_COM","SLE_30DUR_NB_COM",
                  "SLE_MIDDUR_NB_COM","SLE_STAYDUR_NB_COM","SLE_LGDUR_NB_COM","MEDI_NO_COM",
                  "STP_DOTTIME_SS_COM","STP_WORTIME_SS_COM","STP_COLTIME_SS_COM","FAS_F_SCORE_COM",
                  "FAS_A_SCORE_COM","FAS_S_SCORE_COM","CRT_MRT_CORRANS_COM","K10_DSCORE_MCQ",
                  "PER_DSCR_EXT_MCQ","PER_DSCR_AGR_MCQ","PER_DSCR_CON_MCQ","PER_DSCR_EMOS_MCQ",
                  "PER_DSCR_OPEX_MCQ","FAS_TOTAL_SCORE_COM","ADM_DCS_AGE_COM","WLK_TIME_COM",
                  "BP_SYSTOLIC_ALL_AVG_COM","BP_DIASTOLIC_ALL_AVG_COM","BP_PULSE_ALL_AVG_COM",
                  "CR_TIME_COM","CR_AVG_TIME_COM","GS_EXAM_MAX_COM","GS_EXAM_AVG_COM","HGT_HEIGHT_M_COM",
                  "WGT_WEIGHT_KG_COM","HBP_AGE_NB_COM","HYP_UTHYRAGE_NB_COM","IHD_ANGIAGE_NB_COM",
                  "STR_CVAAGE_NB_COM","DIA_AGE_NB_COM","CAO_COPDAGE_NB_COM","BAL_TIME_R_COM","BAL_TIME_L_COM",
                  "BAL_BEST_COM","TON_QUALITYINDEX_R_COM","TON_QUALITYINDEX_L_COM","VA_ETDRS_BOTH_RSLT_COM",
                  "WHC_WAIST_CM_COM","WHC_HIP_CM_COM","FRAX_OSTFX_COM","FRAX_OSTFX_BMD_COM","SDC_POPDENSITY_COM",
                  "RET_SPSEAG_NB_TRM","AGE_NMBR","SMK_FRSTCG_AG","SMK_CGDL_AG","WHO_MPAG_AG","WHO_HRTAG_AG",
                  "WHO_HRTYR_YR","RET_AGE_NB","LFP_LAST_NB","SSA_DPALL","DEP_CESD10","ADL_DSUM","SLS_DSCR",
                  "SMK_DYCS","COG_REYI_SCORE","COG_AFT_SCORE_1","COG_AFT_SCORE_2","COG_MAT_SCORE",
                  "COG_REYII_SCORE","SO2OMIYY","PM25DALYY_01","PM25DALYY_02","O3CHGYY_01","GRLANYY_07",
                  "GRLANYY_08","GRLANYY_09","GRLANYY_01","GRLANYY_04","GRLANYY_16","GRLANYY_12",
                  "GRLANYY_17","GRLANYY_13","GRLANYY_18","GRLANYY_14","GRLANYY_10","GRLANYY_19",
                  "GRLANYY_27","GRLANYY_26","GRLANYY_25","GRLANYY_23","GRLANYY_22","GRLANYY_21",
                  "GRLANYY_07_2011","GRLANYY_07_2013","GRLANYY_08_2011","GRLANYY_08_2013","GRLANYY_09_2011",
                  "GRLANYY_09_2013","GRLANYY_01_2011","GRLANYY_01_2013","GRLANYY_04_2011","GRLANYY_04_2013",
                  "GRLANYY_16_2011","GRLANYY_16_2013","GRLANYY_12_2011","GRLANYY_12_2013","GRLANYY_17_2011",
                  "GRLANYY_17_2013","GRLANYY_13_2011","GRLANYY_13_2013","GRLANYY_18_2011","GRLANYY_18_2013",
                  "GRLANYY_14_2011","GRLANYY_14_2013","GRLANYY_10_2011","GRLANYY_10_2013","GRLANYY_19_2011",
                  "GRLANYY_19_2013","GRLANYY_27_2011","GRLANYY_27_2013","GRLANYY_26_2011","GRLANYY_26_2013",
                  "GRLANYY_25_2011","GRLANYY_25_2013","GRLANYY_23_2011","GRLANYY_23_2013","GRLANYY_22_2011",
                  "GRLANYY_22_2013","GRLANYY_21_2011","GRLANYY_21_2013","MSDYY_MFS","MSDYY_SFS","MSDYY_MPPR",
                  "MSDYY_DAPOP","MSDYY_SPPR","LGTNLTYY_01","WTHNRCYY_08","WTHNRCYY_13","ALE16_02","ALE16_03",
                  "ALE16_04","ALE16_05","ALE16_06","ALE16_07","ALE16_08","ALE16_09","ALE16_10","ALE16_11",
                  "ALE16_12","ALE16_13","bp_1yr_cold","bp_1yr","bp_1yr_warm","bp_5yrs_cold","bp_5yrs",
                  "bp_5yrs_warm","bp_7days_cold","bp_7days_warm","bp_exp","Locrds_length_200m",
                  "no2_1yr_cold","no2_1yr","no2_1yr_warm","no2_5yrs_cold","no2_5yrs","no2_5yrs_warm",
                  "no2_7days_cold","no2_7days_warm","no2_exp","NO2F","o3_1yr_cold","o3_1yr","o3_1yr_warm",
                  "o3_5yrs_cold","o3_5yrs","o3_5yrs_warm","o3_7days_cold","o3_7days_warm","o3_exp",
                  "o38h_1yr_cold","o38h_1yr","o38h_1yr_warm","o38h_5yrs_cold","o38h_5yrs","o38h_5yrs_warm",
                  "o38h_7days_cold","o38h_7days_warm","o38h_exp","PM25_08_12","pm25_1yr_cold","pm25_1yr",
                  "pm25_1yr_warm","pm25_5yrs_cold","pm25_5yrs","pm25_5yrs_warm","pm25_7days_cold",
                  "pm25_7days_warm","pm25_exp","rh_1yr_cold","rh_1yr","rh_1yr_warm","rh_5yrs_cold",
                  "rh_5yrs","rh_5yrs_warm","rh_7days_cold","rh_7days_warm","rh_exp","so2_1yr_cold",
                  "so2_1yr","so2_1yr_warm","so2_5yrs_cold","so2_5yrs","so2_5yrs_warm","so2_7days_cold",
                  "so2_7days_warm","so2_exp","temp_1yr_cold","temp_1yr","temp_1yr_warm","temp_5yrs_cold",
                  "temp_5yrs","temp_5yrs_warm","temp_7days_cold","temp_7days_warm","temp_exp", "DIA_MEDAGE_NB_COM", "Phwy_length_200m",
                  "SLE_DRMDUR_NB_COM", "FAL_NMBR_NB_MCQ")

# Convert all other variables to factors except the pre-defined numeric variables
CLSA.baseline.FU2 <- CLSA.baseline.FU2 %>%
  mutate(across(
    .cols = -all_of(numeric_vars), # Select all columns except those in the numeric_vars list
    .fns = as.factor # Convert the selected columns to factors
  )) %>%
  dplyr::select(-c("X", "X.1", "entity_id","WGHTS_INFLATION"))


CLSA.baseline.FU2<- CLSA.baseline.FU2 %>% dplyr::select(Binary_IADL.ADL_F2,HUP_FREE_MCQ,
              GEN_DHDI,RET_RTRD,WLK_TIME_COM,
              CR1_FRHC,PA2_SSPRTHR_MCQ,HGT_HEIGHT_M_COM,PER_DSCR_CON_MCQ,
              COG_REYI_SCORE,COG_REYII_SCORE,
              WLK_TIME_COM,MSDYY_MFS,GS_EXAM_AVG_COM,ADL_DCLS,PA2_WALK_MCQ,
              AGE_NMBR,K10_DSCORE_MCQ,WHC_HIP_CM_COM,
              CCT_F2, TRA_DSTATUS_MCQ.binary,
              FAL_NMBR_NB_MCQ,
              SEX_ASK, PA2_WALK_MCQ, PA2_SSPRTHR_MCQ, VA_ETDRS_BOTH_RSLT_COM,  MED_USE_MCQ, CCC_URIINC)%>%
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
                   GEN_DHDI+RET_RTRD+WLK_TIME_COM+
                   CR1_FRHC+PA2_SSPRTHR_MCQ+HGT_HEIGHT_M_COM+PER_DSCR_CON_MCQ+
                   COG_REYI_SCORE+COG_REYII_SCORE+
                   WLK_TIME_COM+MSDYY_MFS+GS_EXAM_AVG_COM+ADL_DCLS+PA2_WALK_MCQ+
                   AGE_NMBR+K10_DSCORE_MCQ+WHC_HIP_CM_COM+
                  CCT_F2+ TRA_DSTATUS_MCQ.binary+
                   FAL_NMBR_NB_MCQ+
                   SEX_ASK+ PA2_WALK_MCQ+ PA2_SSPRTHR_MCQ+ VA_ETDRS_BOTH_RSLT_COM+  MED_USE_MCQ+ CCC_URIINC,
                  data = CLSA_train) %>%
  step_impute_bag(all_predictors(), seed_val = 123) %>%
  #step_impute_bag(all_outcomes(), seed_val = 123) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)%>% 
  step_normalize(all_predictors())

# recipe without dummy codes
rec <- recipe(Binary_IADL.ADL_F2~HUP_FREE_MCQ+
                GEN_DHDI+RET_RTRD+WLK_TIME_COM+
                CR1_FRHC+PA2_SSPRTHR_MCQ+HGT_HEIGHT_M_COM+PER_DSCR_CON_MCQ+
                COG_REYI_SCORE+COG_REYII_SCORE+
                WLK_TIME_COM+MSDYY_MFS+GS_EXAM_AVG_COM+ADL_DCLS+PA2_WALK_MCQ+
                AGE_NMBR+K10_DSCORE_MCQ+WHC_HIP_CM_COM+
                CCT_F2+ TRA_DSTATUS_MCQ.binary+
                FAL_NMBR_NB_MCQ+
                SEX_ASK+ PA2_WALK_MCQ+ PA2_SSPRTHR_MCQ + VA_ETDRS_BOTH_RSLT_COM+  MED_USE_MCQ+ CCC_URIINC,
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
  models = list(LR = logistic, XG = xgboost, LR = logistic, NN = nnet_spec, KNN = knn)) 

#XG = xgboost, LR = logistic, NN = nnet_spec, KNN = knn

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

LR.best.pred<-best_models %>% filter(wflow_id == "recipe_LR") %>% collect_predictions() %>% filter(.config == "Preprocessor1_Model17")  

### extract performance metrics ####
y<-LR.best.pred%>%
  dplyr::select(.pred_0, .pred_1,.pred_class, Binary_IADL.ADL_F2)

conf_mat(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
accuracy(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
sens(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
specificity(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)
f_meas(y, truth = Binary_IADL.ADL_F2, estimate = .pred_class)

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
optimal_threshold<- 1-0.02800519

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
  fit(data = CLSA_train)
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
predicted_classes <- ifelse(predictions$.pred_0 > 1-0.9713455, "0", "1")
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
print(coefficients_tidy, n=54)

# model summary on full dataset ####
# update recipe to use full dataset
step.rec <- recipe(Binary_IADL.ADL_F2~HUP_FREE_MCQ+
                     GEN_DHDI+RET_RTRD+WLK_TIME_COM+
                     CR1_FRHC+PA2_SSPRTHR_MCQ+HGT_HEIGHT_M_COM+PER_DSCR_CON_MCQ+
                     COG_REYI_SCORE+COG_REYII_SCORE+
                     WLK_TIME_COM+MSDYY_MFS+GS_EXAM_AVG_COM+ADL_DCLS+PA2_WALK_MCQ+
                     AGE_NMBR+K10_DSCORE_MCQ+WHC_HIP_CM_COM+
                     CCT_F2+ TRA_DSTATUS_MCQ.binary+
                     FAL_NMBR_NB_MCQ+
                     SEX_ASK+ PA2_WALK_MCQ+ PA2_SSPRTHR_MCQ+ VA_ETDRS_BOTH_RSLT_COM+  MED_USE_MCQ+ CCC_URIINC,
                   data = CLSA.baseline.FU2) %>%
  step_impute_bag(all_predictors(), seed_val = 123) %>%
  #step_impute_bag(all_outcomes(), seed_val = 123) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)%>% 
  step_normalize(all_predictors())
# update lr_workflow
lr_workflow <- workflow() %>%
  add_model(glmnet_spec) %>%
  add_recipe(step.rec)
# fit the final model to the training data
final_fit <- lr_workflow %>%
  fit(data = CLSA.baseline.FU2)
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
print(coefficients_tidy, n=54)

# confidence intervals for odds ratios ####


# Assuming `X` and `y` are your predictors and response matrix from the full dataset
X <- model.matrix(Binary_IADL.ADL_F2 ~ ., data = CLSA.baseline.FU2)[, -1]  # remove intercept
y <- full_dataset$Binary_IADL.ADL_F2

# Define the function to fit the model
bootstrap_glmnet <- function(data, indices) {
  data_boot <- data[indices,]  # Resample the data
  fit <- glmnet(x = X[indices, ], y = y[indices], family = "binomial")
  coef(fit, s = "lambda.min")  # Extract coefficients at lambda.min
}

# Set up your full dataset (or however you've structured your inputs)
full_data <- data.frame(X, y = as.numeric(y) - 1)  # Convert to binary numeric

# Perform the bootstrap
set.seed(123)  # for reproducibility
boot_results <- boot(data = full_data, statistic = bootstrap_glmnet, R = 1000)  # R is the number of bootstrap replicates

# Calculate confidence intervals from the bootstrap results
boot_ci <- boot.ci(boot_results, type = "bca")  # using bias-corrected and accelerated (BCa) interval

print(boot_ci)

# export table to csv

# descriptive table (full sample)####

# note PA2_SSPRTHR_MCQ included twice in model; oops; also skip logic Q; need to omit?
# walk time WLK_TIME_COM included twice, fuck...so is PA2_WALK_MCQ

continuous_vars<-c("COG_REYI_SCORE","COG_REYII_SCORE","WHC_HIP_CM_COM",
"FAL_NMBR_NB_MCQ", "AGE_NMBR","K10_DSCORE_MCQ", "WLK_TIME_COM", "MSDYY_MFS","GS_EXAM_AVG_COM", "HGT_HEIGHT_M_COM","PER_DSCR_CON_MCQ",
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
# medication use
CLSA.baseline.FU2 %>% group_by(MED_USE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# descriptive table (high ADL)####

# note PA2_SSPRTHR_MCQ included twice in model; oops; also skip logic Q; need to omit?
# walk time WLK_TIME_COM included twice, fuck...so is PA2_WALK_MCQ

CLSA.baseline.FU2.HighADL<-CLSA.baseline.FU2 %>% filter(Binary_IADL.ADL_F2==0)

continuous_vars<-c("COG_REYI_SCORE","COG_REYII_SCORE","WHC_HIP_CM_COM",
                   "FAL_NMBR_NB_MCQ", "AGE_NMBR","K10_DSCORE_MCQ", "WLK_TIME_COM", "MSDYY_MFS","GS_EXAM_AVG_COM", "HGT_HEIGHT_M_COM","PER_DSCR_CON_MCQ",
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
# strenuous sports
CLSA.baseline.FU2.HighADL %>% group_by(PA2_SSPRTHR_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# medication use
CLSA.baseline.FU2.HighADL %>% group_by(MED_USE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# descriptive table (high ADL)####

# note PA2_SSPRTHR_MCQ included twice in model; oops; also skip logic Q; need to omit?
# walk time WLK_TIME_COM included twice, fuck...so is PA2_WALK_MCQ

CLSA.baseline.FU2.LowADL<-CLSA.baseline.FU2 %>% filter(Binary_IADL.ADL_F2==1)

continuous_vars<-c("COG_REYI_SCORE","COG_REYII_SCORE","WHC_HIP_CM_COM",
                   "FAL_NMBR_NB_MCQ", "AGE_NMBR","K10_DSCORE_MCQ", "WLK_TIME_COM", "MSDYY_MFS","GS_EXAM_AVG_COM", "HGT_HEIGHT_M_COM","PER_DSCR_CON_MCQ",
                   "VA_ETDRS_BOTH_RSLT_COM")

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
