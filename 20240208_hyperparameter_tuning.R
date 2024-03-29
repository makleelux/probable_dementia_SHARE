#############################################################
#                                                           #
#         Data Merging and Cleaning                         #
#                                                           #
#                                                           #
#                                           maklee 20220810 #
#############################################################

source("./scripts/20240121_packages.R")
source("./scripts/20240210_func.R")
load("./data/probdem_dat_complete.RData")

cat('------------ loading data \n')

set.seed(123)

# Models

model_rf <- 
  rand_forest(
    mtry = tune(),         # Randomly Selected Predictors (type: integer)
    trees = tune(),        # Trees (type: integer, default: 500L)
    min_n = tune()) %>%    # Minimal Node Size (type: integer)
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

model_xgboost <- 
  boost_tree(
    trees = tune(),           # Trees (type: integer, default: 15L)
    tree_depth = tune(),      # Tree Depth (type: integer, default: 6L)
    min_n = tune(),           # Minimal Node Size (type: integer, default: 1L)
    loss_reduction = tune(),  # Minimum Loss Reduction (type: double, default: 0.0)
    sample_size = tune(),     # Proportion Observations Sampled (type: double, default: 1.0)
    mtry = tune(),            # Randomly Selected Predictors (type: integer)   
    learn_rate = tune()       # Learning Rate (type: double, default: 0.3)
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow

formula_iv = as.formula("dementia_dbl ~ cf008tot + cf016tot + age + is_female + isced_upper_secondary + isced_lower_secondary + 
                        iadl_meal + iadl_groceries + iadl_phone + iadl_medi + iadl_money + iadl_map + iadl_leaving + iadl_laundry + iadl_garden + 
                        adl_dressing + adl_walking + adl_bathing + adl_eating + adl_outofbed + adl_toilet + 
                        reading_help + willing_bad.at.some.time + proxy_partner + proxy_relatives + proxy_helper.other + askclarification + understood")

wkfl_rf_iv <- workflow() %>% add_formula(formula_iv) %>% add_model(model_rf)
wkfl_wgboost_iv <- workflow() %>% add_formula(formula_iv) %>% add_model(model_xgboost)


# Grid of hyperparameters

grid_rf_iv <- 
  grid_max_entropy(        
    mtry(range = c(0, 28)), # max n col 
    trees(range = c(500, 2000)),
    min_n(),
    size = 30) 

grid_xgb_iv <- 
  grid_max_entropy(
    trees(range = c(500, 2000)),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    mtry(range = c(0, 28)), # max n col 
    learn_rate(),
    size = 30
  )

cat('------------ create cv folds \n')

cv_folds <- vfold_cv(train_dat_baked %>% mutate(dementia_dbl = as_factor(dementia_dbl)), v = 5, strata = dementia)
cv_folds_smote <- vfold_cv(train_dat_baked_smote %>% mutate(dementia_dbl = as_factor(dementia_dbl)), v = 5, strata = dementia)
cv_folds_down <- vfold_cv(train_dat_baked_down %>% mutate(dementia_dbl = as_factor(dementia_dbl)), v = 5, strata = dementia)

cat('------------ tuning \n')
cat('--------------- tuning random forest (split) \n')

# Tuning
rf_fit_iv <- tune_grid(
  wkfl_rf_iv,
  resamples = cv_folds,
  grid = grid_rf_iv,
  control = control_grid(verbose = TRUE) 
)

cat('--------------- tuning random forest (smote) \n')

rf_fit_smote_iv <- tune_grid(
  wkfl_rf_iv,
  resamples = cv_folds_smote,
  grid = grid_rf_iv,
  control = control_grid(verbose = TRUE) 
)

cat('--------------- tuning random forest (down) \n')

rf_fit_down_iv <- tune_grid(
  wkfl_rf_iv,
  resamples = cv_folds_down,
  grid = grid_rf_iv,
  control = control_grid(verbose = TRUE) 
)

cat('--------------- tuning xgb (split) \n')

xgb_fit_iv <- tune_grid(
  wkfl_wgboost_iv,
  resamples = cv_folds,
  grid = grid_xgb_iv,
  control = control_grid(verbose = TRUE) 
)

cat('--------------- tuning xgb (smote) \n')

xgb_fit_smote_iv <- tune_grid(
  wkfl_wgboost_iv,
  resamples = cv_folds_smote,
  grid = grid_xgb_iv,
  control = control_grid(verbose = TRUE) 
)

cat('--------------- tuning xgb (down) \n')

xgb_fit_down_iv <- tune_grid(
  wkfl_wgboost_iv,
  resamples = cv_folds_down,
  grid = grid_xgb_iv,
  control = control_grid(verbose = TRUE) 
)

save(rf_fit_iv, file = "./data/rf_fit_iv.RData")
save(rf_fit_smote_iv, file = "./data/rf_fit_smote_iv.RData")
save(rf_fit_down_iv, file = "./data/rf_fit_down_iv.RData")
save(xgb_fit_iv, file = "./data/xgb_fit_iv.RData")
save(xgb_fit_smote_iv, file = "./data/xgb_fit_smote_iv.RData")
save(xgb_fit_down_iv, file = "./data/xgb_fit_down_iv.RData")


# Inspect tuning 

#rf_fit_iv
#collect_metrics(rf_fit_iv)
#autoplot(rf_fit_iv)
#show_best(rf_fit_iv)
#select_best(rf_fit_iv)


