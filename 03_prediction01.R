############################################################################
# Preamble
############################################################################
rm(list=ls())
set.seed(1337)

### Load packages  Standard
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(magrittr)

### Load extra packages
library(tidymodels)

############################################################################
# Load data
############################################################################

rm(list=ls())
data <- readRDS("../temp/data_pred.rds")

############################################################################
# training setup
############################################################################

# Prepare for prediction (right outcome var)
data  %<>% 
  rename(y = breakthrough01) %>%
  select(-starts_with('breakthrough')) 

# # Idiotic in between step because of some issues later
# data  %<>%
#   recipe(y ~.) %>%
#   step_dummy(tech_field, one_hot = TRUE, preserve = FALSE) %>% 
#   prep() %>% juice()
data %<>% select(-tech_field)

# Sampling for performance issues
data %<>% initial_split(prop = 0.05, strata = y) %>% training()

# Train & test split
data_split <- data %>% 
  initial_split(prop = 0.75, strata = y)

data_train <- data_split  %>%  training()
data_test <- data_split %>% testing()
#rm(data, data_split)

# Create recipe -> ackage to make preprocessing really easy
data_recipe <- data_train %>%
  recipe(y ~.) %>%
  step_scale(nb_applicants, nb_inventors, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness) %>%
  step_center(nb_applicants, nb_inventors, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness) %>%
  prep()

############################################################################
# Model setup
############################################################################

# Null
model_00 <- null_model(mode = "classification")

# Logistic
model_lg <- logistic_reg(mode = 'classification') %>% 
  set_engine('glm', family = binomial) 

# Elastic net
model_en <- logistic_reg(mode = 'classification', 
                         mixture = tune(), 
                         penalty = tune()) %>%
  set_engine('glmnet', family = binomial) 

# Decision tree
model_dt <- decision_tree(mode = 'classification',
                          cost_complexity = tune(),
                          tree_depth = tune(), 
                          min_n = tune()) %>%
  set_engine('rpart') 

# Random Forest
model_rf <- rand_forest(mode = 'classification',
                        trees = 25,
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine('ranger', importance = 'impurity') 

# XGBoost
model_xg <- boost_tree(mode = 'classification', 
                       trees = 25,
                       mtry = tune(), 
                       min_n = tune(), 
                       tree_depth = tune(), 
                       learn_rate = tune()) %>%
  set_engine('xgboost') 

# neural net
model_nn <- mlp(mode = 'classification',
                hidden_units = tune(),
                penalty = tune(),
                epochs = tune()) %>% 
  set_engine('nnet')


############################################################################
# Workflow setup
############################################################################
# General

workflow_general <- workflow() %>% add_recipe(data_recipe) 

# models
workflow_lg <- workflow_general %>% add_model(model_lg)
workflow_en <- workflow_general %>% add_model(model_en)
workflow_dt <- workflow_general %>% add_model(model_dt)
workflow_rf <- workflow_general %>% add_model(model_rf)
workflow_xg <- workflow_general %>% add_model(model_xg)
workflow_nn <- workflow_general %>% add_model(model_nn)

############################################################################
# Hyperparameter Tuning
############################################################################

### Resamples
data_resample <- data_train %>% vfold_cv(v = 3, repeats = 1, strata = y)

# Breakthrough50
tune_en <- workflow_en %>% tune_grid(resamples = data_resample, grid = 5)
tune_dt <- workflow_dt %>% tune_grid(resamples = data_resample, grid = 5)
tune_rf <- workflow_rf %>% tune_grid(resamples = data_resample, grid = 5)
tune_xg <- workflow_xg %>% tune_grid(resamples = data_resample, grid = 5)
tune_nn <- workflow_nn %>% tune_grid(resamples = data_resample, grid = 5)

save.image('../temp/prediction01.RData')

############################################################################
# Final prediction
############################################################################
rm(list=ls())
load('../temp/prediction01.RData')

y <- 'breakthrough01'

best_param_en <- tune_en %>% select_best(metric = 'roc_auc')
best_param_dt <- tune_dt %>% select_best(metric = 'roc_auc')
best_param_rf <- tune_rf %>% select_best(metric = 'roc_auc')
best_param_xg <- tune_xg %>% select_best(metric = 'roc_auc')
best_param_nn <- tune_nn %>% select_best(metric = 'roc_auc')

# Finalize workflow
workflow_final_en <- workflow_en %>% finalize_workflow(parameters = best_param_en)
workflow_final_dt <- workflow_dt %>% finalize_workflow(parameters = best_param_dt)
workflow_final_rf <- workflow_rf %>% finalize_workflow(parameters = best_param_rf)
workflow_final_xg <- workflow_xg %>% finalize_workflow(parameters = best_param_xg)
workflow_final_nn <- workflow_nn %>% finalize_workflow(parameters = best_param_nn)

# Fit on train data
fit_lg <- workflow_lg %>% fit(data_train)
fit_en <- workflow_final_en %>% fit(data_train)
fit_dt <- workflow_final_dt %>% fit(data_train)
fit_rf <- workflow_final_rf %>% fit(data_train)
fit_xg <- workflow_final_xg %>% fit(data_train)
fit_nn <- workflow_final_nn %>% fit(data_train)


############################################################################
# evaluation
############################################################################

### Evaluation Training
pred_train_collected <- tibble(
  truth = data_train %>% pull(y) %>% as.factor(),
  #base = mean(truth),
  lg = fit_lg %>% predict(new_data = data_train) %>% pull(.pred_class),
  en = fit_en %>% predict(new_data = data_train) %>% pull(.pred_class),
  dt = fit_dt %>% predict(new_data = data_train) %>% pull(.pred_class),
  rf = fit_rf %>% predict(new_data = data_train) %>% pull(.pred_class),
  xg = fit_xg %>% predict(new_data = data_train) %>% pull(.pred_class),
  nn = fit_nn %>% predict(new_data = data_train) %>% pull(.pred_class)
) %>% 
  pivot_longer(cols = -truth,
               names_to = 'model',
               values_to = 'pred') 

prob_train_collected <- tibble(
  truth = data_train %>% pull(y) %>% as.factor(),
  lg = fit_lg %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE),
  en = fit_lg %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE),
  dt = fit_dt %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE),
  rf = fit_rf %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE),
  xg = fit_xg %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE),
  nn = fit_nn %>% predict(new_data = data_train, type = 'prob')  %>% pull(.pred_TRUE)
) %>% 
  pivot_longer(cols = -truth,
               names_to = 'model',
               values_to = 'prob') 

results_train <- pred_train_collected %>%
  bind_cols(prob_train_collected %>% select(prob)) %>%
  mutate(outcome = y,
         eval = 'train') %>%
  relocate(eval, model, outcome, truth, pred, prob)

### Last fit
# Todo: Keep the initial_split object that we can use the last_fit function

### Evaluation Testing
pred_test_collected <- tibble(
  truth = data_test %>% pull(y) %>% as.factor(),
  lg = fit_lg %>% predict(new_data = data_test) %>% pull(.pred_class),
  en = fit_en %>% predict(new_data = data_test) %>% pull(.pred_class),
  dt = fit_dt %>% predict(new_data = data_test) %>% pull(.pred_class),
  rf = fit_rf %>% predict(new_data = data_test) %>% pull(.pred_class),
  xg = fit_xg %>% predict(new_data = data_test) %>% pull(.pred_class),
  nn = fit_nn %>% predict(new_data = data_test) %>% pull(.pred_class),
) %>% 
  pivot_longer(cols = -truth,
               names_to = 'model',
               values_to = 'pred') 

prob_test_collected <- tibble(
  truth = data_test %>% pull(y) %>% as.factor(),
  lg = fit_lg %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE),
  en = fit_lg %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE),
  dt = fit_dt %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE),
  rf = fit_rf %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE),
  xg = fit_xg %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE),
  nn = fit_nn %>% predict(new_data = data_test, type = 'prob')  %>% pull(.pred_TRUE)
) %>% 
  pivot_longer(cols = -truth,
               names_to = 'model',
               values_to = 'prob') 

results_test <- pred_test_collected %>%
  bind_cols(prob_test_collected %>% select(prob)) %>%
  mutate(outcome = y,
         eval = 'test') %>%
  relocate(eval, model, outcome, truth, pred, prob)

## combine all and save
resuts_collected <- results_train %>%
  bind_rows(results_test)

resuts_collected %>% saveRDS('../temp/resuts_collected01.rds')
