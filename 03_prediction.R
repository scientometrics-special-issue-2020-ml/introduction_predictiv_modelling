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
data <- readRDS("../temp/data_full.rds")

############################################################################
# training setup
############################################################################

data %<>% slice_sample(n = 1000000)
             
# Normalize variables by technology field/year cohort
data %<>%
  group_by(tech_field_main, appln_filing_year) %>%
  mutate(across(c(tech_field_n, scope, family_size_docdb, family_size_inpadoc, cit_bwd, cit_nlp, claims, originality, nb_applicants, nb_inventors), scale)) %>%
  ungroup()

# Factors
data %<>%
  mutate(tech_field_main = tech_field_main %>% factor())

# Train & test split
data_split <- data %>% 
  select(-appln_filing_year, -cit_fwd5) %>% # Deselect what we dont want
  initial_split(prop = 0.75, strata = breakthrough01)

data_train <- data_split  %>%  training()
data_test <- data_split %>% testing()
rm(data_split)

# Create recipe -> new package to make preprocessing really easy
data_recipe_ <- data_train %>%
  rename(y = breakthrough01) %>%
  select(-breakthrough25, -breakthrough50) %>%
  #
  recipe(y ~.) %>%
  step_dummy(tech_field_main) %>% # , source
  step_zv(all_predictors()) %>% # get rid of zero variance vars
  prep()


# finalize reciepe preperation
data_train01 <- recipe_01 %>% juice()
data_test01 <- recipe_01 %>% bake(data_test %>% select(-breakthrough25, -breakthrough50))

data_train25 <- recipe_25 %>% juice()
data_test25 <- recipe_25 %>% bake(data_test %>% select(-breakthrough01, -breakthrough50))

data_train50 <- recipe_50 %>% juice()
data_test50 <- recipe_50 %>% bake(data_test %>% select(-breakthrough01, -breakthrough25))

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
  set_engine('glm', family = binomial) 

# Decision tree
model_dt <- decision_tree(mode = 'classification',
                          cost_complexity = tune(),
                          tree_depth = tune(), 
                          min_n = tune()) %>%
  set_engine('rpart') 

# Random Forest
model_rf <- rand_forest(mode = 'regression',
                        trees = 100,
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine('ranger', importance = 'impurity') 

# XGBoost
model_xg <- boost_tree(mode = 'classification', 
                       trees = 100,
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
# Hyperparameter Tuning
############################################################################

### Resamples
data_resample01 <- data_train %>% 
  bootstraps(data_train, strata = y, times = 5)

data_resample25 <- bootstraps(data_train25, strata = y, times = 5)
data_resample50 <- bootstraps(data_train50, strata = y, times = 5)

### Parallelprocessing
all_cores <- parallel::detectCores(logical = FALSE)

library(doParallel)
cl <- makePSOCKcluster(all_cores - 1)
registerDoParallel(cl)

# Breakthrough50
tune_el50 <- model_en %>% tune_grid(resamples = data_resample, grid = 10)










############################################################################
# Workflow setup
############################################################################

# General reciepes
workflow_01 <- workflow() %>% add_recipe(recipe_01) 
workflow_25 <- workflow() %>% add_recipe(recipe_25) 
workflow_50 <- workflow() %>% add_recipe(recipe_50) 

# breakthrough01
workflow_01_lg <- workflow_01 %>% add_model(model_lg)
workflow_01_en <- workflow_01 %>% add_model(model_en)
workflow_01_dt <- workflow_01 %>% add_model(model_dt)
workflow_01_rf <- workflow_01 %>% add_model(model_rf)
workflow_01_xg <- workflow_01 %>% add_model(model_xg)
workflow_01_nn <- workflow_01 %>% add_model(model_nn)

# breakthrough25
workflow_25_lg <- workflow_25 %>% add_model(model_lg)
workflow_25_en <- workflow_25 %>% add_model(model_en)
workflow_25_dt <- workflow_25 %>% add_model(model_dt)
workflow_25_rf <- workflow_25 %>% add_model(model_rf)
workflow_25_xg <- workflow_25 %>% add_model(model_xg)
workflow_25_nn <- workflow_25 %>% add_model(model_nn)

# breakthrough50
workflow_50_lg <- workflow_50 %>% add_model(model_lg)
workflow_50_en <- workflow_50 %>% add_model(model_en)
workflow_50_dt <- workflow_50 %>% add_model(model_dt)
workflow_50_rf <- workflow_50 %>% add_model(model_rf)
workflow_50_xg <- workflow_50 %>% add_model(model_xg)
workflow_50_nn <- workflow_50 %>% add_model(model_nn)