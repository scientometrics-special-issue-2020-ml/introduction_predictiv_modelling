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

data <- readRDS("../input/pat_data_main.rds")

data_oecd <-read_delim('../input/202001_OECD_PATENT_QUALITY_USPTO_INDIC.txt', delim = '|')

data_oecd %<>%
  select(appln_id, fwd_cits5, tech_field, many_field, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness) %>%
  mutate(originality = originality %>% round(3),
         radicalness = radicalness %>% round(3))

############################################################################
# Construct final dataset
############################################################################

# Do some filtering
data %<>%
  filter(appln_filing_year  >= 2000 & appln_filing_year  <= 2016) %>%
  filter(appln_auth == 'US') 

# Only use the variables we need
data %<>% select(appln_id, appln_filing_year, nb_applicants, nb_inventors)

# Join both
data %<>% inner_join(data_oecd, by = 'appln_id')
rm(data_oecd)

# Generate breakthrough variables
data %<>%
  mutate(fwd_cits5 = ifelse(fwd_cits5 == 0, NA, fwd_cits5)) %>%
  group_by(appln_filing_year) %>%
  mutate(breakthrough01 = fwd_cits5 >= quantile(fwd_cits5, probs = 0.99, na.rm = TRUE),
         breakthrough25 = fwd_cits5 >= quantile(fwd_cits5, probs = 0.75, na.rm = TRUE), 
         breakthrough50 = fwd_cits5 >= quantile(fwd_cits5, probs = 0.50, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-fwd_cits5)

# Fill out missing values
data %<>%
  replace_na(list(breakthrough01 = FALSE, 
                  breakthrough25 = FALSE,
                  breakthrough50 = FALSE,
                  nb_applicants = 1, 
                  nb_inventors = 1, 
                  many_field = 1,
                  family_size = 1,
                  patent_scope = 1, 
                  bwd_cits = 0, 
                  nlp_cits = 0, 
                  claims = 0,
                  originality = 0, 
                  radicalness = 0
  )) 

# data %>% skimr::skim()

### Save for later
data %>% saveRDS("../temp/data_desc.RDS")

############################################################################
# Preprocessing for prediction
############################################################################

# Factorize outcomes
data %<>%
  mutate(breakthrough01 = breakthrough01 %>% factor(),
         breakthrough25 = breakthrough25 %>% factor(),
         breakthrough50 = breakthrough50 %>% factor())

# Factors of categories
data %<>% mutate(tech_field = tech_field %>% factor())

#Normalize variables by technology field/year cohort
norm_fun <- function(x){x / mean(x, na.rm = FALSE)}

data %<>%
  group_by(tech_field, appln_filing_year) %>%
  mutate(across(c(nb_applicants, nb_inventors, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness), norm_fun)) %>%
  ungroup()

# keep only the vars we need
data %<>%
  select(-appln_id, -appln_filing_year) %>%
  relocate(breakthrough01, breakthrough25, breakthrough50, tech_field)

data %>% saveRDS("../temp/data_pred.RDS")

############################################################################
# Preprocessing for Python Autoencoder (Roman)
############################################################################

# Train & test split
data_split <- data %>% 
  initial_split(prop = 0.75, strata = breakthrough01)

data_train <- data_split  %>%  training()
data_test <- data_split %>% testing()
rm(data_split)

# Create recipe -> ackage to make preprocessing really easy
data_recipe <- data_train %>%
  recipe(breakthrough01 ~.) %>%
  step_scale(nb_applicants, nb_inventors, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness) %>%
  step_center(nb_applicants, nb_inventors, family_size, patent_scope, bwd_cits, npl_cits, claims, originality, radicalness) %>%
  prep()

data_train_prep <-  data_recipe %>% juice()
data_test_prep <-  data_recipe %>% bake(data_test)

# save 
library(feather)
data_train_prep %>% write_feather("../temp/data_train_PY.feather")
data_test_prep %>% write_feather("../temp/data_test_PY.feather")
