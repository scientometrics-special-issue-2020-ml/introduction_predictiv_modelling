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

#########################################################################################################
# DLoad data - full
#########################################################################################################

rm(list=ls())
data <- readRDS("../temp/data_desc.rds")

data %<>% relocate(breakthrough50, breakthrough25, breakthrough01)

### Descriptives
require(stargazer)
data %>%
  as.data.frame() %>%
  stargazer(out = "output/desc_bt.tex",
            title = "Descriptive Statistics: USTPO Patents 2000-2016",
            label = "tab:desc_bt",
            align = FALSE,
            summary = TRUE,
            iqr = FALSE)

############################################################################
# Load data restricted
############################################################################

# Sampling for performance issues
data %<>% initial_split(prop = 0.1, strata = breakthrough01) %>% training()


### Conditional variable distribution
require(ggridges)
p50 <- data %>%
  select(-appln_id, -appln_filing_year, tech_field) %>%
  rename(breakthrough = breakthrough50) %>%
  select(-breakthrough25, -breakthrough01) %>%
  mutate(breakthrough = factor(breakthrough, labels = c("no", "yes"))) %>%
  gather(variable, value, -breakthrough) %>%
  ggplot(aes(y = as.factor(variable), 
             fill =  breakthrough, 
             x = percent_rank(value)) ) +
  geom_density_ridges(alpha = 0.75) +
  labs(title = 'breakthrough50', y = NULL, x = NULL)

p25 <- data %>%
  select(-appln_id, -appln_filing_year, tech_field) %>%
  rename(breakthrough = breakthrough25) %>%
  select(-breakthrough50, -breakthrough01) %>%
  mutate(breakthrough = factor(breakthrough, labels = c("no", "yes"))) %>%
  gather(variable, value, -breakthrough) %>%
  ggplot(aes(y = as.factor(variable), 
             fill =  breakthrough, 
             x = percent_rank(value)) ) +
  geom_density_ridges(alpha = 0.75) +
  labs(title = 'breakthrough25', y = NULL, x = NULL)

p01 <- data %>%
  select(-appln_id, -appln_filing_year, tech_field) %>%
  rename(breakthrough = breakthrough01) %>%
  select(-breakthrough50, -breakthrough25) %>%
  mutate(breakthrough = factor(breakthrough, labels = c("no", "yes"))) %>%
  gather(variable, value, -breakthrough) %>%
  ggplot(aes(y = as.factor(variable), 
             fill =  breakthrough, 
             x = percent_rank(value)) ) +
  geom_density_ridges(alpha = 0.75) +
  labs(title = 'breakthrough01', y = NULL, x = NULL)

p50 / p25 / p01 +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave('../output/fig_var_dist.pdf', width = 20,  height = 25, units = "cm")
rm(p50, p25, p01)

### correlation matrix
data %>%
  select(-appln_id, -appln_filing_year, tech_field) %>%
  GGally::ggpairs(aes(alpha = 0.3), 
                  ggtheme = theme_gray())  
ggsave('../output/corr_matrix.png', width = 20,  height = 20, units = "cm")

############################################################################
# Hyperparameter tuning
############################################################################
rm(list=ls())

### Load data
set.seed(1337)
load('../temp/prediction50.RData')
tune_en50 <- tune_en
tune_dt50 <- tune_dt
tune_rf50 <- tune_rf
tune_xg50 <- tune_xg
tune_nn50 <- tune_nn

rm(list=ls()[! ls() %in% c('tune_en50', 'tune_dt50', 'tune_rf50', 'tune_xg50', 'tune_nn50',
                           'tune_en25', 'tune_dt25', 'tune_rf25', 'tune_xg25', 'tune_nn25',
                           'tune_en01', 'tune_dt01', 'tune_rf01', 'tune_xg01', 'tune_nn01')])


load('../temp/prediction25.RData')
tune_en25 <- tune_en
tune_dt25 <- tune_dt
tune_rf25 <- tune_rf
tune_xg25 <- tune_xg
tune_nn25 <- tune_nn

rm(list=ls()[! ls() %in% c('tune_en50', 'tune_dt50', 'tune_rf50', 'tune_xg50', 'tune_nn50',
                           'tune_en25', 'tune_dt25', 'tune_rf25', 'tune_xg25', 'tune_nn25',
                           'tune_en01', 'tune_dt01', 'tune_rf01', 'tune_xg01', 'tune_nn01')])

load('../temp/prediction01.RData')
tune_en01 <- tune_en
tune_dt01 <- tune_dt
tune_rf01 <- tune_rf
tune_xg01 <- tune_xg
tune_nn01 <- tune_nn

rm(list=ls()[! ls() %in% c('tune_en50', 'tune_dt50', 'tune_rf50', 'tune_xg50', 'tune_nn50',
                           'tune_en25', 'tune_dt25', 'tune_rf25', 'tune_xg25', 'tune_nn25',
                           'tune_en01', 'tune_dt01', 'tune_rf01', 'tune_xg01', 'tune_nn01')])

### Tuning plots
library(patchwork)
p_en50 <- tune_en50 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough50')
p_en25 <- tune_en25 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough25')
p_en01 <- tune_en01 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough01')
p_en50 + p_en25 + p_en01
ggsave('../output/fig_tune_en.eps', width = 20,  height = 7.5, units = "cm")

p_dt50 <- tune_dt50 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough50')
p_dt25 <- tune_dt25 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough25')
p_dt01 <- tune_dt01 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough01')
p_dt50 + p_dt25 + p_dt01
ggsave('../output/fig_tune_dt.eps', width = 20,  height = 7.5, units = "cm")

p_rf50 <- tune_rf50 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough50')
p_rf25 <- tune_rf25 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough25')
p_rf01 <- tune_rf01 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough01')
p_rf50 + p_rf25 + p_rf01
ggsave('../output/fig_tune_rf.eps', width = 20,  height = 7.5, units = "cm")

p_xg50 <- tune_xg50 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough50')
p_xg25 <- tune_xg25 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough25')
p_xg01 <- tune_xg01 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough01')
p_xg50 + p_xg25 + p_xg01
ggsave('../output/fig_tune_xg.pdf', width = 20,  height = 7.5, units = "cm")

p_nn50 <- tune_nn50 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough50')
p_nn25 <- tune_nn25 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough25')
p_nn01 <- tune_nn01 %>% autoplot(metric = 'roc_auc') + labs(title = 'Breakthrough01')
p_nn50 + p_nn25 + p_nn01
ggsave('../output/fig_tune_nn.pdf', width = 20,  height = 7.5, units = "cm")

rm(list=ls()[! ls() %in% c('tune_en50', 'tune_dt50', 'tune_rf50', 'tune_xg50', 'tune_nn50',
                           'tune_en25', 'tune_dt25', 'tune_rf25', 'tune_xg25', 'tune_nn25',
                           'tune_en01', 'tune_dt01', 'tune_rf01', 'tune_xg01', 'tune_nn01')])



############################################################################
# Final prediction
############################################################################
rm(list=ls())

results_collected <- readRDS('../temp/resuts_collected01.rds') %>%
  bind_rows(readRDS('../temp/resuts_collected25.rds')) %>%
  bind_rows(readRDS('../temp/resuts_collected50.rds'))

################ test

multimetric <- metric_set(accuracy, bal_accuracy, kap, f_meas, sens, yardstick::spec, precision, recall, ppv, npv)

results_summary <- results_collected %>%
  group_by(eval, model, outcome) %>%
  multimetric(truth = truth, estimate = pred)  %>%
  select(-.estimator) %>%
  rename(metric = .metric,
         value = .estimate) %>%
  mutate(value = value %>% round(3))

# # Note: Old way of doing it with confmat
# results_summary <- results_collected %>%
#   group_by(eval, model, outcome) %>%
#   conf_mat(truth = truth, estimate = pred) %>%
#   rename(cmat = conf_mat) %>%
#   rowwise() %>%
#   mutate(mat_sum = list(cmat %>% summary())) %>%  
#   ungroup() %>% 
#   unnest(mat_sum) %>%
#   select(-cmat, -.estimator) %>%
#   rename(metric = .metric,
#          value = .estimate) %>%
#   mutate(value = value %>% round(3))


# Note: Something not working....
#results_collected %<>%
#  mutate(prob = 1 - prob)
  
results_roc <- results_collected %>%
 group_by(eval, model, outcome) %>%
 roc_auc(truth = truth, prob) %>%
  select(-.estimator) %>%
  rename(metric = .metric,
         value = .estimate) %>%
  mutate(value = value %>% round(3))

results_summary %<>%
  bind_rows(results_roc)
  
### Summary
library(stargazer)
result_summary_wide_train <- results_summary %>%
  filter(eval == 'train') %>%
  select(-eval) %>%
  arrange(desc(outcome)) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  relocate(outcome, metric, lg, en, dt, rf, xg, nn)

# to c&p in paper
result_summary_wide_train  %>%
  filter(outcome == 'breakthrough50') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)

result_summary_wide_train  %>%
  filter(outcome == 'breakthrough25') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)

result_summary_wide_train  %>%
  filter(outcome == 'breakthrough01') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)


### Summary
library(stargazer)
result_summary_wide_test <- results_summary %>%
  filter(eval == 'test') %>%
  select(-eval) %>%
  arrange(desc(outcome)) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  relocate(outcome, metric, lg, en, dt, rf, xg, nn)

# to c&p in paper
result_summary_wide_test  %>%
  filter(outcome == 'breakthrough50') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)

result_summary_wide_test  %>%
  filter(outcome == 'breakthrough25') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)

result_summary_wide_test  %>%
  filter(outcome == 'breakthrough01') %>%
  select(-outcome) %>%
  stargazer(summary=FALSE, rownames=FALSE)

########## Other stuff
        
#pred_lg %>% conf_mat(truth = truth, estimate = pred) %>% autoplot(type = 'heatmap')
#pred_lg %>% conf_mat(truth = truth, estimate = pred) %>% summary()

############################################################################
# Romans Autoencoder
############################################################################
library(feather)

encoder_results <- read_feather('../input/breakthrough01.feather') %>%
  mutate(outcome = 'breakthrough01') %>%
  bind_rows(read_feather('../input/breakthrough25.feather') %>% mutate(outcome = 'breakthrough25')) %>%
  bind_rows(read_feather('../input/breakthrough50.feather') %>% mutate(outcome = 'breakthrough50')) %>%
  select(-index) %>%
  rename(truth = true_class) %>%
  relocate(outcome, truth, mse, euclidian)

### Plot
encoder_results %>%
  pivot_longer(cols = c(mse, euclidian), names_to = 'measure') %>%
  ggplot(aes(x = value, col = truth, fill = truth)) +
  geom_density(alpha = 0.5) +
  #lims(x = c('0', '1'))
  scale_x_log10() + 
  facet_grid(cols = vars(outcome), rows = vars(measure), scales = 'free') 

ggsave('../output/fig_error_autoencoder.eps', width = 20,  height = 15, units = "cm")

# Setting cuttoff
cuttoff <- 7.5

encoder_results %<>%
  mutate(pred = euclidian >= cuttoff) %>%
  mutate(prob = euclidian - min(euclidian) / (max(euclidian) - min(euclidian)) ) %>%
  relocate(outcome, truth, pred) %>%
  mutate(
    truth = truth %>% factor(),
    pred = pred %>% factor()
  )

### Summary
multimetric <- metric_set(accuracy, bal_accuracy, kap, f_meas, sens, yardstick::spec, precision, recall, ppv, npv)

encoder_summary <- encoder_results %>%
  group_by(outcome) %>%
  multimetric(truth = truth, estimate = pred)  %>%
  select(-.estimator) %>%
  rename(metric = .metric,
         value = .estimate) %>%
  mutate(value = value %>% round(3))


encoder_roc <- encoder_results %>%
  group_by(outcome) %>%
  roc_auc(truth = truth, prob) %>%
  select(-.estimator) %>%
  rename(metric = .metric,
         value = .estimate) %>%
  mutate(value = value %>% round(3))

encoder_summary %<>%
  bind_rows(encoder_roc)

### Summary
library(stargazer)
encoder_summary_wide <- encoder_summary %>%
  arrange(desc(outcome)) %>%
  pivot_wider(names_from = outcome, values_from = value) 


# to c&p in paper
encoder_summary_wide   %>%
  stargazer(summary=FALSE, rownames=FALSE)
