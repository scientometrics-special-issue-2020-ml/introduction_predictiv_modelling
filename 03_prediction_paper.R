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

# NOTE: THIS IS ALL IDIOTIC DUE TO LACK IN TIME. Next time, sepperately take the patent quality data from the OECD and only the other fields from PATSTAT, then join

data <- readRDS("../input/xxxx.rds")


#########################################################################################################
# Descriptives
#########################################################################################################

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


#########################################################################################################
# Prediction - initial parameters
#########################################################################################################

### Load
rm(list=ls())
data <- readRDS("temp/data_reg_sub.RDS")


### Conditional variable distribution
require(ggridges)
data %>%
  select(-breakthrough) %>%
  mutate(breakthrough50 = factor(breakthrough50, labels = c("no", "yes"))) %>%
  gather(variable, value, -breakthrough50) %>%
  ggplot(aes(y = as.factor(variable), 
             fill =  breakthrough50, 
             x = percent_rank(value)) ) +
  geom_density_ridges(alpha = 0.75)
ggsave("var_dist_breakthrough50.pdf", path ="output/figures/", width=30, height=10, units="cm")
graphics.off() # since pdf too big

data %>%
  select(-breakthrough50) %>%
  mutate(breakthrough = factor(breakthrough, labels = c("no", "yes"))) %>%
  gather(variable, value, -breakthrough) %>%
  ggplot(aes(y = as.factor(variable), 
             fill =  breakthrough, 
             x = percent_rank(value)) ) +
  geom_density_ridges(alpha = 0.75)
ggsave("var_dist_breakthrough.pdf", path ="output/figures/", width=30, height=10, units="cm")
graphics.off() # since pdf too big


############################################################################
# training setup
############################################################################

# Normalize variables by technology field/year cohort
data %<>%
  group_by(tech_field_main, appln_filing_year) %>%
  mutate(across(c(family_size_docdb, amily_size_inpadoc, cit_bwd, cit_nlp, claims, originality, nb_applicants, nb_inventors), scale)) %>%
  ungroup()



training <- data; rm(data)

# Create recipe -> new package to make preprocessing really easy
reci <- recipe(breakthrough50 ~ ., data = training) %>%
  step_num2factor(many_field) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_dummy(many_field) %>% # , source
  step_zv(all_predictors()) %>%
  prep(data = training)
reci

# Baking
training <- bake(reci, newdata = training)

# split in y and x
y <- training %>% select(breakthrough50) %>% pull()# only the one we want now
x <- training %>% select(-breakthrough50) 

# remove everything we do not need anymore
rm(reci, training)

# trainControl
ctrl <- trainControl(method = "cv", # "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                     number = 3, # repeats = 1,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary,
                     returnData = FALSE,
                     returnResamp = "final",
                     savePredictions = "final",
                     allowParallel = FALSE,
                     verboseIter = TRUE)

metric <- "ROC"

save.image("temp/workspace_initial.R")


############################################################################
# logistic regression - feature selection
############################################################################

### Load
# rm(list=ls()); load("temp/workspace_initial.R")

# Recursive Feature Elimination
subsets <- c(1:length(x))
lrFuncs$summary <- twoClassSummary

rfe.ctrl = rfeControl(functions = lrFuncs,
                      method = "boot",
                      number = 25,
                      allowParallel = TRUE, verbose = TRUE)

rfe <- rfe(x = x, y = y$breakthrough50, sizes = subsets,
           metric = metric, rfeControl = rfe.ctrl)
rfe # family_size, bwd_cits, nb_applicants, originality, tech_field_X4
saveRDS(rfe, "temp/glm_fit_rfe.RDS"); rm(rfe)


############################################################################
# Elastic nets
############################################################################
tune.glmnet = expand.grid(alpha = seq(0, 1, length = 3), 
                          lambda = seq(0.0, 0.3, length = 7))

fit <- train(x = x, y = factor(y, labels = c("no", "yes")), 
             trControl = ctrl, 
             metric = metric, 
             method = "glmnet", family = "binomial", 
             tuneGrid = tune.glmnet)
saveRDS(fit, "temp/fit_glmnet.RDS")
# Fitting alpha = 1, lambda = 0 on full training set
rm(fit)


############################################################################
# Decision tree
############################################################################
tune.dt = expand.grid(cp = c(0.001, 0.005 ,0.010, 0.020, 0.040))

fit <- train(x = x, y = factor(y, labels = c("no", "yes")), 
             trControl = ctrl, 
             metric = metric,
             method = "rpart", 
             tuneGrid =  tune.dt) 

# The final value used for the model was cp = 0.001.
saveRDS(fit, "temp/fit_dt.RDS")
fit # trials = 20, model = rules, winnow = FALSE 
rm(fit) # TODO: CHeck if that makes sense....

############################################################################
# Random Forest
############################################################################

tune.rf <- expand.grid(mtry = seq(1, ncol(x)-1, length = 3),
                       min.node.size = c(10, 50, 100),
                       splitrule = c("gini", "extratrees"))

fit <- train(x = x, 
             y = factor(y, labels = c("no", "yes")), 
             trControl = ctrl,  
             metric = metric,
             method = "ranger", importance = "impurity", 
             tuneGrid =  tune.rf) 
saveRDS(fit, "temp/fit_rf.RDS")
fit #
rm(fit)

############################################################################
# FINAL RESULTS
############################################################################
rm(list=ls())

fit <- list()
fit$glmnet <- readRDS(fit, file ="temp/fit_glmnet.RDS")
fit$dt <- readRDS(fit, file ="temp/fit_dt.RDS")
fit$rf <- readRDS(fit, file ="temp/fit_rf.RDS")
saveRDS(fit, file ="temp/fit_all.RDS")

plot(fit$glmnet); dev.copy(pdf,"output/figures/tune_glmnet.pdf")
graphics.off()
plot(fit$dt); dev.copy(pdf,"output/figures/tune_dt.pdf") 
graphics.off()
plot(fit$rf); dev.copy(pdf,"output/figures/tune_rf.pdf")
graphics.off()


# plot tree structure
require(rattle); fancyRpartPlot(fit$dt$finalModel, roundint = FALSE) # Doesnt work now....
dev.copy(pdf,"output/figures/structure_dt.pdf")
graphics.off()



#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
# Prediction - Final
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

rm(list=ls()); data <- readRDS("temp/data_reg.RDS") # TODO CHange to final

# test and train split
index <- createDataPartition(y = data$breakthrough50, p = 0.75, list = FALSE)
training <- data[index, ]
test <- data[-index, ]
rm(index)

# Create recipe -> new package to make preprocessing really easy
reci <- recipe(breakthrough50 ~ ., data = training) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>%
  prep(data = training)
reci

# Baking
training <- bake(reci, newdata = training)
test  <- bake(reci, newdata = test)

# split in y and x
y <- select(training, breakthrough50) %>% pull() # only the one we want now
x <- select(training, -breakthrough, -breakthrough50) 

# remove everything we do not need anymore
rm(data, reci, training)


# trainControl
ctrl <- trainControl(method = "none", # "cv, "repeatedcv", #boot, cv, LOOCV, timeslice OR adaptive etc.
                     #number = 1, # repeats = 1,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary,
                     returnData = FALSE,
                     returnResamp = "final",
                     savePredictions = "final",
                     allowParallel = TRUE,
                     verboseIter = TRUE) #  , index = createResample(y$breakthrough3, 25)

metric <- "ROC"

#save.image("temp/workspace_final.R")

#rm(list=ls())
#load("temp/workspace_final.R")


### Joint model selection
tune.glm <- c("x")
tune.glmnet <- expand.grid(alpha = 1, lambda = 0)
tune.dt = expand.grid(cp = 0.001)
tune.rf <- expand.grid(mtry = length(x), min.node.size = 100, splitrule = "extratrees")



fit.glm <- train(x = x, y = factor(y, labels = c("no", "yes"))
                 , trControl = ctrl, metric = metric, 
                 method = "glm", family = "binomial")

fit.glmnet <- train(x = x, y = factor(y, labels = c("no", "yes")),
                    trControl = ctrl, metric = metric, 
                    method = "glmnet", family = "binomial", 
                    tuneGrid = tune.glmnet)

fit.dt <- train(x = x, y = factor(y, labels = c("no", "yes")),
                trControl = ctrl, metric = metric,
                method = "rpart", 
                tuneGrid =  tune.dt) 

fit.rf <- train(x = x, y = factor(y, labels = c("no", "yes")), 
                trControl = ctrl,  metric = metric,
                method = "ranger", importance = "impurity", tuneGrid =  tune.rf,
                num.trees = 25) 


models <- list(glm = fit.glm, elasticnet = fit.glmnet, dt = fit.dt, rf = fit.rf)
rm(fit.glm, fit.glmnet, fit.dt, fit.rf)

# plot tree structure
require(rattle); fancyRpartPlot(models$dt$finalModel); dev.copy(pdf,"output/figures/structure_dt.pdf", width = 10, height = 5); graphics.off()

### predict through all models
models.preds <- data.frame(lapply(models, predict, newdata = test)) 
models.preds.prob <- data.frame(lapply(models, predict, newdata = test, type = "prob"))


#######################################################################################
### plot all the stuff TODO: Do function
#######################################################################################

cm_glm <- confusionMatrix(factor(models.preds$glm, labels = c("no", "yes")), factor(test$breakthrough50, labels = c("no", "yes")), positive = "yes")
cm_glmnet <- confusionMatrix(factor(models.preds$elasticnet, labels = c("no", "yes")), factor(test$breakthrough50, labels = c("no", "yes")), positive = "yes")
cm_dt <- confusionMatrix(factor(models.preds$dt, labels = c("no", "yes")), factor(test$breakthrough50, labels = c("no", "yes")), positive = "yes")
cm_rf <- confusionMatrix(factor(models.preds$rf, labels = c("no", "yes")), factor(test$breakthrough50, labels = c("no", "yes")), positive = "yes")

fourfoldplot(cm_glm$table); dev.copy(pdf, "output/figures/CM_glm.pdf"); graphics.off()
fourfoldplot(cm_glmnet$table); dev.copy(pdf, "output/figures/CM_glmnet.pdf"); graphics.off()
fourfoldplot(cm_dt$table); dev.copy(pdf, "output/figures/CM_dt.pdf"); graphics.off()
fourfoldplot(cm_rf$table); dev.copy(pdf, "output/figures/CM_rf.pdf"); graphics.off()

colAUC(models.preds.prob$glm.yes, test$breakthrough50, plotROC = TRUE); dev.copy(png, "output/figures/ROC_glm.png"); graphics.off()
colAUC(models.preds.prob$elasticnet.yes, test$breakthrough50, plotROC = TRUE); dev.copy(png, "output/figures/ROC_glmnet.png"); graphics.off()
colAUC(models.preds.prob$dt.yes, test$breakthrough50, plotROC = TRUE); dev.copy(png, "output/figures/ROC_dt.png"); graphics.off()
colAUC(models.preds.prob$rf.yes, test$breakthrough50, plotROC = TRUE); dev.copy(png, "output/figures/ROC_rf.png"); graphics.off()

plot(varImp(models$glm)); dev.copy(pdf, "output/figures/varimp_glm.pdf"); graphics.off()
plot(varImp(models$elasticnet)); dev.copy(pdf, "output/figures/varimp_glmnet.pdf"); graphics.off()
plot(varImp(models$dt)); dev.copy(pdf, "output/figures/varimp_dt.pdf"); graphics.off()
plot(varImp(models$rf)); dev.copy(pdf, "output/figures/varimp_rf.pdf"); graphics.off()

ROC_glm <- colAUC(models.preds.prob$glm.yes, factor(test$breakthrough50, labels = c("no", "yes")), plotROC = F)
ROC_glmnet <- colAUC(models.preds.prob$elasticnet.yes, factor(test$breakthrough50, labels = c("no", "yes")), plotROC = F)
ROC_dt <- colAUC(models.preds.prob$dt.yes, factor(test$breakthrough50, labels = c("no", "yes")), plotROC = F)
ROC_rf <- colAUC(models.preds.prob$rf.yes, factor(test$breakthrough50, labels = c("no", "yes")), plotROC = F)


# final plot
model_eval <- tidy(cm_glm$overall) %>%
  select(names) %>%
  mutate(Logit = round(cm_glm$overall,3) ) %>%
  mutate(ElasticNet = round(cm_glmnet$overall,3)) %>%
  mutate(ClassTree = round(cm_dt$overall,3)) %>%
  mutate(RandForest = round(cm_rf$overall,3)) 

model_eval2 <- tidy(cm_glm$byClass) %>%
  select(names) %>%
  mutate(Logit = round(cm_glm$byClass,3) ) %>%
  mutate(ElasticNet = round(cm_glmnet$byClass,3)) %>%
  mutate(ClassTree = round(cm_dt$byClass,3)) %>%
  mutate(RandForest = round(cm_rf$byClass,3)) %>%
  filter( !(names %in% c("AccuracyPValue", "McnemarPValue")) ) 

model_eval_all <- model_eval %>%
  rbind(model_eval2) %>%
  rbind(c("AUC", round(ROC_glm,3), round(ROC_glmnet,3), round(ROC_dt,3), round(ROC_rf,3) ))

require(stargazer)
stargazer(model_eval_all, out="output/model_eval.tex",
          title="Final Model Evaluation with Test Sample",
          label="model_eval",
          align=F,
          summary = F,
          rownames = F)


save.image("temp/workspace_models_completed.R")


# 
# TODO: Maybe do a lime model
#



### Load
rm(list=ls()); load("temp/workspace_final.R")

### Joint model selection
tune.glm <- c("x")
tune.glmnet <- expand.grid(alpha = 1, lambda = 0)
tune.dt = expand.grid(cp = 0.001)
tune.rf <- expand.grid(mtry = 22, min.node.size = 100, splitrule = "extratrees")


models <- caretList(x = x, y = y$breakthrough3, trControl = ctrl, metric = metric, continue_on_fail = T,
                    tuneList = list(logit = caretModelSpec(method = "glm", family = "binomial"),
                                    elasticnet = caretModelSpec(method = "glmnet", family = "binomial",
                                                                tuneGrid =  tune.glmnet),
                                    dt = caretModelSpec(method = "rpart",
                                                        tuneGrid =  tune.dt),
                                    rf = caretModelSpec(method = "ranger", importance = "impurity",
                                                        tuneGrid =  tune.rf,
                                                        ntree = 5)   ) )
saveRDS(models, "temp/models.RDS")

# The last step is to recollect the results from each model and compare them:
results <- resamples(models) 

results$values %>%
  select(-Resample) %>%
  tidy()
modelCor(results) 
bwplot(results); xyplot(results)
dotplot(results); dev.copy(pdf, "output/figures/model_eva.pdf"); graphics.off()


# And now for the ensemble:
greedy_ensemble <- caretEnsemble(models, metric = "ROC", trControl = ctrl)
summary(greedy_ensemble)
