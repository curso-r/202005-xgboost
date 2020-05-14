# Pacotes ----------------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(discrim)
library(DataExplorer)
library(purrr)
library(skimr)
library(pROC)

# número de processadores para paralelizar
processadores <- 5

# Download dos dados -----------------------------------------------------------------------------

# baixa adult.data se nao existe ainda
if(!file.exists("data/adult.data")) 
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", httr::write_disk("data/adult.data"))
  
# baixa adult.test se nao existe ainda
if(!file.exists("data/adult.test"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", httr::write_disk("data/adult.test"))
  
# baixa adult.names se nao existe ainda
if(!file.exists("data/adult.test"))
  httr::GET("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names", httr::write_disk("data/adult.names"))

# Carrega dados ---------------------------------------------------------------------------------------

# prepara os nomes das colunas para colocar no cabecalho
adult_names <- tibble(name = read_lines("data/adult.names")) %>%
  filter(
    str_detect(name, "^[^\\|].*:")
  ) %>%
  separate(name, c("name", "description"), sep = ":") %>%
  mutate(
    name = snakecase::to_snake_case(name)
  ) %>%
  add_row(name = "less_than_50k", description = "person earn more than USD 50K per year.")

# treino/teste 
adult_train <- read_csv(file = "data/adult.data", na = c("?", "", "NA"), col_names = adult_names$name) %>% mutate(less_than_50k = factor(less_than_50k))
adult_test  <- read_csv(file = "data/adult.test", na = c("?", "", "NA"), col_names = adult_names$name, skip = 1) %>%
  mutate(
    less_than_50k = factor(if_else(less_than_50k == "<=50K.", "<=50K", ">50K"))
  )

# Exploracao ------------------------------------------------------------------------------------------

# map(list(test = adult_test, train = adult_train), introduce) %>% enframe() %>% unnest() %>% t %>% as_tibble() %>% set_names(c("test", "train"))
# skimr::skim(adult_train)
# skimr::skim(adult_test)
# 
# plot_bar(adult_train)
# plot_histogram(adult_train)
# plot_histogram(adult_train %>% mutate_if(is.numeric, log1p))
# plot_qq(adult_train)
# plot_qq(adult_train %>% mutate_if(is.numeric, log1p))
# plot_correlation(na.omit(adult_train), maxcat = 5L)
# plot_correlation(na.omit(adult_train), type = "d")
# gg_miss_var(adult_train)
# vis_miss(adult_train)
# gg_miss_case(adult_train)

# Preparacao ------------------------------------------------------------------------------------------

adult_recipe <- recipe(adult_train, less_than_50k ~ .) %>%
  step_other(all_nominal(), -all_outcomes(), threshold = 0.01) %>%
  step_mutate(capital = capital_loss + capital_gain) %>%
  step_rm(capital_loss, capital_gain) %>%
  step_log(fnlwgt, age, capital, offset = 1) %>%
  step_modeimpute(occupation, workclass, native_country) %>%
  step_normalize(all_numeric()) %>%
  step_zv(all_predictors()) %>%
  step_novel(all_predictors(), -all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

# olha como a base ficou com bake()
adult_bake <- bake(prep(adult_recipe), new_data = adult_test)

# plot_bar(adult_bake)
# plot_bar(adult_dummy_bake)

# Modelos ----------------------------------------------------------------------------------------------

adult_knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("classification") %>%
  set_engine("kknn")

adult_nb_model <- naive_Bayes(smoothness = 1) %>%
  set_mode("classification") %>%
  set_engine("klaR")

adult_rf_model <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_mode("classification") %>%
  set_engine("ranger", num.threads = 5)

adult_xgb_model <- boost_tree(
  mtry = tune(), min_n = tune(), 
  tree_depth = tune(), 
  trees = 1000, sample_size = 0.70, 
  learn_rate = tune(), loss_reduction = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost", nthread = 5)

adult_lr_model <- logistic_reg(penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

adult_tree_model <- decision_tree(min_n = 2, cost_complexity = tune(), tree_depth = 8) %>%
  set_mode("classification") %>%
  set_engine("rpart")

# workflows --------------------------------------------------------------------------------------------

adult_workflow <- workflow() %>% add_recipe(adult_recipe)

adult_knn_workflow <-adult_workflow %>% add_model(adult_knn_model)
adult_nb_workflow <-adult_workflow %>% add_model(adult_nb_model)
adult_lr_workflow <-adult_workflow %>% add_model(adult_lr_model)
adult_rf_workflow <-adult_workflow %>% add_model(adult_rf_model)
adult_xgb_workflow <-adult_workflow %>% add_model(adult_xgb_model)
adult_tree_workflow <-adult_workflow %>% add_model(adult_tree_model)

# Tunagem ---------------------------------------------------------------------------------------------
meu_tune_grid <- function(workflow, grid = 10) {
  tune_grid(
    workflow,
    resamples = adult_resamples,
    grid = grid,
    metrics = metric_set(accuracy, precision, recall, roc_auc),
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
}

# reamostragens por validacao cruzada
set.seed(42)
adult_resamples <- vfold_cv(adult_train, v = 5)

# ligar processamento paralelo no linux
doParallel::registerDoParallel(processadores)

tictoc::tic("rf")
adult_rf_tune_grid <- meu_tune_grid(adult_rf_workflow)
tictoc::toc()

tictoc::tic("knn")
adult_knn_tune_grid <- meu_tune_grid(adult_knn_workflow)
tictoc::toc()

tictoc::tic("nb")
adult_nb_tune_grid <- meu_tune_grid(adult_nb_workflow)
tictoc::toc()

tictoc::tic("lr")
adult_lr_tune_grid <- meu_tune_grid(adult_lr_workflow)
tictoc::toc()

tictoc::tic("xgb")
adult_xgb_tune_grid <- meu_tune_grid(adult_xgb_workflow)
tictoc::toc()

tictoc::tic("tree")
adult_tree_tune_grid <- meu_tune_grid(adult_tree_workflow)
tictoc::toc()

doParallel::stopImplicitCluster()

# graficos dos tunes
autoplot(adult_knn_tune_grid)
autoplot(adult_nb_tune_grid)
autoplot(adult_lr_tune_grid) + scale_x_log10()
autoplot(adult_rf_tune_grid)
autoplot(adult_xgb_tune_grid)
autoplot(adult_tree_tune_grid) + scale_x_log10()

# modelos finais ---------------------------------------------------------------------------------------

meu_fit <- function(tune_grid, model, workflow) {
  adult_best_model <- select_best(tune_grid, "roc_auc")
  print(adult_best_model)
  adult_final_model <- finalize_model(model, adult_best_model)
  adult_workflow <- workflow %>% update_model(adult_final_model)
  
  adult_fit <- fit(adult_workflow, data = adult_train)
  adult_fit
}

adult_knn_fit <- meu_fit(adult_knn_tune_grid, adult_knn_model, adult_knn_workflow)
adult_nb_fit <- fit(adult_nb_workflow, adult_train)
adult_lr_fit <- meu_fit(adult_lr_tune_grid, adult_lr_model, adult_lr_workflow)
adult_rf_fit <- meu_fit(adult_rf_tune_grid, adult_rf_model, adult_rf_workflow)
adult_xgb_fit <- meu_fit(adult_xgb_tune_grid, adult_xgb_model, adult_xgb_workflow)
adult_tree_fit <- meu_fit(adult_tree_tune_grid, adult_tree_model, adult_tree_workflow)

# Comparacao de modelos --------------------------------------------------------------------------------
bd <- adult_test
adult_preds <- bind_rows(
  predict(adult_knn_fit, bd, type = "prob") %>% mutate(modelo = "KNN") %>% mutate(less_than_50k = factor(bd$less_than_50k)),
  predict(adult_nb_fit, bd, type = "prob") %>% mutate(modelo = "Naive Bayes") %>% mutate(less_than_50k = factor(bd$less_than_50k)),
  predict(adult_lr_fit, bd, type = "prob") %>% mutate(modelo = "Regressão Logística") %>% mutate(less_than_50k = factor(bd$less_than_50k)),
  predict(adult_rf_fit, bd, type = "prob") %>% mutate(modelo = "Random Forest") %>% mutate(less_than_50k = factor(bd$less_than_50k)),
  predict(adult_xgb_fit, bd, type = "prob") %>% mutate(modelo = "XGBoost") %>% mutate(less_than_50k = factor(bd$less_than_50k)),
  predict(adult_tree_fit, bd, type = "prob") %>% mutate(modelo = "Árvore de decisão") %>% mutate(less_than_50k = factor(bd$less_than_50k))
) %>%
  mutate(
    pred_prob = `.pred_>50K`,
    pred_class = factor(if_else(pred_prob > 0.5,  ">50K", "<=50K"), levels = c(">50K", "<=50K"))
  ) 

# RESULTS
adult_comparacao_de_modelos <- adult_preds %>%
  mutate(
    less_than_50k = fct_relevel(less_than_50k, ">50K"),
    pred_class = factor(if_else(pred_prob > 0.5,  ">50K", "<=50K"), levels = c(">50K", "<=50K"))
  ) %>%  
  group_by(modelo) %>%
  summarise(
    auc = roc_auc_vec(less_than_50k, pred_prob),
    acc = accuracy_vec(less_than_50k, pred_class),
    prc = precision_vec(less_than_50k, pred_class),
    rec = recall_vec(less_than_50k, pred_class),
    roc = list(roc(less_than_50k, pred_prob))
  ) %>%
  mutate(
    `tempo de tunagem` = c( "40s", "185s", "183s",  "750s", "15s", "597s")
  )

# ROC tipo 2
adult_curvas_roc <- adult_preds %>% 
  group_by(modelo) %>% 
  roc_curve(less_than_50k, pred_prob)

adult_curvas_roc %>% 
  autoplot()

write_rds(adult_comparacao_de_modelos, "data/adult_comparacao_de_modelos_v2.rds")
write_rds(adult_curvas_roc, "data/adult_curvas_roc_v2.rds")
