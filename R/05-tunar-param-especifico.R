library(tidymodels)

library(modeldata)
data("lending_club")

model <- boost_tree(mtry = tune(), trees = 100, min_n = 30, 
                    tree_depth = 4, learn_rate = 0.1, loss_reduction = 0,
                    sample_size = 0.8)  %>% 
  set_engine("xgboost", lambda = tune("lambda")) %>% 
  set_mode("classification")


rec <- recipe(Class ~ ., data = lending_club) %>% 
  step_dummy(all_nominal(), -all_outcomes())


pars <- model %>% 
  parameters() %>% 
  update(
    lambda = dials::penalty()
  )

tun <- tune_grid(
  model, 
  preprocessor = rec,
  resamples = vfold_cv(lending_club, 5),
  grid = expand.grid(mtry = c(0.5, 0.8, 0.9), lambda = c(0.1, 0, 1e6)),
  param_info = pars
)