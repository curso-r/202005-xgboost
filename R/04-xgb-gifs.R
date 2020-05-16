library(tidyverse)
library(tidymodels)
library(gganimate)
theme_set(theme_minimal(20))

set.seed(1)
n <- 300
dados <- tibble(
  x = runif(n) - 0.5,
  y = sin(x * pi * 2) + rnorm(n, sd = 0.3)
)


gera_gif <- function(trees = 100, min_n = 1, 
                     tree_depth = 4, learn_rate = 0.1, 
                     sample_size = 1, loss_reduction = 0.1,
                     gif_name = glue::glue("slides/img/xgb_trees{trees}@min_n{min_n}@tree_depth{tree_depth}@learn_rate{learn_rate}@sample_size{sample_size}@loss_reduction{loss_reduction}.gif")) {
  
  modelo <- boost_tree(
    mode = "regression", 
    mtry = 1, 
    trees = trees, 
    min_n = min_n, 
    tree_depth = tree_depth, 
    learn_rate = learn_rate, 
    sample_size = sample_size, 
    loss_reduction = loss_reduction
  ) %>%
    set_engine("xgboost", base_score = mean(dados$y))
  
  
  ajuste <- fit(modelo, y ~ x, data = dados)
  
  # xgboost::xgb.plot.tree(model = ajuste$fit, trees = 1)
  # 
  # xgboost::xgb.plot.tree(model = ajuste$fit)
  # xgboost::xgb.model.dt.tree(model = ajuste$fit, trees = 0)
  
  dados_xgb <- dados %>% select(x) %>% as.matrix()
  
  predict_xgb <- function(step) {
    dados %>%
      mutate(
        pred = xgboost:::predict.xgb.Booster(ajuste$fit, newdata = dados_xgb, ntreelimit = step)
      )
  }
  
  df <- tibble(
    step = 1:trees,
    preds = map(step - 1, predict_xgb)
  ) %>%
    unnest(preds) %>%
    mutate(step = step)
  
  gif <- df %>%
    ggplot(aes(x = x)) +
    geom_point(aes(y = y), size = 2, alpha = 0.4) +
    stat_function(fun = ~sin(. * pi * 2), colour = "purple", size = 1.5) +
    geom_step(aes(y = pred), colour = "orange", size = 2) +
    transition_time(step) + 
    labs(title = 'Step: {frame_time}') +
    ease_aes("linear")
  
  gif <- animate(gif, nframes = trees +15, 5, end_pause = 15, height = 500, width =500*(1+sqrt(5))/2)
  anim_save(gif_name, gif)
  gif
}



# gera gif
gera_gif( 
  trees = 60, 
  min_n = 1, 
  tree_depth = 2, 
  learn_rate = 0.3, 
  sample_size = 0.5, 
  loss_reduction = 0.5
)
