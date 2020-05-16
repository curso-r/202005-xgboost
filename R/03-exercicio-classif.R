library(tidymodels)
library(tidyverse)
library(tidypredict)

# dados ----------------------------------------------------
data <- tribble(
  ~dose, ~curou,
  2, "não curou",
  8, "curou",
  12, "curou",
  16, "não curou"
) 

# especificacao do modelo ---------------------------------
# mapa dos hiperparâmetros:
#
# tree_depth = tree_depth
# loss_reduction = gamma
# lambda = lambda
# learn_rate = eta

# Exercício 1 ##################################################################
# não queremos "regression" mais, queremos "classification"

xgb_model <- boost_tree(
  mode = "regression", 
  mtry = 1, 
  sample_size = 1,
  min_n = 1, 
  
  # -----------------------------------
  loss_reduction = 0,
  learn_rate = 0.3,
  tree_depth = 2,
  trees = 2
    
  #-------------------------------------
) %>%
  set_engine("xgboost", lambda = 0, params = list(min_child_weight = 0))




# fit (para resolver os exercícios 2 e 3)
xgb_fit <- fit(xgb_model, curou ~ dose, data = data)
xgb_fit



# Exercício 2 ##################################################################
# Perceba o erro!! 
# Erro: For classification models, the outcome should be a factor.
# Isso quer dizer que a função precisa que a coluna da variável resposta seja factor().
# Conserte isso e rode novamente.


# Exercício 3 ##################################################################
# Repare no parâmetro 'params = list(min_child_weight = 0)'.
# rode help(xgboost) para consultar a documentação da função xgboost e 
# procure pela definição de min_child_weight.


# Exercício 4 ##################################################################
# Observe essa tabela! O parâmetro  type = "prob" faz com que o predict devolva uma 
# tabela com as predições de cada categoria da variável resposta.
# Remova esse parâmetro type e rode novamente para ver o que ele devolve.
predict(xgb_fit, data, type = "prob")





# tabela com as predições
data %>% mutate(
  pred = predict(xgb_fit, data, type = "prob")$.pred_curou
)
# RESULTADO ESPERADO ########################
# # A tibble: 4 x 3
# dose curou      pred
# <dbl> <chr>     <dbl>
#   1     2 não curou 0.256
# 2     8 curou     0.744
# 3    12 curou     0.744
# 4    16 não curou 0.256

