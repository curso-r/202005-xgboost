library(tidymodels)
library(tidyverse)
library(tidypredict)

# dados ----------------------------------------------------
data <- tribble(
  ~dose, ~eficacia,
  2, -6,
  8, 4,
  12, 5,
  16, -5
)

# especificacao do modelo ---------------------------------
# mapa dos hiperparâmetros:
#
# tree_depth = tree_depth
# loss_reduction = gamma
# lambda = lambda
# learn_rate = eta

xgb_model <- boost_tree(
  mode = "regression", 
  mtry = 1, 
  sample_size = 1,
  min_n = 1, 
  
  # -----------------------------------
  # PREENCHA AQUI (solução tem 4 linhas!)
  
  
  #-------------------------------------
) %>%
  set_engine("xgboost", lambda = 0)

xgb_fit <- fit(xgb_model, eficacia ~ dose, data = data)
xgb_fit

data %>% mutate(
  pred = predict(xgb_fit, data)$.pred
)

# bonus: SQL ------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "meu_sqlite_db.db")
tidypredict_sql(xgb_fit$fit, con)

copy_to(con, data, "data", overwrite = TRUE)
data_sql <- tbl(con, "data") %>%
  mutate(
    pred = !!tidypredict_sql(xgb_fit$fit, con)
  )

# resultado
data_sql

# SQL por trás dos panos
show_query(data_sql)


