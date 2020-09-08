
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Configuração inicial

#### Passo 1: Instalar pacotes

``` r
install.packages("remotes")

# instalar pacote da Curso-R
remotes::install_github("curso-r/CursoR")

# instalar pacotes que vamos usar durante o curso
CursoR::instalar_dependencias()
```

#### Passo 2: Criar um projeto do RStudio

Faça um projeto do RStudio para usar durante todo o curso e em seguida
abra-o.

``` r
install.packages("usethis")
usethis::create_package("caminho_ate_o_projeto/nome_do_projeto")
```

#### Passo 3: Baixar o material

Certifique que você está dentro do projeto criado no passo 2 e rode o
código abaixo.

**Observação**: Assim que rodar o código abaixo, o programa vai pedir
uma escolha de opções. Escolha o número correspondente ao curso de
XGBoost\!

``` r
# Baixar ou atualizar material do curso
CursoR::atualizar_material()
```

## Slides

| slide               | link                                                         |
| :------------------ | :----------------------------------------------------------- |
| slides/xgboost.html | <https://curso-r.github.io/main-xgboost/slides/xgboost.html> |

## Scripts usados em aula

| script                             | link                                                                            |
| :--------------------------------- | :------------------------------------------------------------------------------ |
| 01-adult.R                         | <https://curso-r.github.io/202005-xgboost/R/01-adult.R>                         |
| 01-exercicio-hiperparametros-sql.R | <https://curso-r.github.io/202005-xgboost/R/01-exercicio-hiperparametros-sql.R> |
| 02-algoritmo-xgb-na-mao.R          | <https://curso-r.github.io/202005-xgboost/R/02-algoritmo-xgb-na-mao.R>          |
| 03-exercicio-classif.R             | <https://curso-r.github.io/202005-xgboost/R/03-exercicio-classif.R>             |
| 04-xgb-gifs.R                      | <https://curso-r.github.io/202005-xgboost/R/04-xgb-gifs.R>                      |
| 05-tunar-param-especifico.R        | <https://curso-r.github.io/202005-xgboost/R/05-tunar-param-especifico.R>        |
