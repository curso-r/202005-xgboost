x <- runif(100, -1, 1)
y <- sin(x*3) + rnorm(100, sd = 0.1)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

loss <- function(y, y_hat) {
  (y - y_hat)^2
}

d_loss <- function(y, y_hat) {
  - 2 * (y - y_hat)
}

d2_loss <- function(y, y_hat) {
  - 2
}

f <- function(x, arvores) {
  r <- rep(0, length(x))
  
  if (length(arvores) == 0)
    return(r)
  
  r <- rep(0, length(x))
  for (arvore in arvores) {
    r <- r + lr * predict(arvore, tibble(x = x))
  }
  r
}

arvores <- list()
lr <- 0.1

for (i in 1:100) {
  
  y_hat <- f(x, arvores)
  r <- d_loss(y, y_hat)/d2_loss(y, y_hat)
  
  arvores[[i]] <- tree::tree(r ~ x)
  
}

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_function(fun = f, args = list(arvores = arvores))