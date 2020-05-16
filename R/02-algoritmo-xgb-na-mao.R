x <- runif(100, -1, 1)
y <- sin(x*3) + rnorm(100, sd = 0.1)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

loss <- function(y, y_hat) {
  (y - y_hat)^2
}

# gradiente (G)
G <- function(y, y_hat) {
  - 2 * (y - y_hat)
}

# hessiana (H)
H <- function(y, y_hat) {
  - 2
}

f <- function(x, arvores) {
  r <- rep(0, length(x))
  
  if (length(arvores) == 0)
    return(r)
  
  r <- rep(0, length(x))
  # soma as árvores (os case_whens)
  for (arvore in arvores) {
    r <- r + lr * predict(arvore, tibble(x = x))
  }
  r
}

arvores <- list()
lr <- 0.1


trees <- 100
for (i in 1:trees) {
  y_hat <- 0.5 + f(x, arvores[[1]])
  r <- G(y, y_hat)/d2_loss(y, y_hat) #output
  arvores[[i]] <- tree::tree(r ~ x)
}

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_function(fun = f, args = list(arvores = arvores), colour = "red", size = 1)
