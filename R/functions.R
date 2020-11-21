

# p_grid <- seq(0, 1, length.out = n)
# prior <- seq(10, 0, length.out = n)
# 
# likelihood <- dbinom(1, size = 1, prob = p_grid)
# 
# posterior_unstandardized <- likelihood * prior
# 
# posterior <- posterior_unstandardized / sum(posterior_unstandardized)

get_posterior <- function(k, prior) {
  p_grid <- seq(0, 1, length.out = length(prior))
  
  likelihood <- dbinom(k, size = 1, prob = p_grid)
  
  posterior_unstandardized <- likelihood * prior
  
  posterior_unstandardized / sum(posterior_unstandardized)
}


# get_posterior(3, 3, c(1,2,3))
# 
# prior = c(1,1,1)
# 
# get_posterior(1, prior = c(1, 1, 1))
# 
# k = c(1, 0)
# 
# foo <- function(k, prior) {
#   if(length(k) == 0) return(prior)
#   
#   posterior <- get_posterior(k[1], prior = prior)
#   
#   return(foo(tail(k, -1), prior = posterior))
# }


populate_posteriors <- function(k, prior) {
  df <- tibble::tibble(
    id = seq_along(k),
    k = k,
    p_grid = list(seq(0, 1, length.out = length(prior))),
    prior = c(list(prior), vector("list", length(k) - 1)),
    posterior = vector("list", length(k))
  )
  
  for (i in seq_along(df$k)) {
    if(i != 1)  df[i, "prior"][[1]] = list(df[i - 1, ]$posterior[[1]])
    df[i, "posterior"][[1]] <- list(get_posterior(df[i, ]$k, df[i, ]$prior[[1]]))
  }
  
  tidyr::unnest(df, cols = c(p_grid, prior, posterior))
} 

string2vec <- function(k) {
  as.numeric(unlist(strsplit(k,"")))
}

# df <- populate_posteriors(c(1,0, 1, 1), rep(1, 100))
# 
# library(ggplot2)
# 
# ggplot(df, aes(p_grid, posterior, color = id, group = id)) + 
#     geom_line()



