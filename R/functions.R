get_posterior <- function(k, prior) {
  p_grid <- seq(0, 1, length.out = length(prior))

  likelihood <- dbinom(k, size = 1, prob = p_grid)

  posterior_unstandardized <- likelihood * prior

  posterior_unstandardized / sum(posterior_unstandardized)
}


populate_posteriors <- function(k, prior) {
  df <- tibble::tibble(
    id = seq_along(k),
    k = k,
    p_grid = list(seq(0, 1, length.out = length(prior))),
    prior = c(list(prior / sum(prior)), vector("list", length(k) - 1)),
    posterior = vector("list", length(k))
  )

  for (i in seq_along(df$k)) {
    if (i != 1) df[i, "prior"][[1]] <- list(df[i - 1, ]$posterior[[1]])
    df[i, "posterior"][[1]] <- list(get_posterior(df[i, ]$k, df[i, ]$prior[[1]]))
  }

  tidyr::unnest(df, cols = c(p_grid, prior, posterior))
}

string2vec <- function(k) {
  as.numeric(unlist(strsplit(k, "")))
}

plot_posteriors <- function(data) {
  line_size <- 1.5

  data %>%
    ggplot(aes(p_grid, posterior)) +
    geom_line(aes(color = id, group = id), size = line_size) +
    geom_line(data = . %>% filter(id == 1), aes(p_grid, prior), size = line_size, color = "gray", linetype = "dashed") +
    viridis::scale_color_viridis() +
    theme_classic(
      base_size = 15
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(
      title = "Posterior probability",
      subtitle = "Plausability of any specific probability of success",
      x = "Probability of success",
      y = "Plausability",
      caption = "Gray line represents a prior.",
      color = "Trial"
    )
}

# TODO change before someone pukes:)
are_outcomes_valid <- function(outcomes) {
  stringr::str_remove_all(stringr::str_remove_all(outcomes, "1"), "0") == ""
}
