library(purrr)
library(dplyr)
library(ggplot2)

# Selections
sel_statistic = "mean" # Not used yet
sel_n = 30
sel_num_samples = 30

## Normal
sel_distribution = "normal"
sel_mean = 0
sel_sd = 1

## Uniform
sel_min = 0
sel_max = 1

get_sample <- function(){
  if (sel_distribution == "normal") {
    new_sample <- rnorm(sel_n, sel_mean, sel_sd)
  } else if (sel_distribution == "uniform") {
    new_sample <- runif(sel_n, sel_min, sel_max)
  }
  return(new_sample)
}

resampling_samples <- data.frame(matrix(ncol = 1, nrow = sel_n))

# Set number of samples
# resampling samples
for (i in 1:sel_num_samples) {
  resampling_samples[[i]] <- get_sample()
}

# Bootstrap samples
bootstrap_samples <- data.frame(matrix(ncol = 1, nrow = sel_n))

for (i in 1:sel_num_samples) {
  bootstrap_samples[[i]] <- sample(resampling_samples[[1]], sel_n,
                                   replace = TRUE)
}

names(resampling_samples) <- NULL
resampling_statistics_df <- data.frame(mean = map_dbl(resampling_samples, mean),
                            sd= map_dbl(resampling_samples, sd),
                            median= map_dbl(resampling_samples, median)
                            )

names(bootstrap_samples) <- NULL
bootstrap_statistics_df <- data.frame(mean = map_dbl(bootstrap_samples, mean),
                                     sd = map_dbl(bootstrap_samples, sd),
                                     median = map_dbl(bootstrap_samples, median)
                                     )

statistics_df <- bind_rows("resampling" = resampling_statistics_df,
                           "bootstrap" = bootstrap_statistics_df, .id = "type")

statistics_df %>%
  rename_with(~ gsub(sel_statistic, "target", .x)) %>%
  ggplot(aes(target, color = type, fill=type)) +
         geom_histogram(alpha = 0.2, position = "identity", bins = 20) +
         labs(x=sel_statistic)
