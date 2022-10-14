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

regular_samples <- data.frame(matrix(ncol = 1, nrow = sel_n))

# Set number of samples
# regular samples
for (i in 1:sel_num_samples) {
  regular_samples[[i]] <- get_sample()
}

# Bootstrap samples
bootstrap_samples <- data.frame(matrix(ncol = 1, nrow = sel_n))

for (i in 1:sel_num_samples) {
  bootstrap_samples[[i]] <- sample(regular_samples[[1]], sel_n,
                                   replace = TRUE)
}

names(regular_samples) <- NULL
regular_statistics_df <- data.frame(mean = map_dbl(regular_samples, mean),
                            sd= map_dbl(regular_samples, sd),
                            median= map_dbl(regular_samples, median)
                            )

names(bootstrap_samples) <- NULL
bootstrap_statistics_df <- data.frame(mean = map_dbl(bootstrap_samples, mean),
                                     sd = map_dbl(bootstrap_samples, sd),
                                     median = map_dbl(bootstrap_samples, median)
                                     )

statistics_df <- bind_rows("regular" = regular_statistics_df,
                           "bootstrap" = bootstrap_statistics_df, .id = "type")

statistics_means <- bind_rows("regular" = colMeans(regular_statistics_df),
                              "bootstrap" = colMeans(bootstrap_statistics_df),
                              .id = "type")


parameter_dic <- list("normal" = c("mean" = sel_mean,
                                   "sd" = sel_sd,
                                   "median" = sel_mean),
                      "uniform" = c("mean" =  (sel_max + sel_min)/2,
                                    "sd" = sqrt(((sel_max - sel_min)^2)/12),
                                    "median" = (sel_max + sel_min)/2))

statistics_df %>%
  rename_with(~ gsub(sel_statistic, "target", .x)) %>%
  ggplot(aes(target, color = type, fill=type)) +
         geom_histogram(alpha = 0.2, position = "identity", bins = 20) +
         labs(x=sel_statistic) +
         #geom_vline(aes(xintercept = parameter_dic[[sel_distribution]][[sel_statistic]])) +
         geom_vline(aes(xintercept = mean(target), color=type))


get_statistics_CI <- function(data) {
  results <- data.frame(matrix(ncol = ncol(data), nrow = 2))
  for (i in 1:ncol(data)) {
    data_model <- lm(data[[i]] ~ 1)
    interval <- confint(data_model, level = 0.95)
    results[i] <- c(interval[1], interval[2])
  }
  names(results) <- names(data)
  return(results)
}

get_statistics_quantiles <- function(data) {
  results <- data.frame(matrix(ncol = ncol(data), nrow = 2))
  for (i in 1:ncol(data)) {
    results[i] <- quantile(data[[i]], probs=(c(0.025, 0.975)))
  }
  names(results) <- names(data)
  return(results)
}

statistics_IC <- bind_rows("regular" = get_statistics_CI(),
                           "bootstrap" = get_statistics_quantiles(),
                           .id = "type"
)
