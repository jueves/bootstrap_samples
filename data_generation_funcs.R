# Functions to generate data and get statistics

# Gets sample from the selected distribution
get_sample <- function(distribution, sample_size, pop_mean, pop_sd, pop_min, pop_max){
  # Gets new samples from the defined population
  if (distribution == "normal") {
    new_sample <- rnorm(sample_size, pop_mean, pop_sd)
  } else if (distribution == "uniform") {
    new_sample <- runif(sample_size, pop_min, pop_max)
  }
  return(new_sample)
}

get_statistics_df <- function(distribution, sample_size, num_samples, pop_mean = 0,
                              pop_sd = 1, pop_min = 0, pop_max = 1) {
  # Generate regular samples
  regular_samples <- data.frame(matrix(ncol = 1, nrow = sample_size))
  
  for (i in 1:num_samples) {
    regular_samples[[i]] <- get_sample(distribution, sample_size, pop_mean,
                                       pop_sd, pop_min, pop_max)
  }
  
  #  Generate bootstrap samples
  bootstrap_samples <- data.frame(matrix(ncol = 1, nrow = sample_size))
  
  for (i in 1:num_samples) {
    bootstrap_samples[[i]] <- sample(regular_samples[[1]], sample_size,
                                     replace = TRUE)
  }
  
  # Get all statistics for each sample
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
  
  # Bind both dataframes
  statistics_df <- bind_rows("regular" = regular_statistics_df,
                             "bootstrap" = bootstrap_statistics_df, .id = "type")
  
  return(statistics_df)
}


# statistics_means <- bind_rows("regular" = colMeans(regular_statistics_df),
#                               "bootstrap" = colMeans(bootstrap_statistics_df),
#                               .id = "type") %>%
#   rename_with(~ sub(input$statistic, "target", .x))


# get_statistics_quantiles <- function(data) {
#   results <- data.frame(matrix(ncol = ncol(data), nrow = 2))
#   for (i in 1:ncol(data)) {
#     results[i] <- quantile(data[[i]], probs=(c(0.025, 0.975)))
#   }
#   names(results) <- names(data)
#   return(results)
# }

