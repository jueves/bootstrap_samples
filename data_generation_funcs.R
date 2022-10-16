# Functions to generate data and get statistics

get_sample <- function(distribution, sample_size, pop_mean, pop_sd, pop_min, pop_max){
  # Returns a sample from the defined population
  if (distribution == "normal") {
    new_sample <- rnorm(sample_size, pop_mean, pop_sd)
  } else if (distribution == "uniform") {
    new_sample <- runif(sample_size, pop_min, pop_max)
  }
  return(new_sample)
}

get_statistics_df <- function(distribution, sample_size, num_samples, pop_mean = 0,
                              pop_sd = 1, pop_min = 0, pop_max = 1) {
  # Generates multiple samples from the defined population, both regular samples
  # and bootstrap generated samples. Then returns a dataframe with the mean, sd
  # and median for each sample.
  
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
                             "bootstrap" = bootstrap_statistics_df, .id = "source")
  
  return(statistics_df)
}