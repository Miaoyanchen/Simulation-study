tune_all <- function(max.iter, strategy, beta_vec, gamma_vec, alpha_vec) {
  # Initialize the final data frame
  perf.measure.final <- data.frame(
    true.beta = numeric(0), 
    gamma = numeric(0), 
    alpha = numeric(0),
    strategy = character(0),
    avg.beta.est = numeric(0),
    avg.se = numeric(0),
    var.beta.est = numeric(0),
    var.se = numeric(0)
  )
  
  # Loop over all combinations of beta, gamma, and alpha
  for (beta in beta_vec) {
    for (gamma in gamma_vec) {
      for (alpha in alpha_vec) {
        
        # Initialize a list to store data frames from each iteration
        results_list <- vector("list", max.iter)
        
        for (j in 1:max.iter) {
          # Simulate data for current parameters
          df <- data_simulation(cluster_data = strategy, beta = beta, gamma = gamma, alpha = alpha)
          
          # Fit model and store results
          results_list[[j]] <- fit_model(simulated_data = df, beta_true = beta)
        }
        
        # Combine all results into one data frame
        combined_results <- do.call(rbind, results_list)
        combined_results$iteration <- rep(1:max.iter, each = nrow(results_list[[1]]))
        
        # Calculate the mean and variance of beta estimates and standard errors for each strategy
        aggregated_results <- aggregate(cbind(beta_estimate, standard_error) ~ strategy, 
                                        data = combined_results, 
                                        FUN = function(x) {
                                          c(mean = mean(as.numeric(x), na.rm = TRUE),
                                            var = var(as.numeric(x), na.rm = TRUE))
                                        })
        
        aggregated_results <- do.call(data.frame, aggregated_results)
        
        # Append results to the final data frame
        for (k in 1:nrow(aggregated_results)) {
          new_row <- c(beta, gamma, alpha, 
                       as.character(aggregated_results$strategy[k]),
                       as.numeric(round(aggregated_results$beta_estimate.mean[k], 3)), 
                       as.numeric(round(aggregated_results$standard_error.mean[k], 3)),
                       as.numeric(round(aggregated_results$beta_estimate.var[k], 3)), 
                       as.numeric(round(aggregated_results$standard_error.var[k], 3)))
          
          perf.measure.final <- rbind(perf.measure.final, new_row)
        }
      }
    }
  }
  
  # Rename columns
  names(perf.measure.final) <- c(
    "true.beta", "gamma", "alpha", "strategy", 
    "avg.beta.est", "avg.se", "var.beta.est", "var.se"
  )
  
  return(perf.measure.final)
}
