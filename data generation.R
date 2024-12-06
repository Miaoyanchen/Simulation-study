# Data generation
data_simulation <- function(cluster_data, alpha = 0.5, beta = 1.5, sigma = 1, gamma = 0.5) {
  
  # Empty dataframe to store results
  results_list <- list()
  
  # Loop over each cluster in cluster_data
  for (idx in 1:nrow(cluster_data)) {
    num_clusters <- cluster_data$num_clusters[idx]
    obs_per_cluster <- cluster_data$obs_per_cluster[idx]
    
    # Simulate treatment assignment for each cluster; 1 = treatment and 0 = control
    treatment_assignment <- rbernoulli(n = num_clusters, p = 0.5)
    treatment_assignment <- ifelse(treatment_assignment == TRUE, 1, 0)
    
    # Create a dataframe
    df <- data.frame(
      id = 1:(num_clusters * obs_per_cluster),
      clusters = rep(1:num_clusters, each = obs_per_cluster),
      X = rep(treatment_assignment, each = obs_per_cluster),
      Y = rep(NA, num_clusters * obs_per_cluster)
    )
    
    # Apply cluster-level effects
    for (i in unique(df$clusters)) {
      mu_i0 <- alpha + beta * df$X[df$clusters == i][1] # Fixed effect for the cluster
      epsilon_i <- rnorm(1, mean = 0, sd = gamma)       # Cluster level random effect
      mu_i <- mu_i0 + epsilon_i                         # Total mean effect for the cluster
      
      # Simulate Y for each observation in the cluster
      df$Y[df$clusters == i] <- rnorm(n = sum(df$clusters == i), mean = mu_i, sd = sigma)
    }
    
    # Combine with the main results data frame
    results_list[[idx]] <- df
  }
  
  return(results_list)
}

# Test example
# simulated_data <- data_simulation(cluster_data)



# data_generation <- function(cluster_data = cluster_data, 
#                             alpha = 0.5, beta = 1, sigma = 2, gamma = 0.5, 
#                             folder = "~/Desktop/PHP 2550 Pratical Data Analysis/Project 3/Data",
#                             filename){
#   
#   alpha <- as.numeric(alpha)
#   beta <- as.numeric(beta)
#   sigma <- as.numeric(sigma)
#   gamma <- as.numeric(gamma)
#   
#   data <- data_simulation(cluster_data = cluster_data, alpha, beta, sigma, gamma)
#   
#   # Save simulation data into CSV for each parameter combination
#   file_path <- paste0(folder, "/", filename, "_data.csv")
#   write.csv(data, file_path, row.names = FALSE)
# }
