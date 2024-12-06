## ---- data_simulation --------
data_simulation <- function(cluster_data, alpha = 1, beta = 1.5, gamma = 0.5) {
  
  # Empty dataframe to store results
  results_list <- list()
  
  for(idx in 1:nrow(cluster_data)) {
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
    
    for (i in unique(df$clusters)) {
      log_mu_i0 <- alpha + beta * df$X[df$clusters == i][1]  # Log of the expected mean
      log_mu_i <- rnorm(1, mean = log_mu_i0, sd = gamma)  # Add random effect
      
      # Calculate mu_i from log_mu_i
      mu_i <- exp(log_mu_i)
      
      # Simulate Y for each observation in the cluster
      df$Y[df$clusters == i] <- rpois(obs_per_cluster, lambda = mu_i)  # Poisson distribution
    }
    results_list[[idx]] <- df
  }
  return(results_list)
}

data_generation <- function(cluster_data, alpha = 1, beta = 1.5, gamma = 0.5, 
                            folder = "~/Desktop/PHP 2550 Pratical Data Analysis/Project 3/Data",
                            filename) {
  
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  gamma <- as.numeric(gamma)
  
  data <- data_simulation(cluster_data = cluster_data, alpha = alpha, beta = beta, gamma = gamma)
  
  # Save simulation data into CSV for each parameter combination
  file_path <- paste0(folder, "/", filename, "_data.csv")
  write.csv(data, file_path, row.names = FALSE)
}

## ---- fit_poisson_model --------
fit_model <- function(simulated_data, beta_true = 1.5) {
  
  # Initialize data frame and vector
  performance_measure <- data.frame(strategy = character(0),
                                    beta_estimate = numeric(0), 
                                    standard_error = numeric(0))
  
  for(i in 1:length(simulated_data)){
    
    df <- simulated_data[[i]]
    num_clusters <- length(unique(df$clusters))
    num_obs <- table(df$clusters)[[2]]
    
    if(length(unique(df$X)) < 2){
      measures <- c(strategy = paste(num_clusters, "Clusters", num_obs, "Observations"), 
                    beta_estimate = NA, standard_error = NA)
    } else{  
      
      # Fit cluster level simulated data using a Poisson distribution
      # Using glmer with the Poisson family and log link function
      model <- glmer(Y ~ X + (1 | clusters), data = df, family = poisson(link = "log"),
                     control = glmerControl( optimizer = "bobyqa", 
                                             check.conv.singular = .makeCC(action = "ignore", tol = 1e-4)))
      model.fit <- summary(model)
      
      # Retrieve coefficient and se estimates for the fixed effect of X
      coef <- model.fit$coefficients[2, 1] 
      se <- model.fit$coefficients[2, 2]          
      
      measures <- c(strategy = paste(num_clusters, "Clusters", num_obs, "Observations"),
                    beta_estimate = coef, standard_error = se)
      
    }
    
    # Save results
    performance_measure <- rbind(performance_measure, measures)
  }
  colnames(performance_measure) <- c("strategy", "beta_estimate", "standard_error")
  return(performance_measure)
}


