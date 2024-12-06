
# Fit model, get estimate for beta, variance for beta
fit_model <- function(simulated_data, beta_true = 1.5){
  
  # Initialize dataframe and vector
  performance_measure <- data.frame(strategy = character(0),
                                    beta_estimate = numeric(0), 
                                    standard_error = numeric(0))
  
  for(i in 1:length(simulated_data)){
    
    df <- simulated_data[[i]]
    num_clusters <- length(unique(df$clusters))
    num_obs <- table(df$clusters)[[1]]
    
    if(length(unique(df$X)) < 2){
      measures <- c(strategy = paste(num_clusters, "Clusters", num_obs, "Observations"), 
                    beta_estimate = NA, standard_error = NA)
    } else{
      # Fit cluster level simulated data
      model = lmer(Y ~ X + (1 | clusters), data = df, 
                   control = lmerControl("bobyqa", calc.derivs = F,
                                         check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
      model.fit <- summary(model)
      
      # Retrieve coefficient
      coef <- model.fit$coefficients["X", "Estimate"]
      
      # Evaluate model performance using metrics
      se <- model.fit$coefficients["X", "Std. Error"]
      measures <- c(strategy = paste(num_clusters, "Clusters", num_obs, "Observations"),
                    beta_estimate = coef, standard_error = se)
    }
    
    # Save results
    performance_measure <- rbind(performance_measure, measures)
  }
  
  colnames(performance_measure) <- c("strategy", "beta_estimate", "standard_error")
  return(performance_measure)
}
