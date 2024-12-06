------------------------------------------------------------------------
# Determine the optimal study design under a fixed budget

## Aim
This project explores the impact of study design parameters on the precision of treatment effect estimates $\hat{\beta}$ in cluster-randomized trials under a fixed budget constraint. We aim to determine the optimal study design that lowers the variance of the treatment effect estimates.


## Model Framework
For $Y_{ij} \sim$ normal distribution:

- Cluster-level mean: $\mu_i \mid X_i, \epsilon_i = \alpha + \beta X_i + \epsilon_i, \text{where} \epsilon \sim N(0, \gamma^2)$ We generate the cluster-level mean from a normal distribution with mean $\mu_{i0} = \alpha + \beta X_i$ and variance $\gamma^2$.

- Subject-level outcome: $Y_{ij} \mid \mu_i, e_{ij} = \mu_i + e_{ij}, \quad e_{ij} \sim N(0, \sigma^2)$ We generate the - subject-level outcome from a normal distribution with mean $\mu_i$ and variance $\sigma^2$.

For $Y_{ij} \sim$ Poisson distribution:

- Group-level mean: $\log(u_i) \sim N(\alpha + \beta X_i, \gamma^2)$ We generate the cluster-level mean from a normal distribution with mean $\mu_{i0} = \alpha + \beta X_i$ and variance $\gamma^2$, and exponentiate it to get the mean counts $\mu_i$

- Subject-level outcome: $Y_{ij} \mid \mu_i \sim Poisson(\mu_i)$ for each subject in the cluster We generate the subject-level outcome from a Poisson distribution with mean $\mu_i$.


### Results
The optimal study design is determined by minimizing the variance of the treatment effect estimates (\(\text{Var}(\hat{\beta})\)). The optimal study design is largely influenced by the between cluster correlation, and the budget constraint.

## Files

### Report files

-   `Project 3 - Simulation.qmd` - R quarto file that contains the code for visualization, and simulation results
-   `vary all parameters normal.R` - R script file that records function to test the impact of varying all parameters on the variance of the treatment effect estimates for $Y_{ij} \sim$ normal distribution
-   `vary all parameters poisson.R` - R script file that records function to test the impact of varying all parameters on the variance of the treatment effect estimates for $Y_{ij} \sim$ Poisson distribution
-   `fit model.R` - R script file that records function to fit the model for the normal distribution outcome
-   `data generation.R` - R script file that records function to generate data for the normal distribution outcome
-   `poisson simulation.R` - R script file that records function to simulate data, and fit model for the Poisson distribution outcome


## Libraries
lme4 - for fitting the mixed effects model

purrr - rbinom generating random numbers

ggplot2 - visualization

latex2exp - for latex expressions

tidyverse - data manipulation

ggpubr - visualization


