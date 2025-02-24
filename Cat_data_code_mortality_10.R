set.seed(123)
zero_sim <- rbinom(1000, 1, nu_prob)  # Simulate zero-inflation indicator
gamma_sim <- rtrunc(n = 1000, spec = "gamma",
                    a = min_value, b = max_value, 
                    shape = fit_gamma_f$estimate["shape"], 
                    rate = fit_gamma_f$estimate["rate"])
F_samples <- ifelse(zero_sim == 1, 0, gamma_sim)
F_seasonal <- F_samples*26
T_samples <- rlnorm(1000, meanlog = fit_lognorm_t$estimate["meanlog"], 
                    sdlog = fit_lognorm_t$estimate["sdlog"])
#Resample from Observed
FO_sample <- sample(Subject$fre_week, size=1000, replace=TRUE)
FO_seasonal <- FO_sample*26
TO_sample <- sample(Catcam_filtered$traffic, size=1000, replace=TRUE)

V_1 <-2.17
V_2 <-7
B <-7.03
C_1<-B/V_1
C_2<-B/V_2

Mortality_o_1 <- (1 - exp(-(TO_sample) * C_1)) * 0.7
Mortality_o_2 <- (1 - exp(-(TO_sample) * C_2)) * 0.7
Mortality_ow_1 <-(1 - (exp(-(TO_sample) * C_1)^(FO_sample))) * 0.7
Mortality_ow_2 <-(1 - (exp(-(TO_sample) * C_2)^(FO_sample))) * 0.7
Mortality_os_1 <-(1 - (exp(-(TO_sample) * C_1)^(FO_seasonal))) * 0.7
Mortality_os_2 <-(1 - (exp(-(TO_sample) * C_2)^(FO_seasonal))) * 0.7
Mortality_1 <- (1 - exp(-(T_samples) * C_1)) * 0.7
Mortality_2 <- (1 - exp(-(T_samples) * C_2)) * 0.7
Mortality_w_1 <-(1 - (exp(-(T_samples) * C_1)^(F_samples))) * 0.7
Mortality_w_2 <-(1 - (exp(-(T_samples) * C_2)^(F_samples))) * 0.7
Mortality_s_1 <-(1 - (exp(-(T_samples) * C_1)^(F_seasonal))) * 0.7
Mortality_s_2 <-(1 - (exp(-(T_samples) * C_2)^(F_seasonal))) * 0.7
# Plot densities
ggplot() +
  geom_density(aes(x = Mortality_o_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_o_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Observed Mortality per Crossing", y = "Density") +
  theme_minimal()
ggplot() +
  geom_density(aes(x = Mortality_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Mortality per Crossing", y = "Density") +
  theme_minimal()
ggplot() +
  geom_density(aes(x = Mortality_ow_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_ow_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Observed Weekly Mortality of Crossing", y = "Density") +
  theme_minimal()
ggplot() +
  geom_density(aes(x = Mortality_w_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_w_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Weekly Mortality of Crossing", y = "Density") +
  theme_minimal()
ggplot() +
  geom_density(aes(x = Mortality_s_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_s_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Seasonal Mortality of Crossing", y = "Density") +
  theme_minimal()
ggplot() +
  geom_density(aes(x = Mortality_os_1), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_os_2), fill = "red", alpha = 0.5) +
  labs(title = " ", x = "Observed Seasonal Mortality of Crossing", y = "Density") +
  theme_minimal()
# List of mortality datasets
mortality_datasets <- list(
  "Observed Mortality (v=2.17)" = Mortality_o_1,
  "Observed Mortality (v=7)" = Mortality_o_2,
  "Observed Weekly Mortality (v=2.17)" = Mortality_ow_1,
  "Observed Weekly Mortality (v=7)" = Mortality_ow_2,
  "Mortality (v=2.17)" = Mortality_1,
  "Mortality (v=7)" = Mortality_2,
  "Weekly Mortality (v=2.17)" = Mortality_w_1,
  "Weekly Mortality (v=7)" = Mortality_w_2,
  "6 months Mortality (v=2.17)" = Mortality_s_1,
  "6 months Mortality (v=7)" = Mortality_s_2,
  "Observed 6 months Mortality (v=2.17)" = Mortality_os_1,
  "Observed 6 months Mortality (v=7)" = Mortality_os_2
)

# Function to fit Gamma, Log-Normal, and Beta distributions
fit_models <- function(data) {
  data <- data[data > 0]  # Remove zero values for log transformations
  fit_gamma <- fitdist(data, "gamma")
  fit_lognorm <- fitdist(data, "lnorm")
  fit_beta <- fitdist(data / max(data), "beta")  # Beta requires values in (0,1)
  
  return(list(
    gamma = fit_gamma,
    lognorm = fit_lognorm,
    beta = fit_beta
  ))
}

# Fit distributions to each dataset
fitted_models <- lapply(mortality_datasets, fit_models)

# Function to extract model comparison criteria (AIC, BIC, AD, CVM)
extract_criteria <- function(fitted_list) {
  gof_gamma <- gofstat(fitted_list$gamma)
  gof_lognorm <- gofstat(fitted_list$lognorm)
  gof_beta <- gofstat(fitted_list$beta)
  
  return(data.frame(
    Distribution = c("Gamma", "Log-Normal", "Beta"),
    AIC = c(fitted_list$gamma$aic, fitted_list$lognorm$aic, fitted_list$beta$aic),
    BIC = c(fitted_list$gamma$bic, fitted_list$lognorm$bic, fitted_list$beta$bic),
    AD = c(gof_gamma$ad, gof_lognorm$ad, gof_beta$ad),
    CVM = c(gof_gamma$cvm, gof_lognorm$cvm, gof_beta$cvm)
  ))
}

# Create a structured data frame for model selection results
model_comparison <- do.call(rbind, lapply(names(fitted_models), function(name) {
  df <- extract_criteria(fitted_models[[name]])
  df$Dataset <- name
  return(df)
}))

# Print the model comparison table
print(model_comparison)

# Function to plot fitted distributions
plot_fitted_distributions <- function(data, fits, title) {
  ggplot() +
    geom_density(aes(x = data), fill = "grey", alpha = 0.5, color = "black") +
    stat_function(fun = dgamma, args = list(shape = fits$gamma$estimate["shape"], rate = fits$gamma$estimate["rate"]), 
                  color = "blue", linetype = "solid", size = 1, alpha = 0.7) +
    stat_function(fun = dlnorm, args = list(meanlog = fits$lognorm$estimate["meanlog"], sdlog = fits$lognorm$estimate["sdlog"]), 
                  color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
    stat_function(fun = dbeta, args = list(shape1 = fits$beta$estimate["shape1"], shape2 = fits$beta$estimate["shape2"]), 
                  color = "green", linetype = "dotted", size = 1, alpha = 0.7) +
    labs(title = title, x = "Mortality", y = "Density") +
    theme_minimal() +
    theme(legend.position = "topright")
}

# Plot each dataset with fitted distributions
plot_list <- lapply(names(mortality_datasets), function(name) {
  plot_fitted_distributions(mortality_datasets[[name]], fitted_models[[name]], title = paste("Fitted Distributions -", name))
})

# Display the plots
print(plot_list)

# Function to compute mode (peak value) from density
get_density_mode <- function(data) {
  d <- density(data, na.rm = TRUE)  # Compute density
  mode_index <- which.max(d$y)      # Find index of max density
  return(d$x[mode_index])           # Return x-value at peak
}

# Function to compute mean
get_mean <- function(data) {
  return(mean(data, na.rm = TRUE))
}

# Compute Mode and Mean for Mortality Distributions
results <- data.frame(
  Dataset = names(mortality_datasets),
  Mode = sapply(mortality_datasets, get_density_mode),
  Mean = sapply(mortality_datasets, get_mean)
)

# Print results
print(results)
# Function to find multiple density peaks
get_density_modes <- function(data) {
  d <- density(data, na.rm = TRUE)  # Compute density estimate
  peaks <- which(diff(sign(diff(d$y))) == -2)  # Find local maxima indices
  valid_modes <- d$x[peaks] 
  valid_modes <- valid_modes[valid_modes >= 0]  # Keep only non-negative values
  return(valid_modes)  
}

# Compute corrected modes
mode_6_1 <- get_density_modes(Mortality_s_1) 
mode_6_2 <- get_density_modes(Mortality_s_2) 
mode_o6_1 <- get_density_modes(Mortality_os_1) 
mode_o6_2 <- get_density_modes(Mortality_os_2) 

# Print new modes
print(mode_6_1)
print(mode_6_2)
print(mode_o6_1)
print(mode_o6_2)


# Create an empty data frame for storing summary statistics
summary_df <- data.frame(
  Dataset = character(),
  Min = numeric(),
  `1st Qu.` = numeric(),
  Median = numeric(),
  Mean = numeric(),
  `3rd Qu.` = numeric(),
  Max = numeric(),
  stringsAsFactors = FALSE
)
# Compute summary statistics for each dataset
for (name in names(mortality_datasets)) {
  dataset <- mortality_datasets[[name]]
  
  # Ensure dataset is numeric and has valid entries
  if (is.numeric(dataset) && length(dataset) > 0) {
    summary_df <- rbind(summary_df, data.frame(
      Dataset = name,
      Min = min(dataset, na.rm = TRUE),
      `1st Qu.` = quantile(dataset, 0.25, na.rm = TRUE),
      Median = median(dataset, na.rm = TRUE),
      Mean = mean(dataset, na.rm = TRUE),
      `3rd Qu.` = quantile(dataset, 0.75, na.rm = TRUE),
      Max = max(dataset, na.rm = TRUE)
    ))
  }
}

# Print the summary table
print(summary_df)

# Save as CSV if needed
write.csv(summary_df, "mortality_summary.csv", row.names = FALSE)