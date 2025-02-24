Subject <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_data.xlsx", sheet="Subject")
Subject <- Subject %>% select_at(vars(1:4))
Subject$fre_day<- Subject$frequency/Subject$calendar_day
Subject$fre_week <-Subject$fre_day *7
Subject$fre_week <- as.numeric(Subject$fre_week)
summary(Subject$fre_week)
var(Subject$fre_week)
mean(Subject$fre_week)
# Fit Gamma Distribution
fit_gamma_f <- fitdist(Subject$fre_week, "gamma")
summary(fit_gamma_f)

Subject$fre_week_r <- round(Subject$fre_week)

##Zero-Adjusted Gamma
fit_zaga_f <- gamlss(Subject$fre_week ~ 1, family = ZAGA) 
summary(fit_zaga_f)

sum(Subject$fre_week == 0) / length(Subject$fre_week)

# Extract fitted values (mu and sigma from the ZAGA model)
fitted_mu <- fitted(fit_zigamma_f, what = "mu")
fitted_sigma <- fitted(fit_zigamma_f, what = "sigma")
ad_test_result <- ad.test(Subject$fre_week, pgamma, shape = fitted_mu, rate = 1 / fitted_sigma)
print(ad_test_result)
cvm_test_result <- cvm.test(Subject$fre_week, pgamma, shape = fitted_mu, rate = 1 / fitted_sigma)
print(cvm_test_result)

# Anderson-Darling Test for Gamma
ad.test(Subject$fre_week, pgamma, 
        shape = fit_gamma_f$estimate["shape"], 
        rate = fit_gamma_f$estimate["rate"])
# Anderson-Darling Test for Exponential
ad.test(Subject$fre_week, pexp, 
        rate = fit_exp_f$estimate["rate"])

# Anderson-Darling Test for Gamma
cvm.test(Subject$fre_week, pgamma, 
        shape = fit_gamma_f$estimate["shape"], 
        rate = fit_gamma_f$estimate["rate"])

plot.legend3 <- c("Gamma", "Zero-inflated Gamma")
denscomp(list(fit_gamma_f, fit_zigamma_f), legendtext = plot.legend3)
qqcomp(list(fit_gamma_f, fit_zigamma_f), legendtext = plot.legend3)
cdfcomp(list(fit_gamma_f, fit_zigamma_f), legendtext = plot.legend3)

aic_values_f <- c(fit_gamma_f$aic, fit_zaga_f$aic, aic_ziga)
names(aic_values_f) <- c("Gamma", "ZAGA", "ZIGA")

bic_values_f <- c(fit_gamma_f$bic, fit_exp_f$bic, bic_ziga)
names(bic_values_f) <- c("Gamma", "ZAGA", "ZIGA")
print(aic_values_f)
print(bic_values_f)

# Define parameters based on model estimates
mu_value <- 2.1345     # Mean of nonzero values (log-scale)
sigma_value <- 0.06039  # Dispersion parameter
nu_value <- -0.5108    # Probability of structural zeros (logit scale)
# Convert nu from logit scale to probability scale
nu_prob <- exp(nu_value) / (1 + exp(nu_value))  # Logit transformation

# Print the corrected nu value
print(nu_prob)  # This should be between 0 and 1

# Generate 1000 simulated observations
set.seed(123)  # For reproducibility
simul_zaga_f <- rZAGA(1000, mu = mu_value, sigma = sigma_value, nu = nu_prob)
# Check the summary of the simulated dataset
summary(simul_zaga_f)

##ZIGA
fit_zero_part <- glm((Subject$fre_week == 0) ~ 1, family = binomial, data = Subject)
summary(fit_zero_part)
fit_gamma_part <- gamlss(Subject$fre_week[Subject$fre_week > 0] ~ 1, family = GA)
summary(fit_gamma_part)
# Probability of zeros (from logistic regression)
prob_zeros <- exp(coef(fit_zero_part)) / (1 + exp(coef(fit_zero_part)))

# Gamma parameters for nonzero values
mu_gamma <- fitted(fit_gamma_part, what = "mu")
sigma_gamma <- fitted(fit_gamma_part, what = "sigma")
set.seed(123)
n <- 1000
zero_sim <- rbinom(n, size = 1, prob = prob_zeros)  # Generate zero/nonzero indicator
gamma_sim <- rgamma(n, shape = mu_gamma, rate = 1 / sigma_gamma)  # Gamma-distributed values
simul_ziga <- ifelse(zero_sim == 1, 0, gamma_sim)  # Assign zeros where necessary


# Proportion of zero values
sum(simul_zaga_f == 0) / length(simul_zaga_f)
sum(Subject$fre_week == 0) / length(Subject$fre_week)
sum(simul_gamma_f == 0) / length(simul_gamma_f)
sum(simul_ziga == 0) / length(simul_ziga)

##Simulation
summary(simul_ziga)

simul_gamma_f <- rgamma(1000, 
                         shape = fit_gamma_f$estimate["shape"], 
                         rate = fit_gamma_f$estimate["rate"])
mean(simul_zaga_f)
summary(Subject$fre_week)
summary(simul_gamma_f)
ggplot() +
  geom_density(data = data.frame(Value = Subject$fre_week), 
               aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_f), 
               aes(x = Value), fill = "red", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_ziga), 
               aes(x = Value), fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Frequency", y = "Density") +
  geom_vline(aes(xintercept = mean(Subject$fre_week)), col = "blue")+
  geom_vline(aes(xintercept = mean(simul_gamma_f)), col = "red")+
  geom_vline(aes(xintercept = mean(simul_ziga)), col = "green")+
  xlim(0,50)+ylim(0,0.2)
## Rematching
library(truncdist)
simul_gamma_f_trunc <- rtrunc(n = 1000, spec = "gamma", a = min(Subject$fre_week), 
                              b = max(Subject$fre_week),
                              shape = fit_gamma_f$estimate["shape"], 
                              rate = fit_gamma_f$estimate["rate"])
simul_gamma_f_adjusted <- quantile(simul_gamma_f, probs = ecdf(Subject$fre_week)(simul_gamma_f))
##Range Limit Simulation
min_value <- min(Subject$fre_week)  
max_value <- max(Subject$fre_week) 

# Generate truncated ZAGA samples
simul_zaga_f <- ifelse(runif(1000) < nu_prob, 0,  
                       rtrunc(n = 1000, spec = "gamma", 
                              a = min_value, b = max_value, 
                              shape = mu_value, rate = sigma_value))
summary(simul_zaga_f)
# Generate truncated Gamma samples
simul_gamma_f <- rtrunc(n = 1000, spec = "gamma", 
                        a = min_value, b = max_value, 
                        shape = fit_gamma_f$estimate["shape"], 
                        rate = fit_gamma_f$estimate["rate"])
summary(simul_gamma_f)

# Generate zero-inflated probability (assuming zero_sim stores zero indicators)
zero_sim <- rbinom(1000, 1, nu_prob)  # Simulate zero-inflation indicator
gamma_sim <- rtrunc(n = 1000, spec = "gamma",
                    a = min_value, b = max_value, 
                    shape = fit_gamma_f$estimate["shape"], 
                    rate = fit_gamma_f$estimate["rate"])
# Apply zero-inflation condition
simul_ziga <- ifelse(zero_sim == 1, 0, gamma_sim)
summary(simul_ziga)
