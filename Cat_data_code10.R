library(gamlss)
library(pracma)
# Fit a Zero-Inflated Gamma Model
fit_zig <- gamlss(fre_week ~ 1, family = ZAGA, data = Subject)
summary(fit_zig)
AIC(fit_zig)
logLik(fit_zig)
fit_ziln <- gamlss(fre_week ~ 1, family = ZILN, data = Subject)
summary(fit_ziln)

# Fit ZINB Model
zinb_model <- zeroinfl(fre_week ~ 1, data = Subject, dist = "negbin")
summary(zinb_model)

hurdle_model <- hurdle(fre_week ~ 1, data = Subject, dist = "gamma")
summary(hurdle_model)

gamma_f<- function(x) {exp(-x)*dgamma(x, shape = fit_gamma_f$estimate["shape"], 
         rate = fit_gamma_f$estimate["rate"])*x}
expected_f <- integral(gamma_f, 0, Inf)
print(expected_f)
gamma_t<- function(x) {exp(-x)*dgamma(x, shape = fit_gamma_t$estimate["shape"], 
                                      rate = fit_gamma_t$estimate["rate"])*x}
expected_t <- integral(gamma_t, 0, Inf)
print(expected_t)


# Create a data frame
mortality_df <- data.frame(Mortality = Mortality, Mortality_w = Mortality_w)

# Plot densities
ggplot(mortality_df) +
  geom_density(aes(x = Mortality), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Mortality_w), fill = "red", alpha = 0.3) +
  labs(title = "Density of Mortality vs. Mortality_w", x = "Mortality", y = "Density") +
  theme_minimal()

# Fit Gamma Distribution
fit_gamma_m <- fitdist(Mortality, "gamma")
fit_gamma_w <- fitdist(Mortality_w, "gamma")

# Fit Log-Normal Distribution
fit_lognorm_m <- fitdist(Mortality, "lnorm")
fit_lognorm_w <- fitdist(Mortality_w, "lnorm")

# Fit Weibull Distribution
fit_weibull_m <- fitdist(Mortality, "weibull")
fit_weibull_w <- fitdist(Mortality_w, "weibull")

# Fit Beta Distribution (Only if values are in [0,1])
if (all(Mortality >= 0 & Mortality <= 1)) {
  fit_beta_m <- fitdist(Mortality, "beta")
}
if (all(Mortality_w >= 0 & Mortality_w <= 1)) {
  fit_beta_w <- fitdist(Mortality_w, "beta")
}
# Collect AIC values
aic_values_m <- c(
  fit_gamma_m$aic, fit_lognorm_m$aic, fit_weibull_m$aic,
  fit_gamma_w$aic, fit_lognorm_w$aic, fit_weibull_w$aic
)
names(aic_values_m) <- c("Gamma_M", "Log-Normal_M", "Weibull_M", 
                         "Gamma_W", "Log-Normal_W", "Weibull_W")

# Print AIC values (lower is better)
print(aic_values_m)

# QQ-Plot for Mortality (Best Model)
qqcomp(list(fit_gamma_m, fit_lognorm_m, fit_weibull_m), legendtext = c("Gamma", "Log-Normal", "Weibull"))

# QQ-Plot for Mortality_w (Best Model)
qqcomp(list(fit_gamma_w, fit_lognorm_w, fit_weibull_w), legendtext = c("Gamma", "Log-Normal", "Weibull"))
# Anderson-Darling, CVM Test for Gamma
ad.test(Mortality, pgamma, 
        shape = fit_gamma_m$estimate["shape"], 
        rate = fit_gamma_m$estimate["rate"])
cvm.test(Mortality, pgamma, 
         shape = fit_gamma_m$estimate["shape"], 
         rate = fit_gamma_m$estimate["rate"])

ad.test(Mortality_w, pgamma, 
        shape = fit_gamma_w$estimate["shape"], 
        rate = fit_gamma_w$estimate["rate"])
cvm.test(Mortality_w, pgamma, 
         shape = fit_gamma_w$estimate["shape"], 
         rate = fit_gamma_w$estimate["rate"])
