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
##AIC and BIC

# Compute AIC and BIC for the logistic model (zero inflation)
aic_zero <- AIC(fit_zero_part)
bic_zero <- BIC(fit_zero_part)

# Compute AIC and BIC for the Gamma model (nonzero values)
aic_gamma <- AIC(fit_gamma_part)
bic_gamma <- BIC(fit_gamma_part)

# Total AIC & BIC for the full Zero-Inflated Gamma model
aic_ziga <- aic_zero + aic_gamma
bic_ziga <- bic_zero + bic_gamma

# Print the results
cat("AIC for ZIGA Model:", aic_ziga, "\n")
cat("BIC for ZIGA Model:", bic_ziga, "\n")
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
