# Fit Weibull
fit_weibull_A <- fitdist(Catcam_filtered$AADT, "weibull")
summary(fit_weibull_A)
# Fit Gamma
fit_gamma_A <- fitdist(Catcam_filtered$AADT, "gamma")
summary(fit_gamma_A)
# Fit Log-Normal
fit_lognorm_A <- fitdist(Catcam_filtered$AADT, "lnorm")
summary(fit_lognorm_A)
# Fit Exponential
fit_exp_A <- fitdist(Catcam_filtered$AADT, "exp")
summary(fit_exp_A)

plot.legend2 <- c("Weibull", "Gamma", "Log-Normal", "Exponential","Normal", "Logistic", "Generalized Gamma")
denscomp(list(fit_weibull_A, fit_gamma_A, fit_lognorm_A, fit_exp_A, 
              fit_normal_A, fit_logistic_A, fit_gen_gamma_A), legendtext = plot.legend2)
qqcomp(list(fit_weibull_A, fit_gamma_A, fit_lognorm_A, fit_exp_A, 
            fit_normal_A, fit_logistic_A, fit_gen_gamma_A), legendtext = plot.legend2)
cdfcomp(list(fit_weibull_A, fit_gamma_A, fit_lognorm_A, fit_exp_A, 
             fit_normal_A, fit_logistic_A, fit_gen_gamma_A), legendtext = plot.legend2)


# Anderson-Darling Test for Weibull
ad.test(Catcam_filtered$AADT, pweibull, 
        shape = fit_weibull_A$estimate["shape"], 
        scale = fit_weibull_A$estimate["scale"])
# Anderson-Darling Test for Gamma
ad.test(Catcam_filtered$AADT, pgamma, 
        shape = fit_gamma_A$estimate["shape"], 
        rate = fit_gamma_A$estimate["rate"])
# Anderson-Darling Test for Log-Normal
ad.test(Catcam_filtered$AADT, plnorm, 
        meanlog = fit_lognorm_A$estimate["meanlog"], 
        sdlog = fit_lognorm_A$estimate["sdlog"])
# Anderson-Darling Test for Exponential
ad.test(Catcam_filtered$AADT, pexp, 
        rate = fit_exp_A$estimate["rate"])

# Cramér–von Mises Test for Weibull
cvm.test(Catcam_filtered$AADT, pweibull, 
         shape = fit_weibull_A$estimate["shape"], 
         scale = fit_weibull_A$estimate["scale"])
# Cramér–von Mises Test for Gamma
cvm.test(Catcam_filtered$AADT, pgamma, 
         shape = fit_gamma_A$estimate["shape"], 
         rate = fit_gamma_A$estimate["rate"])
# Cramér–von Mises Test for Log-Normal
cvm.test(Catcam_filtered$AADT, plnorm, 
         meanlog = fit_lognorm_A$estimate["meanlog"], 
         sdlog = fit_lognorm_A$estimate["sdlog"])
# Cramér–von Mises Test for Exponential
cvm.test(Catcam_filtered$AADT, pexp, 
         rate = fit_exp_A$estimate["rate"])

aic_values_A <- c(fit_weibull_A$aic, fit_gamma_A$aic, fit_lognorm_A$aic, fit_exp_A$aic)
names(aic_values_A) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")

bic_values_A <- c(fit_weibull_A$bic, fit_gamma_A$bic, fit_lognorm_A$bic, fit_exp_A$bic)
names(bic_values_A) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
print(aic_values_A)
print(bic_values_A)

##Different distribution
# Define Generalized Gamma functions
dgengamma <- function(x, shape, scale, k) dgamma(x, shape = shape, scale = scale)
pgengamma <- function(q, shape, scale, k) pgamma(q, shape = shape, scale = scale)
qgengamma <- function(p, shape, scale, k) qgamma(p, shape = shape, scale = scale)
# Manually define starting values based on rough estimates from Gamma fit
start_values <- list(shape = 2, scale = 1, k = 1)

# Fit Normal Distribution
fit_normal_A <- fitdist(Catcam_filtered$AADT, "norm")
summary(fit_normal_A)
# Fit Logistic Distribution
fit_logistic_A <- fitdist(Catcam_filtered$AADT, "logis")
summary(fit_logistic_A)

# Fit Generalized Gamma Distribution
fit_gen_gamma_A <- fitdist(Catcam_filtered$AADT, "gengamma", start = start_values)
summary(fit_gen_gamma_A)

# AD Test for Normal
ad.test(Catcam_filtered$AADT, pnorm, 
        mean = fit_normal_A$estimate["mean"], 
        sd = fit_normal_A$estimate["sd"])
# AD Test for Logistic
ad.test(Catcam_filtered$AADT, plogis, 
        location = fit_logistic_A$estimate["location"], 
        scale = fit_logistic_A$estimate["scale"])

# AD Test for Generalized Gamma
ad.test(Catcam_filtered$AADT, pgengamma, 
        shape = fit_gen_gamma_A$estimate["shape"], 
        scale = fit_gen_gamma_A$estimate["scale"], 
        k = fit_gen_gamma_A$estimate["k"])

# CvM Test for Normal
cvm.test(Catcam_filtered$AADT, pnorm, 
         mean = fit_normal_A$estimate["mean"], 
         sd = fit_normal_A$estimate["sd"])
# CvM Test for Logistic
cvm.test(Catcam_filtered$AADT, plogis, 
         location = fit_logistic_A$estimate["location"], 
         scale = fit_logistic_A$estimate["scale"])

# CvM Test for Generalized Gamma
cvm.test(Catcam_filtered$AADT, pgengamma, 
         shape = fit_gen_gamma_A$estimate["shape"], 
         scale = fit_gen_gamma_A$estimate["scale"], 
         k = fit_gen_gamma_A$estimate["k"])

# Collect AIC/BIC values
aic_values_A2 <- c(
  fit_normal_A$aic, fit_logistic_A$aic, fit_gen_gamma_A$aic
)
names(aic_values_A2) <- c("Normal", "Logistic", "Generalized Gamma")

bic_values_A2 <- c(
  fit_normal_A$bic, fit_logistic_A$bic, fit_gen_gamma_A$bic
)
names(bic_values_A2) <- c("Normal", "Logistic", "Generalized Gamma")

# Print results
print(aic_values_A2)
print(bic_values_A2)

###Gamma
simul_gamma_A <- rgamma(1000, shape = fit_gamma_A$estimate["shape"], 
                        rate = fit_gamma_A$estimate["rate"])
ggplot() +
  geom_density(data = data.frame(Value = Catcam_filtered$AADT), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_A), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Gamma Fit vs. Empirical Data", x = "AADT", y = "Density") +
  geom_vline(aes(xintercept = mean(Catcam_filtered$AADT)), col = "blue") +
  geom_vline(aes(xintercept = mean(simul_gamma_A)), col = "red")