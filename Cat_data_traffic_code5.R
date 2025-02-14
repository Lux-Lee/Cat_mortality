dev.off() 

# Fit Weibull
fit_weibull_t <- fitdist(Catcam_filtered$traffic, "weibull")
summary(fit_weibull_t)
# Fit Gamma
fit_gamma_t <- fitdist(Catcam_filtered$traffic, "gamma")
summary(fit_gamma_t)
# Fit Log-Normal
fit_lognorm_t <- fitdist(Catcam_filtered$traffic, "lnorm")
summary(fit_lognorm_t)
# Fit Exponential
fit_exp_t <- fitdist(Catcam_filtered$traffic, "exp")
summary(fit_exp_t)

plot.legend2 <- c("Weibull", "Gamma", "Log-Normal", "Exponential","Normal", "Logistic", "Generalized Gamma")
denscomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t, 
              fit_normal_t, fit_logistic_t, fit_gen_gamma_t), legendtext = plot.legend2)
qqcomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t, 
            fit_normal_t, fit_logistic_t, fit_gen_gamma_t), legendtext = plot.legend2)
cdfcomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t, 
             fit_normal_t, fit_logistic_t, fit_gen_gamma_t), legendtext = plot.legend2)


# Anderson-Darling Test for Weibull
ad.test(Catcam_filtered$traffic, pweibull, 
        shape = fit_weibull_t$estimate["shape"], 
        scale = fit_weibull_t$estimate["scale"])
# Anderson-Darling Test for Gamma
ad.test(Catcam_filtered$traffic, pgamma, 
        shape = fit_gamma$estimate["shape"], 
        rate = fit_gamma$estimate["rate"])
# Anderson-Darling Test for Log-Normal
ad.test(Catcam_filtered$traffic, plnorm, 
        meanlog = fit_lognorm_t$estimate["meanlog"], 
        sdlog = fit_lognorm_t$estimate["sdlog"])
# Anderson-Darling Test for Exponential
ad.test(Catcam_filtered$traffic, pexp, 
        rate = fit_exp_t$estimate["rate"])

# Cramér–von Mises Test for Weibull
cvm.test(Catcam_filtered$traffic, pweibull, 
         shape = fit_weibull_t$estimate["shape"], 
         scale = fit_weibull_t$estimate["scale"])
# Cramér–von Mises Test for Gamma
cvm.test(Catcam_filtered$traffic, pgamma, 
         shape = fit_gamma_t$estimate["shape"], 
         rate = fit_gamma_t$estimate["rate"])
# Cramér–von Mises Test for Log-Normal
cvm.test(Catcam_filtered$traffic, plnorm, 
         meanlog = fit_lognorm_t$estimate["meanlog"], 
         sdlog = fit_lognorm_t$estimate["sdlog"])
# Cramér–von Mises Test for Exponential
cvm.test(Catcam_filtered$traffic, pexp, 
         rate = fit_exp_t$estimate["rate"])

aic_values_t <- c(fit_weibull_t$aic, fit_gamma_t$aic, fit_lognorm_t$aic, fit_exp_t$aic)
names(aic_values_t) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")

bic_values_t <- c(fit_weibull_t$bic, fit_gamma_t$bic, fit_lognorm_t$bic, fit_exp_t$bic)
names(bic_values_t) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
print(aic_values_t)
print(bic_values_t)

##Different distribution
# Define Generalized Gamma functions
dgengamma <- function(x, shape, scale, k) dgamma(x, shape = shape, scale = scale)
pgengamma <- function(q, shape, scale, k) pgamma(q, shape = shape, scale = scale)
qgengamma <- function(p, shape, scale, k) qgamma(p, shape = shape, scale = scale)
# Manually define starting values based on rough estimates from Gamma fit
start_values <- list(shape = 2, scale = 1, k = 1)

# Fit Normal Distribution
fit_normal_t <- fitdist(Catcam_filtered$traffic, "norm")
summary(fit_normal_t)
# Fit Logistic Distribution
fit_logistic_t <- fitdist(Catcam_filtered$traffic, "logis")
summary(fit_logistic_t)

# Fit Generalized Gamma Distribution
fit_gen_gamma_t <- fitdist(Catcam_filtered$traffic, "gengamma", start = start_values)
summary(fit_gen_gamma_t)

# AD Test for Normal
ad.test(Catcam_filtered$traffic, pnorm, 
        mean = fit_normal_t$estimate["mean"], 
        sd = fit_normal_t$estimate["sd"])
# AD Test for Logistic
ad.test(Catcam_filtered$traffic, plogis, 
        location = fit_logistic_t$estimate["location"], 
        scale = fit_logistic_t$estimate["scale"])

# AD Test for Generalized Gamma
ad.test(Catcam_filtered$traffic, pgengamma, 
        shape = fit_gen_gamma_t$estimate["shape"], 
        scale = fit_gen_gamma_t$estimate["scale"], 
        k = fit_gen_gamma_t$estimate["k"])

# CvM Test for Normal
cvm.test(Catcam_filtered$traffic, pnorm, 
         mean = fit_normal_t$estimate["mean"], 
         sd = fit_normal_t$estimate["sd"])
# CvM Test for Logistic
cvm.test(Catcam_filtered$traffic, plogis, 
         location = fit_logistic_t$estimate["location"], 
         scale = fit_logistic_t$estimate["scale"])

# CvM Test for Generalized Gamma
cvm.test(Catcam_filtered$traffic, pgengamma, 
         shape = fit_gen_gamma_t$estimate["shape"], 
         scale = fit_gen_gamma_t$estimate["scale"], 
         k = fit_gen_gamma_t$estimate["k"])

# Collect AIC/BIC values
aic_values_t2 <- c(
  fit_normal_t$aic, fit_logistic_t$aic, fit_gen_gamma_t$aic
)
names(aic_values_t2) <- c("Normal", "Logistic", "Generalized Gamma")

bic_values_t2 <- c(
  fit_normal_t$bic, fit_logistic_t$bic, fit_gen_gamma_t$bic
)
names(bic_values_t2) <- c("Normal", "Logistic", "Generalized Gamma")

# Print results
print(aic_values_t2)
print(bic_values_t2)

###Gamma
simul_gamma_t <- rgamma(1000, shape = fit_gamma_t$estimate["shape"], 
                   rate = fit_gamma_t$estimate["rate"])
ggplot() +
  geom_density(data = data.frame(Value = Catcam_filtered$traffic), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_t), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Gamma Fit vs. Empirical Data", x = "traffic", y = "Density") +
  geom_vline(aes(xintercept = mean(Catcam_red_rtraffic$traffic)), col = "black")
