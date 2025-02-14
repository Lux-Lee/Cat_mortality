dev.off() 
Q1_t <- 0.007507
Q3_t <- 0.011458
IQR_t <- Q3_t - Q1_t
lower_bound_t <- max(0, Q1_t - 1.5 * IQR_t)
upper_bound_t <- Q3_t + 1.5 * IQR_t
Catcam_red_rtraffic <- subset(Catcam_filtered, traffic >= lower_bound & traffic <= upper_bound)

# Fit Weibull
fit_weibull_rt <- fitdist(Catcam_red_rtraffic$traffic, "weibull")
summary(fit_weibull_rt)
# Fit Gamma
fit_gamma_rt <- fitdist(Catcam_red_rtraffic$traffic, "gamma")
summary(fit_gamma_rt)
# Fit Log-Normal
fit_lognorm_rt <- fitdist(Catcam_red_rtraffic$traffic, "lnorm")
summary(fit_lognorm_rt)
# Fit Exponential
fit_exp_rt <- fitdist(Catcam_red_rtraffic$traffic, "exp")
summary(fit_exp_rt)

plot.legend1 <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
denscomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt), legendtext = plot.legend1)
qqcomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt), legendtext = plot.legend1)
cdfcomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt), legendtext = plot.legend1)

plot.legend2 <- c("Weibull", "Gamma", "Log-Normal", "Exponential","Normal", "Logistic", "Generalized Gamma")
denscomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt, 
              fit_normal_rt, fit_logistic_rt, fit_gen_gamma_rt), legendtext = plot.legend2)
qqcomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt, 
            fit_normal_rt, fit_logistic_rt, fit_gen_gamma_rt), legendtext = plot.legend2)
cdfcomp(list(fit_weibull_rt, fit_gamma_rt, fit_lognorm_rt, fit_exp_rt, 
             fit_normal_rt, fit_logistic_rt, fit_gen_gamma_rt), legendtext = plot.legend2)


# Anderson-Darling Test for Weibull
ad.test(Catcam_red_rtraffic$traffic, pweibull, 
        shape = fit_weibull_rt$estimate["shape"], 
        scale = fit_weibull_rt$estimate["scale"])
# Anderson-Darling Test for Gamma
ad.test(Catcam_red_rtraffic$traffic, pgamma, 
        shape = fit_gamma$estimate["shape"], 
        rate = fit_gamma$estimate["rate"])
# Anderson-Darling Test for Log-Normal
ad.test(Catcam_red_rtraffic$traffic, plnorm, 
        meanlog = fit_lognorm_rt$estimate["meanlog"], 
        sdlog = fit_lognorm_rt$estimate["sdlog"])
# Anderson-Darling Test for Exponential
ad.test(Catcam_red_rtraffic$traffic, pexp, 
        rate = fit_exp_rt$estimate["rate"])

# Cramér–von Mises Test for Weibull
cvm.test(Catcam_red_rtraffic$traffic, pweibull, 
         shape = fit_weibull_rt$estimate["shape"], 
         scale = fit_weibull_rt$estimate["scale"])
# Cramér–von Mises Test for Gamma
cvm.test(Catcam_red_rtraffic$traffic, pgamma, 
         shape = fit_gamma_rt$estimate["shape"], 
         rate = fit_gamma_rt$estimate["rate"])
# Cramér–von Mises Test for Log-Normal
cvm.test(Catcam_red_rtraffic$traffic, plnorm, 
         meanlog = fit_lognorm_rt$estimate["meanlog"], 
         sdlog = fit_lognorm_rt$estimate["sdlog"])
# Cramér–von Mises Test for Exponential
cvm.test(Catcam_red_rtraffic$traffic, pexp, 
         rate = fit_exp_rt$estimate["rate"])

aic_values_rt <- c(fit_weibull_rt$aic, fit_gamma_rt$aic, fit_lognorm_rt$aic, fit_exp_rt$aic)
names(aic_values_rt) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")

bic_values_rt <- c(fit_weibull_rt$bic, fit_gamma_rt$bic, fit_lognorm_rt$bic, fit_exp_rt$bic)
names(bic_values_rt) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
print(aic_values_rt)
print(bic_values_rt)

##Different distribution
# Define Generalized Gamma functions
dgengamma <- function(x, shape, scale, k) dgamma(x, shape = shape, scale = scale)
pgengamma <- function(q, shape, scale, k) pgamma(q, shape = shape, scale = scale)
qgengamma <- function(p, shape, scale, k) qgamma(p, shape = shape, scale = scale)
# Manually define starting values based on rough estimates from Gamma fit
start_values <- list(shape = 2, scale = 1, k = 1)

# Fit Normal Distribution
fit_normal_rt <- fitdist(Catcam_red_rtraffic$traffic, "norm")
summary(fit_normal_rt)
# Fit Logistic Distribution
fit_logistic_rt <- fitdist(Catcam_red_rtraffic$traffic, "logis")
summary(fit_logistic_rt)

# Fit Generalized Gamma Distribution
fit_gen_gamma_rt <- fitdist(Catcam_red_rtraffic$traffic, "gengamma", start = start_values)
summary(fit_gen_gamma_rt)

# AD Test for Normal
ad.test(Catcam_red_rtraffic$traffic, pnorm, 
        mean = fit_normal_rt$estimate["mean"], 
        sd = fit_normal_rt$estimate["sd"])
# AD Test for Logistic
ad.test(Catcam_red_rtraffic$traffic, plogis, 
        location = fit_logistic_rt$estimate["location"], 
        scale = fit_logistic_rt$estimate["scale"])

# AD Test for Generalized Gamma
ad.test(Catcam_red_rtraffic$traffic, pgengamma, 
        shape = fit_gen_gamma_rt$estimate["shape"], 
        scale = fit_gen_gamma_rt$estimate["scale"], 
        k = fit_gen_gamma_rt$estimate["k"])

# CvM Test for Normal
cvm.test(Catcam_red_rtraffic$traffic, pnorm, 
         mean = fit_normal_rt$estimate["mean"], 
         sd = fit_normal_rt$estimate["sd"])
# CvM Test for Logistic
cvm.test(Catcam_red_rtraffic$traffic, plogis, 
         location = fit_logistic_rt$estimate["location"], 
         scale = fit_logistic_rt$estimate["scale"])

# CvM Test for Generalized Gamma
cvm.test(Catcam_red_rtraffic$traffic, pgengamma, 
         shape = fit_gen_gamma_rt$estimate["shape"], 
         scale = fit_gen_gamma_rt$estimate["scale"], 
         k = fit_gen_gamma_rt$estimate["k"])

# Collect AIC/BIC values
aic_values_rt2 <- c(
  fit_normal_rt$aic, fit_logistic_rt$aic, fit_gen_gamma_rt$aic
)
names(aic_values_rt2) <- c("Normal", "Logistic", "Generalized Gamma")

bic_values_rt2 <- c(
  fit_normal_rt$bic, fit_logistic_rt$bic, fit_gen_gamma_rt$bic
)
names(bic_values_rt2) <- c("Normal", "Logistic", "Generalized Gamma")

# Print results
print(aic_values_rt2)
print(bic_values_rt2)
