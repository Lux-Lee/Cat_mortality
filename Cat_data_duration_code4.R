dev.off() 

# Fit Weibull
fit_weibull_d <- fitdist(Catcam_filtered$Duration, "weibull")
summary(fit_weibull_d)
# Fit Gamma
fit_gamma_d <- fitdist(Catcam_filtered$Duration, "gamma")
summary(fit_gamma_d)
# Fit Log-Normal
fit_lognorm_d <- fitdist(Catcam_filtered$Duration, "lnorm")
summary(fit_lognorm_d)
# Fit Exponential
fit_exp_d <- fitdist(Catcam_filtered$Duration, "exp")
summary(fit_exp_d)

plot.legend <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
denscomp(list(fit_weibull_d, fit_gamma_d, fit_lognorm_d, fit_exp_d), legendtext = plot.legend)
qqcomp(list(fit_weibull_d, fit_gamma_d, fit_lognorm_d, fit_exp_d), legendtext = plot.legend)
cdfcomp(list(fit_weibull_d, fit_gamma_d, fit_lognorm_d, fit_exp_d), legendtext = plot.legend)


# Anderson-Darling Test for Weibull
ad.test(Catcam_filtered$Duration, pweibull, 
        shape = fit_weibull_d$estimate["shape"], 
        scale = fit_weibull_d$estimate["scale"])
# Anderson-Darling Test for Gamma
ad.test(Catcam_filtered$Duration, pgamma, 
        shape = fit_gamma_d$estimate["shape"], 
        rate = fit_gamma_d$estimate["rate"])
# Anderson-Darling Test for Log-Normal
ad.test(Catcam_filtered$Duration, plnorm, 
        meanlog = fit_lognorm_d$estimate["meanlog"], 
        sdlog = fit_lognorm_d$estimate["sdlog"])
# Anderson-Darling Test for Exponential
ad.test(Catcam_filtered$Duration, pexp, 
        rate = fit_exp_d$estimate["rate"])

# Cramér–von Mises Test for Weibull
cvm.test(Catcam_filtered$Duration, pweibull, 
         shape = fit_weibull_d$estimate["shape"], 
         scale = fit_weibull_d$estimate["scale"])
# Cramér–von Mises Test for Gamma
cvm.test(Catcam_filtered$Duration, pgamma, 
         shape = fit_gamma_d$estimate["shape"], 
         rate = fit_gamma_d$estimate["rate"])
# Cramér–von Mises Test for Log-Normal
cvm.test(Catcam_filtered$Duration, plnorm, 
         meanlog = fit_lognorm_d$estimate["meanlog"], 
         sdlog = fit_lognorm_d$estimate["sdlog"])
# Cramér–von Mises Test for Exponential
cvm.test(Catcam_filtered$Duration, pexp, 
         rate = fit_exp_d$estimate["rate"])

aic_values_d <- c(fit_weibull_d$aic, fit_gamma_d$aic, fit_lognorm_d$aic, fit_exp_d$aic)
names(aic_values_d) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")

bic_values_d <- c(fit_weibull_d$bic, fit_gamma_d$bic, fit_lognorm_d$bic, fit_exp_d$bic)
names(bic_values_d) <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
print(aic_values_d)
print(bic_values_d)

######## Log-Normal ########
simul_lognorm_d <- rlnorm(1000, 
                        meanlog = fit_lognorm_d$estimate["meanlog"], 
                        sdlog = fit_lognorm_d$estimate["sdlog"])
summary(simul_lognorm_d)
ggplot() +
  geom_density(data = data.frame(Value = Catcam_filtered$Duration), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_lognorm_d), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Log-Normal Fit vs. Empirical Data", x = "Duration", y = "Density") +
  geom_vline(aes(xintercept = mean(Catcam_filtered$Duration)), col = "black")
