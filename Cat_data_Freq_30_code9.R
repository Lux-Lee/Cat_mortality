Freq_s <- Catcam_data %>%
  group_by(cat_id) %>%
  summarise(frequency = n())
summary(Freq_s$fre_week)
var(Freq_s$fre_week)
mean(Freq_s$fre_week)
Freq_s <-merge(Freq_s, Subject, by="cat_id", all=FALSE)

# Fit Gamma Distribution
fit_gamma_f2 <- fitdist(Freq_s$fre_week, "gamma")
summary(fit_gamma_f2)
# Fit Exponential
fit_exp_f2 <- fitdist(Freq_s$fre_week, "exp")
summary(fit_exp_f2)
# Fit Poisson
fit_poisson_f2 <- fitdist(Freq_s$fre_week, "pois")
summary(fit_poisson_f2)
# Fit Negative Binomial
fit_negbin_f2 <- fitdist(Freq_s$fre_week, "nbinom")
summary(fit_negbin_f2)

Freq_s$fre_week_r <-round(Freq_s$fre_week)
summary(Freq_s$fre_week_r)
var(Freq_s$fre_week_r)
mean(Freq_s$fre_week_r)

zinb_model_f2 <- zeroinfl(fre_week_r ~ 1, data = Freq_s, dist = "negbin")
summary(zinb_model_f2)
AIC(zinb_model_f2)
BIC(zinb_model_f2)

# AD Test for Poisson
ad.test(Freq_s$fre_week, ppois, lambda = fit_poisson_f2 $estimate)

# AD Test for Negative Binomial
ad.test(Freq_s$fre_week, pnbinom, size = fit_negbin_f2 $estimate["size"], 
        mu = fit_negbin_f2 $estimate["mu"])
# Anderson-Darling Test for Gamma
ad.test(Freq_s$fre_week, pgamma, 
        shape = fit_gamma_f2$estimate["shape"], 
        rate = fit_gamma_f2$estimate["rate"])
# Anderson-Darling Test for Exponential
ad.test(Freq_s$fre_week, pexp, 
        rate = fit_exp_f2$estimate["rate"])
# CvM Test for Poisson
cvm.test(Freq_s$fre_week, ppois, lambda = fit_poisson_f2 $estimate)

# CvM Test for Negative Binomial
cvm.test(Freq_s$fre_week, pnbinom, size = fit_negbin_f2 $estimate["size"], 
         mu = fit_negbin_f2 $estimate["mu"])
# Anderson-Darling Test for Gamma
cvm.test(Freq_s$fre_week, pgamma, 
         shape = fit_gamma_f2$estimate["shape"], 
         rate = fit_gamma_f2$estimate["rate"])
# Anderson-Darling Test for Exponential
cvm.test(Freq_s$fre_week, pexp, 
         rate = fit_exp_f2$estimate["rate"])

plot.legend3 <- c("Gamma", "Exponential", "Poisson", "Binomial")
denscomp(list(fit_gamma_f2, fit_exp_f2, fit_poisson_f2, fit_negbin_f2), legendtext = plot.legend3)
qqcomp(list(fit_gamma_f2, fit_exp_f2, fit_poisson_f2, fit_negbin_f2), legendtext = plot.legend3)
cdfcomp(list(fit_gamma_f2, fit_exp_f2, fit_poisson_f2, fit_negbin_f2), legendtext = plot.legend3)

aic_values_f2 <- c(fit_gamma_f2$aic, fit_exp_f2$aic, fit_poisson_f2$aic, fit_negbin_f2$aic)
names(aic_values_f2) <- c("Gamma", "Exponential", "Poisson", "Binomial")

bic_values_f2 <- c(fit_gamma_f2$aic, fit_exp_f2$aic, fit_poisson_f2$aic, fit_negbin_f2$aic)
names(bic_values_f2) <- c("Gamma", "Exponential", "Poisson", "Binomial")
print(aic_values_f2)
print(bic_values_f2)

######## Binomial ########
simul_gamma_f2 <- rlnorm(1000, 
                           meanlog = fit_lognorm_f2$estimate["meanlog"], 
                           sdlog = fit_lognorm_f2$estimate["sdlog"])

ggplot() +
  geom_f2ensity(data = data.frame(Value = Freq_s$fre_week), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_f2ensity(data = data.frame(Value = simul_lognorm_f2), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Log-Normal Fit vs. Empirical Data", x = "Duration", y = "Density") +
  geom_vline(aes(xintercept = mean(Freq_s$fre_week)), col = "black")

