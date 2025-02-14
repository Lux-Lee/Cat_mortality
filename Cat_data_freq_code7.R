Subject <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_fata.xlsx", sheet="Subject")
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
expected(fit_gamma_f)
# Fit Exponential
fit_exp_f <- fitdist(Subject$fre_week, "exp")
summary(fit_exp_f)
# Fit Poisson
fit_poisson_f <- fitdist(Subject$fre_week, "pois")
summary(fit_poisson_f)
# Fit Negative Binomial
fit_negbin_f <- fitdist(Subject$fre_week, "nbinom")
summary(fit_negbin_f)

Subject$fre_week_r <- round(Subject$fre_week)

# Fit Zero-Inflation Negative Binomial
zinb_model <- zeroinfl(fre_week_r ~ 1, data = Subject, dist = "negbin")
summary(zinb_model)
AIC(zinb_model)
BIC(zinb_model)
# AD Test for Poisson
ad.test(Subject$fre_week, ppois, lambda = fit_poisson_f $estimate)

# AD Test for Negative Binomial
ad.test(Subject$fre_week, pnbinom, size = fit_negbin_f $estimate["size"], 
        mu = fit_negbin_f$estimate["mu"])
# Anderson-Darling Test for Gamma
ad.test(Subject$fre_week, pgamma, 
        shape = fit_gamma_f$estimate["shape"], 
        rate = fit_gamma_f$estimate["rate"])
# Anderson-Darling Test for Exponential
ad.test(Subject$fre_week, pexp, 
        rate = fit_exp_f$estimate["rate"])

# Zero inflation negative binomial
size_param <- zinb_model$theta
mu_values <- predict(zinb_model, type = "response")

ad.test(Subject$fre_week_r, pnbinom, size = size_param, mu = mu_values)
cvm.test(Subject$fre_week_r, pnbinom, size = size_param, mu = mu_values)
# CvM Test for Poisson
cvm.test(Subject$fre_week, ppois, lambda = fit_poisson_f $estimate)

# CvM Test for Negative Binomial
cvm.test(Subject$fre_week, pnbinom, size = fit_negbin_f $estimate["size"], 
         mu = fit_negbin_f $estimate["mu"])
# Anderson-Darling Test for Gamma
cvm.test(Subject$fre_week, pgamma, 
        shape = fit_gamma_f$estimate["shape"], 
        rate = fit_gamma_f$estimate["rate"])
# Anderson-Darling Test for Exponential
cvm.test(Subject$fre_week, pexp, 
        rate = fit_exp_f$estimate["rate"])

plot.legend3 <- c("Gamma", "Exponential", "Poisson", "Binomial")
denscomp(list(fit_gamma_f, fit_exp_f, fit_poisson_f, fit_negbin_f), legendtext = plot.legend3)
qqcomp(list(fit_gamma_f, fit_exp_f, fit_poisson_f, fit_negbin_f), legendtext = plot.legend3)
cdfcomp(list(fit_gamma_f, fit_exp_f, fit_poisson_f, fit_negbin_f), legendtext = plot.legend3)

aic_values_f <- c(fit_gamma_f$aic, fit_exp_f$aic, fit_poisson_f$aic, fit_negbin_f$aic)
names(aic_values_f) <- c("Gamma", "Exponential", "Poisson", "Binomial")

bic_values_f <- c(fit_gamma_f$bic, fit_exp_f$bic, fit_poisson_f$bic, fit_negbin_f$bic)
names(bic_values_f) <- c("Gamma", "Exponential", "Poisson", "Binomial")
print(aic_values_f)
print(bic_values_f)
mean(simul_gamma_f)
simul_gamma_f <- rgamma(1000, 
                         shape = fit_gamma_f$estimate["shape"], 
                         rate = fit_gamma_f$estimate["rate"])
summary(simul_gamma_f)
ggplot() +
  geom_density(data = data.frame(Value = Subject$fre_week), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_f), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Gamma Fit vs. Empirical Data", x = "Frequency", y = "Density") +
  geom_vline(aes(xintercept = mean(Subject_red)), col = "blue")+
  geom_vline(aes(xintercept = mean(simul_gamma_f)), col = "red")
