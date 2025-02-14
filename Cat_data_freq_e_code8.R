Subject <- Subject %>% select_at(vars(1:4))
Subject$fre_day<- Subject$frequency/Subject$calendar_day
Subject$fre_week <-Subject$fre_day *7
Subject$fre_week <- as.numeric(Subject$fre_week)
summary(Subject$fre_week)
Subject_red <- Subject$fre_week+0.000001

summary(Subject_red)
var(Subject_red)
mean(Subject_red)
# Fit Gamma Distribution
fit_gamma_fr <- fitdist(Subject_red, "gamma")
summary(fit_gamma_fr)
# Fit Exponential
fit_exp_fr <- fitdist(Subject_red, "exp")
summary(fit_exp_fr)
# Fit Log-Normal
fit_lognorm_fr <- fitdist(Subject_red, "lnorm")
summary(fit_lognorm_fr)

# Anderson-Darling Test for Gamma
ad.test(Subject_red, pgamma, 
        shape = fit_gamma_fr$estimate["shape"], 
        rate = fit_gamma_fr$estimate["rate"])
# cvm Test for Gamma
cvm.test(Subject_red, pgamma, 
         shape = fit_gamma_fr$estimate["shape"], 
         rate = fit_gamma_fr$estimate["rate"])
# Anderson-Darling Test for Log-Normal
ad.test(Subject_red, plnorm, 
        meanlog = fit_lognorm_fr$estimate["meanlog"], 
        sdlog = fit_lognorm_fr$estimate["sdlog"])
cvm.test(Subject_red, plnorm, 
         meanlog = fit_lognorm_fr$estimate["meanlog"], 
         sdlog = fit_lognorm_fr$estimate["sdlog"])

plot.legend4 <- c("Gamma", "Exponential", "Log-norm")
denscomp(list(fit_gamma_fr, fit_exp_fr, fit_lognorm_fr), legendtext = plot.legend4)
qqcomp(list(fit_gamma_fr, fit_exp_fr, fit_lognorm_fr), legendtext = plot.legend4)
cdfcomp(list(fit_gamma_fr, fit_exp_fr, fit_lognorm_fr), legendtext = plot.legend4)

aic_values_fr <- c(fit_gamma_fr$aic, fit_exp_fr$aic, fit_lognorm_fr$aic)
names(aic_values_fr) <- c("Gamma", "Exponential", "Log-norm")

bic_values_fr <- c(fit_gamma_fr$bic, fit_exp_fr$bic, fit_lognorm_fr$bic)
names(bic_values_fr) <- c("Gamma", "Exponential", "Log-norm")
print(aic_values_fr)
print(bic_values_fr)

######## Gamma ########
simul_gamma_fr <- rgamma(1000, 
                          shape = fit_gamma_fr$estimate["shape"], 
                          rate = fit_gamma_fr$estimate["rate"])
summary(simul_gamma_fr)
ggplot() +
  geom_density(data = data.frame(Value = Subject_red), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_fr), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Gamma", x = "Duration", y = "Density") +
  geom_vline(aes(xintercept = mean(Subject_red)), col = "black")
