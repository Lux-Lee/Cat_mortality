##Remove extreme outliers
Q1 <- 11
Q3 <- 20
IQR <- Q3 - Q1
lower_bound <- max(0, Q1 - 1.5 * IQR)
upper_bound <- Q3 + 1.5 * IQR
Catcam_red <- subset(Catcam_filtered,time >= lower_bound & time <= upper_bound)
freq_t_red <- Catcam_red %>%
  group_by(time) %>%
  summarise(frequency = n())
simul_time <- rtruncnorm(1000, a=0, mean = mean(Catcam_red$time), 
                         sd = sd(Catcam_red$time))
ggplot() +
  geom_density(data = data.frame(Value = Catcam_red$time), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_time), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Fitted Normal Distribution vs. Sample", x = "Time", y = "Density") +
  scale_fill_manual(values = c("blue", "red"))+
  geom_vline(aes(xintercept= mean(Catcam_red$time)), col="black") 
##freq >0
summary(Catcam_red$time)
freq_s <- Catcam_data %>%
  group_by(cat_id) %>%
  summarise(frequency = n())
Freq_s <-merge(freq_s, Subject, by="cat_id", all=FALSE)
summary(Freq_s$fre_week)
sd(Freq_s$fre_week)
sd(Freq_s$fre_week)/30
Q1<-2.0562
Q3<-9.8438
IQR <- Q3 - Q1
lower_bound <- max(0, Q1 - 1.5 * IQR)
upper_bound <- Q3 + 1.5 * IQR
Freq_s_red <- subset(Freq_s, fre_week >= lower_bound & fre_week <= upper_bound)
summary(Freq_s_red$fre_week)
sd(Freq_s_red$fre_week)
sd(Freq_s_red$fre_week)/sqrt(26)
###
# Fit Exponential
fit_exp_f <- fitdist(Subject$fre_week, "exp")
summary(fit_exp_f)
# Fit Poisson
fit_poisson_f <- fitdist(Subject$fre_week, "pois")
summary(fit_poisson_f)
# Fit Negative Binomial
fit_negbin_f <- fitdist(Subject$fre_week, "nbinom")
summary(fit_negbin_f)
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
# Anderson-Darling Test for Exponential
cvm.test(Subject$fre_week, pexp, 
         rate = fit_exp_f$estimate["rate"])