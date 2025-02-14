summary(Catcam_filtered$traffic)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.003472 0.007507 0.008718 0.010394 0.011458 0.033513 
sd(Catcam_filtered$traffic)
0.00558844
se=sd(Catcam_filtered$traffic)/sqrt(337)
0.0003044217

##Remove extreme outliers of traffic
Q1_t <- 0.007507
Q3_t <- 0.011458
IQR_t <- Q3_t - Q1_t
lower_bound_t <- max(0, Q1_t - 1.5 * IQR_t)
upper_bound_t <- Q3_t + 1.5 * IQR_t
Catcam_red_traffic <- subset(Catcam_filtered, traffic >= lower_bound & traffic <= upper_bound)
simul_traffic <- rgamma(1000, 
                        shape=((mean(Catcam_red_traffic$traffic))^2)/sd(Catcam_red_traffic$traffic)^2, 
                        scale=(sd(Catcam_red_traffic$traffic)^2)/mean(Catcam_red_traffic$traffic))

summary(Catcam_red_traffic$traffic)
sd(Catcam_red_traffic$traffic)
sd(Catcam_red_traffic$traffic)/sqrt(322)

ggplot() +
  geom_density(data = data.frame(Value = Catcam_red_traffic$traffic), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_traffic ), aes(x = Value), fill = "red", alpha = 0.5) +
  labs(title = "Fitted Normal Distribution vs. Sample", x = "Traffic", y = "Density") +
  scale_fill_manual(values = c("blue", "red"))+
  geom_vline(aes(xintercept= mean(Catcam_red_traffic$traffic)), col="black")

##Frequency > 0
Subject <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_data.xlsx", sheet="Subject")
Subject <- Subject %>% select(1,2,3,4)
Subject$fre_day<- Subject$frequency/Subject$calendar_day
Subject$fre_week <-Subject$fre_day *7
summary(Subject$fre_week)
sd(Subject$fre_week)
sd(Subject$fre_week)/sqrt(48)

Q1_s<-0
Q3_s<-4.167
IQR_s <- Q3_s - Q1_s
lower_bound_s <- max(0, Q1 - 1.5 * IQR_s)
upper_bound_s <- Q3 + 1.5 * IQR_s
Subject_red <- subset(Subject, fre_week >= lower_bound & fre_week <= upper_bound)
simul_freq <- rnorm(1000, mean = mean(Subject_red$fre_week), 
                    sd = sd(Subject_red$fre_week))
ggplot() +
  geom_density(data = data.frame(Value = Subject_red$fre_week), aes(x = Value), fill = "blue", alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_freq), aes(x = Value), fill = "red", alpha = 0.5) +
  labs(title = "Fitted Normal Distribution vs. Sample", x = "Frequency per week", y = "Density") +
  scale_fill_manual(values = c("blue", "red"))+
  geom_vline(aes(xintercept= mean(Subject_red$fre_week)), col="black")

summary(Subject_red$fre_week)
sd(Subject_red$frequency)
sd(Subject_red$frequency)/sqrt(40)
########################################
gamma_f<- function(x) {exp(-x)*dgamma(x, shape = fit_gamma_f$estimate["shape"], 
                                      rate = fit_gamma_f$estimate["rate"])*x}
expected_f <- integral(gamma_f, 0, Inf)
gamma_t<- function(x) {exp(-x)*dgamma(x, shape = fit_gamma_t$estimate["shape"], 
                                      rate = fit_gamma_t$estimate["rate"])*x}
expected_t <- integral(gamma_t, 0, Inf)
# Generate random values from Gamma distributions
F_samples <- rgamma(10000, shape = fit_gamma_f$estimate["shape"], rate = fit_gamma_f$estimate["rate"])
T_samples <- rgamma(10000, shape = fit_gamma_t$estimate["shape"], rate = fit_gamma_t$estimate["rate"])
summary(F_samples)
summary(T_samples)
Mortality <- (1 - exp(-(T_samples) * 0.769)) * 0.7
Mortality_w <-(1 - exp(-(T_samples) * 0.769))^(F_samples) * 0.7

summary(Mortality)
summary(Mortality_w)
sd(Mortality_w)

fit_gamma_m <- fitdist(Mortality, "gamma")
fit_gamma_w <- fitdist(Mortality_w, "gamma")

simul_gamma_m <- rgamma(1000, 
                        shape = fit_gamma_m$estimate["shape"], 
                        rate = fit_gamma_m$estimate["rate"])
simul_gamma_w <- rgamma(1000, 
                        shape = fit_gamma_w$estimate["shape"], 
                        rate = fit_gamma_w$estimate["rate"])
summary(fit_gamma_w)
summary(simul_gamma_m)
summary(simul_gamma_w)

ggplot() +
  geom_density(data = data.frame(Value = simul_gamma_m), aes(x = Value), fill = "blue", alpha = 0.5) +
  labs(title = "Gamma Fit vs. Empirical Data", x = "Mortality", y = "Density") +
  geom_vline(aes(xintercept = mean(simul_gamma_m)), col = "blue")

ggplot() +
  geom_density(data = data.frame(Value = simul_gamma_w), aes(x = Value), fill = "red", alpha = 0.3) +
  labs(title = "Gamma Fit vs. Empirical Data", x = "Mortality", y = "Density") +
  geom_vline(aes(xintercept = mean(simul_gamma_w)), col = "red")

Success_p<-exp(-expected_t*5)
print(Success_p)

mortality_df <- data.frame(
  Mortality = Mortality,
  Mortality_w = Mortality_w
)

#Per crossing
(1-Success_p)*0.7
#Per week
(1-(Success_p^expected_f))*0.7

## Mortality using v=13m/s, road width=10m

Success_p2<-exp(-expected_t*0.769)
print(Success_p2)
#Per crossing
(1-Success_p2)*0.7
#Per week
(1-(Success_p2^expected_f))*0.7

