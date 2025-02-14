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
