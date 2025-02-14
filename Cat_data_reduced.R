##Remove extreme outliers
Q1 <- 11
Q3 <- 20
IQR <- Q3 - Q1
lower_bound <- max(0, Q1 - 1.5 * IQR)
upper_bound <- Q3 + 1.5 * IQR
Catcam_red <- subset(Catcam_filtered,time >= lower_bound & time <= upper_bound)

##Mortality per crossing
Catcam_red$success <- exp(-Catcam_red$traffic*Catcam_red$time)
Catcam_red$mortality <- (1-Catcam_red$success)*0.7

##Frequency of duration
freq_t_red <- Catcam_red %>%
  group_by(time) %>%
  summarise(frequency = n())

##Summary of values
summary(Catcam_red$time)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.00   10.00   14.00   14.71   19.00   33.00 
  sd(Catcam_red$time)
  6.645329
  se=sd(Catcam_red$time)/sqrt(314)
  0.3750176

summary(Catcam_red$traffic)
  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  0.003472 0.007507 0.008718 0.010495 0.011458 0.033513 
  sd(Catcam_red$traffic)
  0.005726133
  se=sd(Catcam_red$traffic)/sqrt(314)
  0.0003231444

summary(Catcam_red$mortality)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.01634 0.05806 0.08581 0.09226 0.11783 0.31707 
  sd(Catcam_red$mortality)
  0.04769421
  se=sd(Catcam_red$mortality)/sqrt(314)
  0.00269154

##Plot the duration distribution
freq_red<-ggplot(data=freq_t_red, aes(x = time, y = frequency))+
  labs(title="Data Distribution",
       x="Time", y="Frequency")
freq_red +geom_point(data=freq_t_red, aes(x = time, y = frequency), color="black") 
freq_red+ geom_area(color = "black", fill = "#00AFBB") +
  geom_vline(aes(xintercept= mean(Catcam_red$time)), col="blue") 


ggplot(freq_t_red, aes(x=frequency, y=time)) +
  geom_boxplot()

stats_t_red <- Catcam_red %>%
  summarize(
    mean = mean(time),
    se = sd(time) / sqrt(n()),
    .groups = 'drop'
  )
stats_tf_red <- freq_t_red %>%
  summarize(
    mean = mean(frequency),
    se = sd(frequency) / sqrt(n()),
    .groups = 'drop'
  )

##Average mortality per subject
Ave_red <- Catcam_red %>%
  group_by(cat_id) %>%
  summarise(success=mean(success, na.rm=T)) 
Ave_red$mortality <- (1-Ave_red$success)*0.7
Ave_red$frequency <- freq_s$frequency
Ave_red$mortality_period <- (1-((Ave_red$success)^(Ave_red$frequency)))*0.7

##Average mortality using subject average
mean(Ave_red$mortality)
  0.09770635
mean(Ave_red$frequency)
  12.53333
(1-(mean(Ave_red$success)^mean(Ave_red$frequency)))*0.7
  0.5936347

##Average mortality of data
mean(freq_s$frequency)
  12.53333
mean(Catcam_red$Success)
  0.8682042
(1-(mean(Catcam_red$Success)^mean(freq_s$frequency)))*0.7
  0.5809236
##Average mortality
(1-(mean(Catcam_red$Success)^mean(freq_s$frequency)))*0.7
  0.5809236