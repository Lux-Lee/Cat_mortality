library(readxl)
library(xlsx)
library(dplyr)                          
library(ggplot2)
library(tidyverse)
library(car)
Catcam_data <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_data.xlsx")

##Removing "NA" data in duration and AADT
Catcam_filtered <- subset(Catcam_data, Duration !="NA")
Catcam_filtered$Duration <- as.numeric(as.character(Catcam_filtered$Duration))
Catcam_filtered$time <-round(Catcam_filtered$Duration,digits=0)
Catcam_filtered <- subset(Catcam_filtered, AADT !="NA")
Catcam_filtered$AADT <- as.numeric(as.character(Catcam_filtered$AADT))
Catcam_filtered$traffic <- Catcam_filtered$AADT/(24*60*60)

##Mortality per crossing
Catcam_filtered$Success <- exp(-Catcam_filtered$traffic*Catcam_filtered$time)
Catcam_filtered$mortality <- 1-Catcam_filtered$Success

##Frequency of crossing and duration
freq_t <- Catcam_filtered %>%
  group_by(time) %>%
  summarise(frequency = n())

freq_s <- Catcam_data %>%
  group_by(cat_id) %>%
  summarise(frequency = n())

##Summary of values
summary(freq_s$frequency)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.00    4.00    6.00   12.53    9.00  103.00 
  sd(freq_s$frequency)
  19.31184
  se=sd(freq_s$frequency)/30
  0.643728
  
summary(Catcam_filtered$time)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.00   11.00   15.00   16.93   20.00   97.00 
  sd(Catcam_filtered$time)
  11.09303
  se=sd(Catcam_filtered$time)/sqrt(337)
  0.6042757

summary(Catcam_filtered$traffic)
  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  0.003472 0.007507 0.008718 0.010394 0.011458 0.033513 
  sd(Catcam_filtered$traffic)
  0.00558844
  se=sd(Catcam_filtered$traffic)/sqrt(337)
  0.0003044217
  
summary(Catcam_filtered$mortality)
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.02334 0.08349 0.12845 0.14588 0.18170 0.64755 
  sd(Catcam_filtered$mortality)
  0.09000111
  se=sd(Catcam_filtered$mortality)/sqrt(337)
  0.004902673

##Plot the duration distribution
freq<-ggplot(data=freq_t, aes(x = time, y = frequency))+
  labs(title="Data Distribution",
       x="Time", y="Frequency")
freq +geom_point(data=freq_t, aes(x = time, y = frequency), color="black") 
freq+ geom_area(color = "black", fill = "#00AFBB") +
  geom_vline(aes(xintercept= mean(Catcam_filtered$time)), col="blue") 

ggplot(freq_t, aes(x=frequency, y=time)) +
  geom_boxplot()

stats_t <- Catcam_filtered %>%
  summarize(
    mean = mean(time),
    se = sd(time) / sqrt(n()),
    .groups = 'drop'
  )
stats_tf <- freq_t %>%
  summarize(
    mean = mean(frequency),
    se = sd(frequency) / sqrt(n()),
    .groups = 'drop'
  )

##Average mortality per subject
Ave_filtered <- Catcam_filtered %>%
  group_by(cat_id) %>%
  summarise(mortality=mean(mortality, na.rm=T)) 
Ave_filtered$success <- 1-Ave_filtered$mortality
Ave_filtered$frequency <- freq_s$frequency
Ave_filtered$mortality_period <- 1-((Ave_filtered$success)^(Ave_filtered$frequency))

##Average mortality using subject average
mean(Ave_filtered$mortality)
  0.1692337
mean(Ave_filtered$frequency)
  12.53333
1-(mean(Ave_filtered$success)^mean(Ave_filtered$frequency))
  0.9020956
  
##Average mortality of data
mean(freq_s$frequency)
  12.53333
mean(Catcam_filtered$Success)
  0.8541241
1-(mean(Catcam_filtered$Success)^mean(freq_s$frequency))
  0.8614107