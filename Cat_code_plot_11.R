##PLOT##

##AADT log transformation plot
ggplot() +
  geom_density(data = data.frame(Value = Kitchener_road$Aadt, Group="AADT"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = Kitchener_road$ROADLENGTH, Group="Road Length"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  labs(title = " ", x = "Parameter Value", y = "Density", fill=" ") +
  xlim(0,5000)+ylim(0,0.005)+
  scale_fill_manual(values = c("AADT" = "blue", "Road Length" = "red")) +
  theme_bw()+
  theme(
    legend.position = c(0.98, 0.98),  # Moves legend inside top-right corner
    legend.justification = c(1.2, 1.2),   # Aligns legend box to top-right
    plot.margin = margin(10, 10, 10, 10)  # Keeps enough space around the plot
  )

ggplot() +
  geom_density(data = data.frame(Value = log(Kitchener_road$Aadt), Group="AADT"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = log(Kitchener_road$ROADLENGTH), Group="Road Length"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  labs(title = " ", x = "Log Transformed Value", y = "Density", fill=" ") +
  scale_fill_manual(values = c("AADT" = "blue", "Road Length" = "red")) +
  theme_bw()+
  theme(
    legend.position = c(0.98, 0.98),  # Moves legend inside top-right corner
    legend.justification = c(1.2, 1.2),   # Aligns legend box to top-right
    plot.margin = margin(10, 10, 10, 10)  # Keeps enough space around the plot
  )

##
plot.legend2 <- c("Weibull", "Gamma", "Log-Normal", "Exponential")
denscomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t), 
         fitlwd = 2, legendtext = plot.legend2, xlegend = "topright",
         main=" ", 
         xlab="Traffic Volume per Second", 
         ylab="Probability Density")
cdfcomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t), 
        fitlwd = 2, legendtext = plot.legend2, xlegend = "bottomright",
        main=" ", 
        xlab="Traffic Volume per Second", 
        ylab="Cumulative Probability")
qqcomp(list(fit_weibull_t, fit_gamma_t, fit_lognorm_t, fit_exp_t), 
       fitpch=19, legendtext = plot.legend2, xlegend = "bottomright",
       main=" ", 
       xlab="Theoretical Quantiles", 
       ylab="Empirical Quantiles")

ggplot() +
  geom_density(data = data.frame(Value = Catcam_filtered$traffic, Group="Observed Data"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_lognorm_t, Group="Simulated Data"), 
               aes(x = Value, fill = Group), alpha = 0.5) +
  labs(title = " ", x = "Vehicle per second", y = "Density", fill="Dataset") +
  geom_vline(aes(xintercept = mean(Catcam_filtered$traffic)), 
             col = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(simul_lognorm_t)), 
             col = "red", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Observed Data" = "blue", "Simulated Data" = "red")) +
  theme_bw()+
  theme(
    legend.position = c(0.98, 0.98),  # Moves legend inside top-right corner
    legend.justification = c(1.2, 1.2),   # Aligns legend box to top-right
    plot.margin = margin(10, 10, 10, 10)  # Keeps enough space around the plot
  )
########
ggplot() +
  geom_density(data = data.frame(Value = Subject$fre_week, Group="Observed Data"), 
               aes(x = Value, fill=Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_gamma_f, Group="Gamma"), 
               aes(x = Value, fill=Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_zaga_f, Group="ZAGA"), 
               aes(x = Value, fill=Group), alpha = 0.5) +
  geom_density(data = data.frame(Value = simul_ziga, Group="ZIGA"), 
               aes(x = Value, fill=Group), alpha = 0.5) +
  scale_fill_manual(values = c("Observed Data" = "blue", "Gamma" = "red", "ZAGA"="green", "ZIGA"="grey")) +
  labs(title = " ", x = "Frequency per week", y = "Density", fill="Dataset") +
  geom_vline(aes(xintercept = mean(Subject$fre_week)), col = "blue", linetype = "dashed", linewidth = 1)+
  geom_vline(aes(xintercept = mean(simul_gamma_f)), col = "red", linetype = "dashed", linewidth = 1)+
  geom_vline(aes(xintercept = mean(simul_zaga_f)), col = "green", linetype = "dashed", linewidth = 1)+
  geom_vline(aes(xintercept = mean(simul_ziga)), col = "grey", linetype = "dashed", linewidth = 1)+
  xlim(0,50)+ylim(0,0.175)+
  theme_bw()+
  theme(
    legend.position = c(0.98, 0.98),  # Moves legend inside top-right corner
    legend.justification = c(1.2, 1.2),   # Aligns legend box to top-right
    plot.margin = margin(10, 10, 10, 10)  # Keeps enough space around the plot
  )
###################
# Load necessary library
library(ggplot2)
library(gridExtra)

# Function to create density plot comparisons
plot_mortality <- function(data1, data2, title, xlabel) {
  ggplot() +
    geom_density(aes(x = data1), fill = "blue", alpha = 0.5) +
    geom_density(aes(x = data2), fill = "red", alpha = 0.5) +
    labs(title = title, x = xlabel, y = " ") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Generate plots
p1 <- plot_mortality(Mortality_o_1, Mortality_o_2, 
                     "Observed Mortality per Crossing", "Mortality Probability")
p2 <- plot_mortality(Mortality_ow_1, Mortality_ow_2, 
                     "Observed Weekly Mortality", "Mortality Probability")
p3 <- plot_mortality(Mortality_1, Mortality_2, 
                     "Simulated Mortality per Crossing", "Mortality Probability")
p4 <- plot_mortality(Mortality_w_1, Mortality_w_2, 
                     "Simulated Weekly Mortality", "Mortality Probability")
p5 <- plot_mortality(Mortality_s_1, Mortality_s_2, 
                     "Simulated 6-Month Mortality", "Mortality Probability")
p6 <- plot_mortality(Mortality_os_1, Mortality_os_2, 
                     "Observed 6-Month Mortality", "Mortality Probability")

# Arrange and display plots in a grid
grid.arrange(p3, p1, p4, p2, p5, p6, ncol = 2)

