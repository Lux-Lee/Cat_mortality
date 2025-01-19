#### GAM Workshop #################
# Sarah Mueller
# Nov 4, 2024

library(tidyverse)
library(mgcv) # for fitting gams and related functions
library(gratia) # for better visualization of GAM results

# Example from Pedersen et al. 2019

# Using the built-in CO2 dataset in R, which gives uptake of CO2 in grasses under 
# varying CO2 concentrations, with plants from two locations (Mississippi and Quebec)
# and two temp treatments; 12 plants, 7 CO2 concentrations for each plant

#The default CO2 plant variable is ordered;
#This recodes it to an unordered factor (see main text for why)
CO2 <- transform(CO2, Plant_uo=factor(Plant, ordered=FALSE))

# CO2 data example ----------

# plot CO2 uptake vs. CO2 concentration for each plant
(CO2_vis_plot <- ggplot(CO2, aes(x=conc, 
                                 y=uptake, 
                                 group=Plant,
                                 color=Type, 
                                 lty=Plant)) +
   geom_point() +
   geom_line() +
   scale_color_manual(values = rep(c("red","blue","black"), times =4))+
   scale_linetype_manual(values = rep(1:4, each=3))+
   labs(x=expression(CO[2] ~ concentration ~ (mL ~ L^{-1})), 
        y=expression(CO[2] ~ uptake ~ (mu*mol ~ m^{-2}))))

##### First model ---------

# model log uptake as a function of two smoothers: 
# thin plate spline of log concentration and RE for individual-plant-specific intercepts
CO2_mod1 <- gam(log(uptake) ~ s(log(conc), k=5, bs="tp") +
                  s(Plant_uo, k=12, bs="re"),
                data=CO2, method="REML", family="gaussian")

# plot the default gratia plot for the CO2 model
draw(CO2_mod1)
# left: estimated smoother for effect log CO2 concentration on CO2 uptake
# right: QQ plot of the estimated random effects vs. Gaussian quantiles

# get the model summary
summary(CO2_mod1)

#### Interpreting the summary
# edfs represent complexity of the smooth; edf = 1 is a straight line, higher edfs mean more wiggly

# Interpretation of significance: a significant smooth is one where you can't 
# draw a horizontal line through the 95% CI
# high edf =/= significance, and vice versa
# a smooth can be linear and significant, non-linear and non-significant, or one of each

# check the model
gam.check(CO2_mod1) 
# makes 4 plots, scroll through them (qq plot, residuals vs. linear predictor, histogram of resids, response vs. fitted)
# alternate form of the 4 plots from gratia package:
appraise(CO2_mod1)
# just the k part of gam.check
k.check(CO2_mod1)
# k for log concentration smooth is probably too low
table(CO2$conc)
# but there are only 7 unique values of concentration, so k can be no larger than 7


# setup prediction data (all possible concentration values for all individual plants)
CO2_mod1_pred <- with(CO2,
                      expand.grid(conc=seq(min(conc), max(conc), length=100),
                                  Plant_uo=levels(Plant_uo)))

# make the prediction, add this and a column of standard errors to the prediction
# data.frame. Predictions are on the log scale.
CO2_mod1_pred <- cbind(CO2_mod1_pred,
                       predict(CO2_mod1, 
                               CO2_mod1_pred, 
                               se.fit=TRUE))

# Plot back-transformed model predictions of CO2 uptake by concentration for each plant
ggplot(data=CO2, aes(x=conc, y=uptake, group=Plant_uo)) +
  facet_wrap(~Plant_uo) +
  geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=conc),
              data=CO2_modG_pred, 
              alpha=0.3, 
              inherit.aes=FALSE) +
  geom_line(aes(y=exp(fit)), data=CO2_modG_pred) +
  geom_point() +
  labs(x=expression(CO[2] ~ concentration ~ (mL ~ L^{-1})),
       y=expression(CO[2] ~ uptake ~ (mu*mol ~ m^{-2})))
# different intercepts but same shape for all plants
# functional responses among plants are similar, but model doesn't capture all the patterns
# e.g. it underestimates for plant Qc2

##### Second model ----------

# fit another model adding on individual-plant-level smoothers
# allows shape of the uptake vs. concentration curve to vary between plants (but same wiggliness)
CO2_mod2 <- gam(log(uptake) ~ s(log(conc), k=5, m=2) + # the "global" smooth for uptake vs. conc
                   s(log(conc), Plant_uo, k=5,  bs="fs", m=2), # the individual-level smooths
                 data=CO2, method="REML")

# gratia draw() plot for CO2_modGS
draw(CO2_mod2)
# left: "global" smooth for uptake vs. conc
# right: individual adjustments for uptake vs. conc relationship
# plants differ in both average log uptake and shape of functional response

# model summary
summary(CO2_mod2)

# check model
gam.check(CO2_mod2)
appraise(CO2_mod2)

# generate and plot predictions
CO2_mod2_pred <- predict(CO2_mod2, se.fit=TRUE)
CO2 <- transform(CO2, 
                 mod2 = CO2_mod2_pred$fit, 
                 mod2_se = CO2_mod2_pred$se.fit)
ggplot(data=CO2, aes(x=conc, y=uptake, group=Plant_uo)) +
  facet_wrap(~Plant_uo) +
  geom_ribbon(aes(ymin=exp(mod2-2*mod2_se),
                  ymax=exp(mod2+2*mod2_se)), alpha=0.25) +
  geom_line(aes(y=exp(mod2))) +
  geom_point() +
  labs(x=expression(CO[2] ~ concentration ~ (mL ~ L^{-1})),
       y=expression(CO[2] ~ uptake ~ (mu*mol ~ m^{-2})))
# predictions fit each individual plant better now

##### Third model ----------

# Same as second model, but now individual-level smoothers can have *different* wiggliness
CO2_mod3 <- gam(log(uptake) ~ s(log(conc), k=5, m=2, bs="tp") +
                   s(log(conc), by = Plant_uo, k=5, m=1, bs="tp") + # m = 1, so penalize squared first derivative
                   s(Plant_uo, bs="re", k=12), # must explicitly include random intercept
                 data=CO2, method="REML")

# view the smooths
draw(CO2_mod3)
draw(CO2_mod3, select = c(1,14,8,2,11,5), scales = "fixed")

# model summary
summary(CO2_mod3)

# generate and plot predictions
CO2_mod3_pred <- predict(CO2_mod3, se.fit=TRUE)
CO2 <- transform(CO2, 
                 mod3 = CO2_mod3_pred$fit, 
                 mod3_se = CO2_mod3_pred$se.fit)
ggplot(data=CO2, aes(x=conc, y=uptake, group=Plant_uo)) +
  facet_wrap(~Plant_uo) +
  geom_ribbon(aes(ymin=exp(mod3-2*mod3_se),
                  ymax=exp(mod3+2*mod3_se)), alpha=0.25) +
  geom_line(aes(y=exp(mod3))) +
  geom_point() +
  labs(x=expression(CO[2] ~ concentration ~ (mL ~ L^{-1})),
       y=expression(CO[2] ~ uptake ~ (mu*mol ~ m^{-2})))
# the curves don't really differ much in their wiggliness, so this model is probably overkill

##### Compare models --------

AIC(CO2_mod1, CO2_mod2, CO2_mod3)




# Another GAM example ------

# From Noam Ross GAMs in R course https://noamross.github.io/gams-in-r-course/

##### Motorcycle crash data: linear approach ------

# fit linear model to crash data that models acceleration of a crash test dummy head as a function of time

# get the data and look at it
mcycle <- MASS::mcycle
head(mcycle)
plot(mcycle$accel~mcycle$times) # very non-linear, dips sharply down, then up, then back to 0

# fit a linear model
lm_mod <- lm(accel ~ times, data = mcycle)

# visualize the model
termplot(lm_mod, partial.resid = T, se = T)
# a very bad fit

##### Motorcycle crash data: non-linear approach ------

# fit a model to the crash data where accel has a smooth, nonlinear relationship with time

gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

summary(gam_mod)

# plot the results
plot(gam_mod, residuals = T, pch = 1) # mgcv plotting function
draw(gam_mod, residuals = T) # gratia plotting function

# extract the model coefficients of the GAM
coef(gam_mod)
# the smooth is made up of 9 basis functions, so model has 9 coefficients for the smooth

# extract the smoothing parameter
gam_mod$sp # 0.00078

# usually gam() estimates the smoothing parameter, lambda, for you...
# but you can set it with sp = ?, in the smoother or for the whole model

# try a higher lambda
gam_mod1 <- gam(accel ~ s(times, sp = 0.1), data = mcycle, method = "REML")
draw(gam_mod1, residual = T) # obviously not right, too smooth

# try a smaller lambda
draw(gam(accel ~ s(times, sp = 0.0001), data = mcycle, method = "REML"), residual = T)

# setting the number of basis functions, k
draw(gam(accel ~ s(times, k = 3), data = mcycle, method = "REML"), residuals = T) # k too small
draw(gam(accel ~ s(times, k = 10), data = mcycle, method = "REML"), residuals = T) # large enough k
draw(gam(accel ~ s(times, k = 20), data = mcycle, method = "REML"), residuals = T) # about the same result, even though k larger

# change both lambda (0.0001) and k (50)
draw(gam(accel ~ s(times, sp = 0.0001, k = 50), data = mcycle, method = "REML"), residuals = T) # overfit

