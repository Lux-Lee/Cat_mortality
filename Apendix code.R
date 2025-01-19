Model1 <- glm(cbind(Ex_G, F_P) ~ Gender + Group + Period, data = Data, family = binomial)
Model2 <- glm(cbind(Ex_G, F_P) ~ Gender + Group * Period, data = Data, family = binomial)
Model3 <-glm(cbind(Ex_G, F_P) ~ Gender * Group * Period, data = Data, family = binomial)
Model4<- glm(Ex_G ~ offset(log(total)) + Gender + Group + Period, data = Data, family = poisson)
Model5<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Data, family = poisson)
Model6<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Data, family = poisson)
Model7<- glm.nb(Ex_G ~ offset(log(total)) + Gender + Group + Period, control=glm.control(maxit=1000), data = Data)
Model8<- glm.nb(Ex_G ~ offset(log(total)) + Gender + Group * Period, control=glm.control(maxit=1000), data = Data)
Model9<- glm.nb(Ex_G ~ offset(log(total)) + Gender * Group * Period, control=glm.control(maxit=1000), data = Data)
summary(Model6)
anova(Model5, Model6, test="Chisq")
pchisq(30.813,11, lower.tail=F)
Model, Res.Dev, Res.DF, P
Model1 256.8 12 5.22e-48
Model2 206.13, 11, 1.08e-17
Model3 185.9, 8, 5.93e-36
Model4 37.095, 12, 2.16e-4
Model5 30.813, 11 1.18e-3
Model6 25.015, 8, 1.55e-3
Model7 17.471, 12, 0.133
Model8 15.149, 11, 0.176
Model9 12.689, 8, 0.123

