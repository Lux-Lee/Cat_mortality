##Picloram##
Picloram_Data$Proportion <- Picloram_Data$Number.Died/Picloram_Data$Total.Exposed
plot(Picloram_Data$Dose,Picloram_Data$Proportion, xlab="Dose(unit)", 
     ylab="Proportion of death", main="Proportion of death vs Dose")
Picloram.model1 <-lm(Proportion~Dose, data=Picloram_Data)
summary(Picloram.model1)
abline(Picloram.model1, col="blue")
Picloram.model2 <-glm(cbind(Number.Died, Number.Survived)~Dose, data=Picloram_Data,
                     family=binomial)
summary(Picloram.model2)

Dose.value <- seq(-1,5,0.005)
Pro.value <- ilogit(Picloram.model2$coefficients[1]+Picloram.model2$coefficients[2]*Dose.value)
lines(Dose.value, Pro.value, col="red")
dose.p(Picloram.model2,p=0.5)
dose.p(Picloram.model2,p=0.9)
points(1.36479, 0.5, pch=15, col="red")
points(2.54078, 0.9, pch=15, col="red")

predict(Picloram.model1, newdata=data.frame(Dose=(2.2)))
ilogit(Picloram.model2$coefficients[1]+Picloram.model2$coefficients[2]*2.2)
pnorm(predict(Picloram.model3, newdata=data.frame(Dose=2.2)))
pnorm(0.7142237)
library(faraway)
##Picloram Probit##
plot(Picloram_Data$Dose,Picloram_Data$Proportion, xlab="Dose(unit)", 
     ylab="Proportion of death", main="Proportion of death vs Dose")
Picloram.model3 <-glm(cbind(Number.Died, Number.Survived)~Dose, data=Picloram_Data,
                      family=binomial(link=probit))
summary(Picloram.model3)
Pro.value2 <- pnorm(Picloram.model3$coefficients[1]+Picloram.model3$coefficients[2]*Dose.value)
lines(Dose.value, Pro.value2, col="blue")
dose.p(Picloram.model3, p=0.5)
dose.p(Picloram.model3, p=0.9)

points(1.429759,0.5, pch=17, col="blue")
points(2.811824,0.9, pch=17, col="blue")

legend("bottomright", legend=c("Logit","Probit"), pch=c(15,17), lty=c(1,1), 
       col=c("red","blue"))

##West Nile Virus##
WNV.model1 <-glm(No.HorseCase~offset(log(No.Farms))+No.BirdCase+Area+Population, family=poisson, data=WNV_Data)
summary(WNV.model1)
WNV.model2 <-glm(No.HorseCase~offset(log(No.Farms))+No.BirdCase+Area, family=poisson, data=WNV_Data)
summary(WNV.model2)
anova(WNV.model2,WNV.model1, test="Chisq")
predict.glm(WNV.model2, newdata=data.frame(No.Farms=(1000), No.BirdCase=(14), 
                                             Area=(1500), Population=(150000)), type="link")
exp(2.194599)
###Metribuzin###
##SSD##
Metribuzin_fitS<-ssd_fit_dists(Metribuzin_Species,dists=c("lnorm", "gamma", "invpareto","burrIII3",
                                                          "llogis_llogis","lnorm_lnorm",
                                                          "llogis", "lgumbel", "weibull", "gompertz"))
Metribuzin_fitS
theme_set(theme_bw())
autoplot(Metribuzin_fitS, delta=30)
Metribuzin_gofS<-ssd_gof(Metribuzin_fitS)
Metribuzin_gofS[order(Metribuzin_gofS$aic),]
Metribuzin_fitS_B <-ssd_fit_dists(Metribuzin_Species,dists=c("lnorm_lnorm"))
coef(Metribuzin_fitS_B)
Metribuzin_hc5<-ssd_hc(Metribuzin_fitS_B,ci=TRUE)
Metribuzin_hc5
--
set.seed(99)
Metribuzin_predS<-predict(Metribuzin_fitS_B,ci=TRUE)
Metribuzin_predS
--
MetplotS<-ssd_plot(Metribuzin_Species,Metribuzin_predS, color="Group",label="Species",
                   xlab="Concentration (mg/L)",ribbon=TRUE, hc=NULL)+expand_limits(x=10000)
print(MetplotS)
MetplotS2<-MetplotS+geom_hcintersect(xintercept=c(9.49),yintercept=c(5)/100,
                                       colour="red", size=1)
##EED##
Metribuzin_fitE<-ssd_fit_dists(Metribuzin_Exposure,dists=c("lnorm", "gamma", "invpareto","burrIII3",
                                                            "llogis_llogis","lnorm_lnorm",
                                                            "llogis", "lgumbel", "weibull", "gompertz"))
Metribuzin_fitE
Metribuzin_gofE<-ssd_gof(Metribuzin_fitE)
Metribuzin_gofE[order(Metribuzin_gofE$aic),]
theme_set(theme_bw())
autoplot(Metribuzin_fitE, delta=5400)
Metribuzin_fitE_B<- ssd_fit_dists(Metribuzin_Exposure, dists=c("lnorm_lnorm"))
coef(Metribuzin_fitE_B)
set.seed(99)
Metribuzin_predE<-predict(Metribuzin_fitE_B,ci=TRUE)
Metribuzin_predE[95,]
MetplotE<-ssd_plot(Metribuzin_Exposure,Metribuzin_predE,xlab="Concentration (mg/L)",
                  ylab="Centile",ribbon=TRUE)
MetplotE2<- MetplotE+geom_hcintersect(xintercept=c(0.432),yintercept=c(95)/100,
                                      colour="red", size=1)
##Risk##
Risk_data <- data.frame(Conc = exp(seq(log(.0001), log(100), .1)))
Risk_data$Y<-ssd_plnorm_lnorm(Risk_data$Conc, meanlog1=-4.45, meanlog2=-2.61, pmix=0.515,
                              sdlog1=0.597, sdlog2=1.4)
Met_Risk<- MetplotS2+geom_line(data=ex.cdf,aes(x=Conc,y=ex.cdf),color="red",size=1)+
  annotate("text", label=paste("Exposure distribution"),x = 0.8 *ex.cdf$Conc[which.max(Risk_data$Y > 0.2)], 
           y = 0.55, angle = 85)+geom_vline(xintercept=0.432,color="blue",lty=2,size=1)+
  geom_vline(xintercept=9.49,color="green",lty=2,size=1)+annotate("text", label=paste("95th centile"),
                                                                  x=1, y=0.85, angle=90)+
  annotate("text", label=paste("HC5"), x=30, y=0.90,angle=90)
