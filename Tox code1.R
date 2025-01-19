##Fitting the data in different models from ssdtools##
eedexample_dists<-ssd_fit_dists(eedexample,dists=c("lnorm", "gamma", "invpareto",
                                                   "llogis", "lgumbel", "weibull", "gompertz"))
##Check which one fits the best##
eedexample_gof<-ssd_gof(eedexample_dists)
##comparing summary-Looking for the smallest aic##
##Objective way to check##
eedexample_gof[order(eedexample_gof$delta),]
##Re-fit the data to the best model##
eedexample_dists<-ssd_fit_dists(eedexample,dists=c("invpareto"))
eedexample_dists
##Divide the distribution in centiles##
set.seed(99)
eedexample_pred<-predict(eedexample_dists,ci=TRUE)
eedexample_pred
##Show me all-Most important is 95##
View(eedexample_pred)
##Grey area=Confidence interval##
eedplot<-ssd_plot(eedexample,eedexample_pred,xlab="Concentration (mg/L)",
                  ylab="Centile",ribbon=TRUE)
print(eedplot)
##Different way to show the plot##
eedplot<-ssd_plot(eedexample,eedexample_pred,xlab="Concentration (mg/L)",
                  ylab="Centile",ribbon=FALSE)
print(eedplot)
##Showing the 95% centile point##
eedplot<-ssd_plot(eedexample,eedexample_pred,xlab="Concentration (mg/L)",
                  ylab="Centile",ribbon=FALSE, hc=95)+expand_limits(x=100)
eedplot<-eedplot+geom_hcintersect(xintercept=c(3.0071156),yintercept=c(95)/100,
                                  colour="red",size=1)
eedplot<-ssd_plot(eedexample,eedexample_pred,xlab="Concentration (mg/L)",
                  ylab="Centile",ribbon=FALSE)+expand_limits(x=100)+geom_vline(xintercept = 3.09 ,
                                                                               color = "red",size=1)
