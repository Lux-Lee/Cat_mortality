##Load Package and Data##
library(dplyr)                          
library(openxlsx)                      
library(nnet) 
library(readxl)
library(emmeans)
library(ggplot2)

##Theme##
library(gridExtra)
theme_black <- function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      line= element_line(color="white"),
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"), 
      legend.axis.line = element_line(color="white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

##Data##
Conv_Data<-read_excel("Proj_data.xlsx", sheet = "Sheet1")
##Convert Data Format##
Conv_Data_R<- Conv_Data %>%
  mutate(
    Ex_G = round(Ex_G, digits=0),
    F_P = round(F_P, digits=0),
    total = Ex_G+F_P
  )
Conv_Data_R <- subset(Conv_Data_R, select=-c(Total, Percentage))

Conv_Data_F<- Conv_Data_R %>%
  mutate(
    Year = factor(Year),
    Period = factor(Period, levels =c("Pre","Post")),
    Gender = factor(Gender, levels = c("Women", "Men")),
    Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )
Data<- Conv_Data_F
##Logistic Model and Comparison##
glm1 <- glm(cbind(Ex_G, F_P) ~ Gender + Group + Period, data = Conv_Data_F, family = binomial)
summary(glm1)
glm2 <- glm(cbind(Ex_G, F_P) ~ Gender + Group * Period, data = Conv_Data_F, family = binomial)
summary(glm2)
glm3 <-glm(cbind(Ex_G, F_P) ~ Gender * Group * Period, data = Conv_Data_F, family = binomial)
summary(glm3)
anova(glm1, glm2, test="Chisq")
##Residual plot##
f1 <- fitted(glm2)
res<-residuals(glm2, "pearson")
Res<-data.frame(f1,res)
plot(f1,res)
ggplot(data=Res, aes(x=f1, y=res)) +
  labs(title="Pearson Residual vs Fitted of full data",
       x="Predicted Values", y="Residual Values") +
  geom_point(data=df,aes(x=x, y=y), color="white") +
  geom_path(data=df, aes(x=x, y=y), color="#00FF0066", size=2)+
  geom_point(color="black", aes(x=f1, y=res))+
  geom_line(y=0, col="red", aes(x=f1, y=res))
##Model fit##
pchisq(185.9, 8, lower.tail = F)
185.9/8
##Poisson Rate model and comparison##
glm_p1<- glm(Ex_G ~ offset(log(total)) + Gender + Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p1)
glm_p2<- glm(Ex_G ~ offset(log(total)) + Gender * Group * Period, data = Conv_Data_F, family = poisson)
summary(glm_p2)
anova(glm_p1, glm_p2, test="Chisq")
##Poisson residual plot##
f2 <- fitted(glm_p1)
res2<-residuals(glm_p1, "pearson")
Res2<-data.frame(f2,res2)
plot(f2,res)
ggplot(data=Res2, aes(x=f2, y=res2)) +
  labs(title="Pearson Residual vs Fitted of Poisson model",
       x="Predicted Values", y="Residual Values") +
  geom_point(color="black", aes(x=f2, y=res2))+
  geom_line(y=0, col="red", aes(x=f2, y=res2))
##Model fit##
pchisq(30.813,11, lower.tail=F)
30.813/11
##negative binomial and Comparison##
library(MASS)
nb_glm1 <- glm.nb(Ex_G ~ offset(log(total)) + Gender + Group * Period, 
                  control=glm.control(maxit=1000), data = Conv_Data_F)
summary(nb_glm1)
nb_glm2 <- glm.nb(Ex_G ~ offset(log(total)) + Gender * Group * Period, 
                  control=glm.control(maxit=1000), data = Conv_Data_F)
summary(nb_glm2)
anova(nb_glm1, nb_glm2, test="Chisq")
##negative binomial residual plot##
f3 <- fitted(nb_glm1)
res3<-residuals(nb_glm1, "pearson")
Res3<-data.frame(f3,res3)
plot(f3,res3)
ggplot(data=Res3, aes(x=f3, y=res3)) +
  labs(title="Pearson Residual vs Fitted of Negative Binomial model",
       x="Predicted Values", y="Residual Values") +
  geom_point(color="black", aes(x=f3, y=res3))+
  geom_line(y=0, col="red", aes(x=f3, y=res3))
##Model fit##
pchisq(15.149, 11, lower.tail=F)
15.149/11
##EMM and plots##
emm_nb1<- emmeans(nb_glm1, ~Gender + Period + Group)
summary(nb_glm1)
emmnb1_D<- data.frame(emm_nb1)
emm_nb2<- emmeans(nb_glm1, ~ Period + Group)
plot(emm_nb2, xlab= "Estimated Marginal Mean",ylab="Category", type="response")
emmnb1_D<-data.frame(emmnb1_D[1:4])
emmnb1_D <-emmnb1_D %>%
  mutate(Period = factor(Period, levels =c("Pre","Post")),
         Gender = factor(Gender, levels = c("Women", "Men")),
         Group = factor(Group, levels = c("Canadian", "Indigenous"))
  )
emmnb1_D <-emmnb1_D %>%
  group_by(Gender, Group)
ggplot(data=emmnb1_D, aes(color=interaction(Gender, Group), x=Period, y=emmean)) +
  labs(title="COVID-19 impact on Perceived Health",
       x="Period", y="Estimated Marginal Mean", color="Gender.Group") +
  geom_point() +
  geom_line(data=subset(emmnb1_D, Group=="Canadian"),aes(group=Gender)) +
  geom_line(data=subset(emmnb1_D, Group=="Indigenous"),aes(group=Gender)) 
