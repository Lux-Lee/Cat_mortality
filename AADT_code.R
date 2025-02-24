##AADT
Guelph_road <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Guelph_road.xlsx")
Kitchener_road <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Kitchener_road.xlsx")
Kitchener_road <-subset(Kitchener_road,SPEED<90)
Guelph_road$OPSCLASS[Guelph_road$OPSCLASS==0] <-6
Guelph_road$OPSCLASS<-as.factor(Guelph_road$OPSCLASS)
Kitchener_road$OPSCLASS<-as.factor(Kitchener_road$OPSCLASS)
Kitchener_road$LANES<-as.factor(Kitchener_road$LANES)
Guelph_road$LANES<-as.factor(Guelph_road$LANES)
Guelph_road$SPEED<-as.factor(Guelph_road$SPEED)
Kitchener_road$SPEED<-as.factor(Kitchener_road$SPEED)

Kit <-subset(Kit,ROADCLASS!="Ramp")

Kit_AADT_1<- lm(log(Aadt)~log(ROADLENGTH)+OPSCLASS+DEAD_END+ROADCLASS+LANES+TRUCKROUTE+STREET_TYPE+FLOW_DIRECTION+SPEED, data=Kitchener_road)
vif(Kit_AADT_1)
alias(Kit_AADT_1)

summary(Kit_AADT_1)
anova(Kit_AADT_1)

cor_matrix <- cor(model.matrix(Kit_AADT_1)[, -1])  # Exclude intercept
print(cor_matrix)
Kit_AADT_2<- lm(log(Aadt)~log(ROADLENGTH)+OPSCLASS+DEAD_END+ROADCLASS+LANES+TRUCKROUTE+STREET_TYPE+SPEED, data=Kitchener_road)
vif(Kit_AADT_2)
summary(Kit_AADT_2)
anova(Kit_AADT_2)

Kit_AADT_3<- lm(log(Aadt)~OPSCLASS+DEAD_END+ROADCLASS+LANES+TRUCKROUTE+STREET_TYPE+SPEED, data=Kitchener_road)
vif(Kit_AADT_3)
summary(Kit_AADT_3)
anova(Kit_AADT_3)

logLik(Kit_AADT_1)
AIC(Kit_AADT_1)
AIC(Kit_AADT_2)
AIC(Kit_AADT_3)

library(lmtest)
lrtest(Kit_AADT_2, Kit_AADT_3)



Kit_A<- lm(log(Aadt)~OPSCLASS+DEAD_END+ROADCLASS+LANES+STREET_TYPE, data=Kit)
vif(Kit_A)
summary(Kit_A)
anova(Kit_A)



common_data <- intersect(rownames(Kit_AADT_1$model), rownames(Kit_AADT_2$model))
model1_new <- lm(log(Aadt)~log(ROADLENGTH)+OPSCLASS+DEAD_END+ROADCLASS+LANES+TRUCKROUTE+STREET_TYPE+FLOW_DIRECTION+SPEED, data = full_data[common_data, ])
model2_new <- lm(log(Aadt)~OPSCLASS+DEAD_END+ROADCLASS+LANES+TRUCKROUTE+STREET_TYPE+SPEED, data = full_data[common_data, ])
Guelph_road$AADT <- exp(predict(Kit_AADT_2, newdata=Guelph_road))


Catcam_data <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_data.xlsx", sheet="AADT_1")

Catcam_data$AADT_1 <- exp(predict(Kit_AADT_2, newdata=Catcam_data))
# Install the package if you haven't already
install.packages("writexl")

# Load the package
library(writexl)

# Export to Excel
write_xlsx(Catcam_data, "Catcam_data_with_predictions.xlsx")
