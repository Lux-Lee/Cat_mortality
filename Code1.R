library(dplyr)                          
library(openxlsx)                      
library(nnet) 
library(readxl)
library(ggplot2)
library(tidyr)
snail <- read_excel("C:/Users/95joo/Desktop/snail.xlsx")
snail.data<- data.frame(snail[2:7])
snail <- snail.data %>%                         
  mutate(
    Temperature = factor(Temperature))
names(snail)[2]<-paste("snail_w")
names(snail)[3]<-paste("food_pre")
names(snail)[4]<-paste("food_pro")
names(snail)[5]<-paste("food")
names(snail)[6]<-paste("f_perw")
tem14<- subset(snail, Temperature=="14")
tem28<- subset(snail, Temperature=="28")
snailw<- lm(f_perw~Temperature, data=snail)
summary(snailw)
snail_r<-lm(`Food intake per weight (g)` ~ Temperature, data=snail_w)
summary(aov(snail_r))

## mean and SD##
summary(tem14$f_perw)
mean(tem14$f_perw)
summary(tem28$f_perw)
var(tem14$f_perw)
var(tem28$f_perw)
std <- function(x) sd(x)/sqrt(length(x))
std(tem14$f_perw)
std(tem28$f_perw)
## Plot ##
plot(f_perw~Temperature, xlab="Temperature ('C)", ylab="Lettuce consumption per weight (g)",
     main="Lettuce consumption vs Temperature", data = snail)
##sparrow
sparrow1 <- read_excel("C:/Users/95joo/Desktop/snail.xlsx", 
                       sheet = "Sheet1")
sparrow <- sparrow1 %>%
  mutate(
    Species=factor(Species),
    Treatment=factor(Treatment)
  )
House<- subset(sparrow, Species == "House Sparrow")
Tree<- subset(sparrow, Species == "Tree Sparrow")
rownames(House) <- NULL
rownames(House) <- NULL
ggplot(sparrow, aes(x=Treatment, y=Proportion, fill=Species)) +
  geom_boxplot()
stats <- sparrow %>%
  group_by(Treatment, Species) %>%
  summarize(
    mean = mean(Proportion),
    se = sd(Proportion) / sqrt(n()),
    .groups = 'drop'
  )
ggplot(sparrow, aes(x = Treatment, y = Proportion, fill = Species))+
  geom_boxplot() +
  geom_text(data = stats,
            aes(x = Treatment, y = 15,
                label = paste("Mean:", round(mean, 2), "\nSE:", round(se, 2))),
            position = position_dodge(width = 0.75),  # Adjusts text horizontally to place it on the right
            size = 2) +
  labs(x = "Treatment", y = "Proportion")
summary(House)
House_R<-lm(Proportion~Treatment, data=House)
Tree_R<-lm(Proportion~Treatment, data=Tree)
summary(House_R)
summary(Tree_R)
summary(lm(Tree$Proportion~Tree$Treatment))
