snail <- read_excel("C:/Users/95joo/Desktop/snail.xlsx")
snail.data<- data.frame(snail[2],snail[7])
snail_w <- snail.data %>%                         
  mutate(
    Temperature = factor(Temperature))
names(snail_w)[2]<-paste("Food intake per weight (g)")
Temperature14<- subset(snail_w, Temperature=="14")
Temperature28<- subset(snail_w, Temperature=="28")
rownames(Temperature14) <- NULL
rownames(Temperature28) <- NULL
mean(Temperature14$`Food intake per weight (g)`)
mean(Temperature28[2])
std <- function(x) sd(x)/sqrt(length(x))
std(Temperature14[2])
std(Temperature28[2])
