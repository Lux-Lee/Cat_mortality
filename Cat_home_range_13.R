##Home Range
Subject <- read_excel("C:/Users/95joo/OneDrive/School-OD/IBIO4521.2/DATA/Catcam_data.xlsx", sheet="Subject")
Subject$fre_day<- Subject$frequency/Subject$calendar_day
Subject$fre_week <-Subject$fre_day *7
Subject_r<- subset(Subject, Total_road != "NA")
Subject_r$frequency_r <- ifelse(Subject_r$frequency > 0, 1, 0)


sub_m1 <- glm(Subject_r$frequency_r ~ Major_road + Total_road + home_range_size_MCP, data = Subject_r, family = binomial)
summary(sub_m1)
