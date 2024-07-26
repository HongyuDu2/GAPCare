library(tidyverse)
library(dplyr)

Stroop_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Cognition/Cognition_stroop_new.csv")
R <- as.vector(Stroop_data_original$pt)
ID <- R[!duplicated(R)]

data.all <- Stroop_data_original

data.all.summ = data.all%>%
  mutate(time_diff = as.numeric(as.character(endTime))-as.numeric(as.character(startTime)),
         match_text = colorSelected==text,
         match_color = colorSelected==color)
summ = data.all.summ %>%
  group_by(pt)%>%
  summarise(n= n(),
            avg_time_diff = mean(time_diff),
            match_text_p = sum(match_text)/n(),
            match_color_p = sum(match_color)/n(),
            match_both_p = sum(match_text*match_color)/n()
  )

N1 <- data.all.summ %>% group_by(pt) %>% 
  filter(match_text == "TRUE") %>% summarise('1' = mean(time_diff))

N0 <- data.all.summ %>% group_by(pt) %>% 
  filter(match_text == "FALSE") %>% summarise('0' = mean(time_diff))

#distribution of proportions of matched color, matched text, or matched both color and text for each patient. 
hist(summ$match_text_p)
hist(summ$match_color_p)
hist(summ$match_both_p)

#distribution of average time differences between end time and start time each patient. 

hist(summ$avg_time_diff,100)

#distribution of time differences between end time and start time each patient and each task. 
hist(data.all.summ$time_diff,100)


# a simple logistic regression shows that larger time difference is significantly associated with having a mismatched text result.  
model = glm(match_text~time_diff, data.all.summ, family='binomial')
summary(model)

model = glm(match_color~time_diff, data.all.summ, family='binomial')
summary(model)
