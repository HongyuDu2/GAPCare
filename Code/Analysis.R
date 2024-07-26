library(tidyverse)
library(dplyr)
library(readxl)
library(car)
library(jtools)
library(huxtable)
library(knitr)

#Import Dataset
Stroop_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Cognition/Cognition_stroop_new.csv")
Trailmaking_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Cognition/Cognition_trailmakingtest_new.csv")
GAPCare2 <- read.csv("/Users/dhyscuduke/Desktop/Data/GAPcare II Clinical Trial/GAPcareIIClinicalTri_DATA_LABELS_2023-02-09_1630.csv")
#ID_Data <- read_excel("/Users/dhyscuduke/Desktop/GAPcare II Login info - 02.15.2023.xlsx")
#write.csv(ID_Data, "/Users/dhyscuduke/Desktop/GAPcare II Login info - 02.15.2023.csv")
ID_Data <- read.csv("/Users/dhyscuduke/Desktop/GAPcare II Login info - 02.15.2023.csv")

#ID_Data <- select(ID_Data, REDCap.ID, ED.Apple.Watch.Fall.ID) 
#ID_Data <- data.frame(ID_Data$REDCap.ID, ID_Data$ED.Apple.Watch.Fall.ID)

#Stroop Baseline Screeded
R1 <- as.vector(Stroop_data_original$pt)
Stroop_data_original$selected_nums_stroop <- sub("(\\d{16}).*", "\\1", R1)
Baseline_stroop <- Stroop_data_original[Stroop_data_original$selected_nums_stroop
                                        %in% ID_Data$ED.Apple.Watch.Fall.ID, ]
ID1 <- as.vector(Baseline_stroop$selected_nums_stroop)[!duplicated(as.vector(Baseline_stroop$selected_nums_stroop))]
selected_ID_Data <- ID_Data[ID_Data$ED.Apple.Watch.Fall.ID %in% ID1,]


#Stroop Analysis
data.all <- Baseline_stroop

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

data.all.summ$congruent <- data.all.summ$match_text & data.all.summ$match_color

N1 <- data.all.summ %>% 
  group_by(pt) %>% 
  filter(congruent == "TRUE") %>% 
  summarise('1' = mean(time_diff))

N0 <- data.all.summ %>% 
  group_by(pt) %>% 
  filter(congruent == "FALSE") %>% 
  summarise('0' = mean(time_diff))

Baseline_stroop <- subset(Baseline_stroop, !duplicated(Baseline_stroop$pt))
Baseline_stroop$N1 <- N1$`1`
Baseline_stroop$N0 <- N0$`0`
Baseline_stroop$effect <- Baseline_stroop$N1 -  Baseline_stroop$N0

for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$ID[i] <- (selected_ID_Data 
                            %>% filter(ED.Apple.Watch.Fall.ID 
                                       == Baseline_stroop$selected_nums_stroop[i]))$REDCap.ID
}

#Filter Age, Gender, EDU, and Assistive Devices
GAPCare_Data_Stroop <- GAPCare2 %>% filter(Event.Name == 'ED Visit ') #Filter ED Visit
for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$Age[i] <- (GAPCare_Data_Stroop 
                            %>% filter(Record.ID 
                                       == Baseline_stroop$ID[i]))$How.old.is.the.patient.as.listed.in.the.chart.
}

for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$Gender[i] <- (GAPCare_Data_Stroop 
                             %>% filter(Record.ID 
                                        == Baseline_stroop$ID[i]))$What.is.the.gender.of.the.patient.as.listed.in.the.chart.
}

for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$Education[i] <- (GAPCare_Data_Stroop 
                                %>% filter(Record.ID 
                                           == Baseline_stroop$ID[i]))$What.is.the.highest.grade.or.year.of.school.you.completed.
}

for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$Device[i] <- (GAPCare_Data_Stroop 
                                %>% filter(Record.ID 
                                           == Baseline_stroop$ID[i]))$Do.you.use.any.special.equipment..such.as.a.cane..a.walker..a.wheelchair..a.special.bed..or.a.special.telephone..include.occasional.use.or.use.in.certain.circumstances..
}

for (i in 1:length(Baseline_stroop$pt)){
  Baseline_stroop$Intervention[i] <- (GAPCare_Data_Stroop 
                                %>% filter(Record.ID 
                                           == Baseline_stroop$ID[i]))$Randomize.Patient
}

#Change categorical values
Baseline_stroop$Gender[Baseline_stroop$Gender == 'Male'] <- 1
Baseline_stroop$Gender[Baseline_stroop$Gender == 'Female'] <- 0
Baseline_stroop$Device[Baseline_stroop$Device == 'Yes'] <- 1
Baseline_stroop$Device[Baseline_stroop$Device == 'No'] <- 0
Baseline_stroop$Education[Baseline_stroop$Education == 'Never attended school or only attendedkindergarten'] <- 1
Baseline_stroop$Education[Baseline_stroop$Education == 'Grades 1 through 8 (Elementary/middle school)'] <- 2
Baseline_stroop$Education[Baseline_stroop$Education == 'Grades 9 through 11 (Some high school)'] <- 3
Baseline_stroop$Education[Baseline_stroop$Education == 'Grades 12 or GED (High school graduate)'] <- 4
Baseline_stroop$Education[Baseline_stroop$Education == 'College 1 year to 3 years (Some college or technical school)'] <- 5
Baseline_stroop$Education[Baseline_stroop$Education == 'College 4 years or more (College graduate)'] <- 6
Baseline_stroop$Education[Baseline_stroop$Education == 'Do not know'] <- 998
Baseline_stroop$Education[Baseline_stroop$Education == 'Refused'] <- 999
Baseline_stroop$Intervention[Baseline_stroop$Intervention == 'Intervention Group'] <- 1
Baseline_stroop$Intervention[Baseline_stroop$Intervention == 'Control Group'] <- 0
Baseline_stroop$Gender <- as.numeric(Baseline_stroop$Gender)
Baseline_stroop$Device <- as.numeric(Baseline_stroop$Device)
Baseline_stroop$Education <- as.numeric(Baseline_stroop$Education)
Baseline_stroop$Intervention <- as.numeric(Baseline_stroop$Intervention)

#Missing Data Process
Baseline_stroop <- Baseline_stroop %>% filter(Education != 998)

#Create Demographic Tables
Baseline_stroop$Age_group <- cut(Baseline_stroop$Age, breaks = c(60, 75, 100))

Baseline_stroop %>%
  group_by(Gender, Education) %>%
  summarise(avg_effect = mean(effect), medium_effect = median(effect))

Baseline_stroop %>%
  group_by(Age_group, Education) %>%
  summarise(avg_effect = mean(effect), medium_effect = median(effect))

Baseline_stroop %>%
  group_by(Gender, Age_group) %>%
  summarise(avg_effect = mean(effect), medium_effect = median(effect))

#Histograms for variables
hist(Baseline_stroop$Age)
hist(Baseline_stroop$effect)
hist(Baseline_stroop$Gender)
hist(Baseline_stroop$Education)
hist(Baseline_stroop$Device)

#Regression models
model1 <- lm(effect ~ Age + Gender + Education, data=Baseline_stroop)
plot(model1, which = 1) #Residual plot
plot(model1, which = 2) #Normality plot

model2 <- lm(effect ~ Age + Gender + Education + Device, data=Baseline_stroop)
plot(model2, which = 1) #Residual plot
plot(model2, which = 2) #Normality plot

export_summs(model1, model2,model.names=c("Hypothesis 1","Hypothesis 4"))

###############################################
###############################################
###############################################

#Trailmaking Baseline Screeded
R2 <- as.vector(Trailmaking_data_original$pt)
Trailmaking_data_original$selected_nums_trail<- sub("(\\d{16}).*", "\\1", R2)
Baseline_trail <- Trailmaking_data_original[Trailmaking_data_original$selected_nums_trail
                                        %in% ID_Data$ED.Apple.Watch.Fall.ID, ]
ID2 <- as.vector(Baseline_trail$selected_nums_trail)[!duplicated(as.vector(Baseline_trail$selected_nums_trail))]
selected_ID_Data2 <- ID_Data[ID_Data$ED.Apple.Watch.Fall.ID %in% ID2,]

#Trailmaking Analysis


Baseline_trail<- Baseline_trail %>%
  group_by(pt) %>% slice(which.max(Timestamp))

for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$ID[i] <- (selected_ID_Data2 
                            %>% filter(ED.Apple.Watch.Fall.ID 
                                       == Baseline_trail$selected_nums_trail[i]))$REDCap.ID
}

#Filter Age, Gender, EDU, and Assistive Devices
GAPCare_Data_trail <- GAPCare2 %>% filter(Event.Name == 'ED Visit ') #Filter ED Visit
for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$Age[i] <- (GAPCare_Data_trail 
                             %>% filter(Record.ID 
                                        == Baseline_trail$ID[i]))$How.old.is.the.patient.as.listed.in.the.chart.
}

for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$Gender[i] <- (GAPCare_Data_trail 
                                %>% filter(Record.ID 
                                           == Baseline_trail$ID[i]))$What.is.the.gender.of.the.patient.as.listed.in.the.chart.
}

for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$Education[i] <- (GAPCare_Data_trail 
                                   %>% filter(Record.ID 
                                              == Baseline_trail$ID[i]))$What.is.the.highest.grade.or.year.of.school.you.completed.
}

for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$Device[i] <- (GAPCare_Data_trail 
                                %>% filter(Record.ID 
                                           == Baseline_trail$ID[i]))$Do.you.use.any.special.equipment..such.as.a.cane..a.walker..a.wheelchair..a.special.bed..or.a.special.telephone..include.occasional.use.or.use.in.certain.circumstances..
}

for (i in 1:length(Baseline_trail$pt)){
  Baseline_trail$Intervention[i] <- (GAPCare_Data_trail 
                                      %>% filter(Record.ID 
                                                 == Baseline_trail$ID[i]))$Randomize.Patient
}

#Change categorical values
Baseline_trail$Gender[Baseline_trail$Gender == 'Male'] <- 1
Baseline_trail$Gender[Baseline_trail$Gender == 'Female'] <- 0
Baseline_trail$Device[Baseline_trail$Device == 'Yes'] <- 1
Baseline_trail$Device[Baseline_trail$Device == 'No'] <- 0
Baseline_trail$Education[Baseline_trail$Education == 'Never attended school or only attendedkindergarten'] <- 1
Baseline_trail$Education[Baseline_trail$Education == 'Grades 1 through 8 (Elementary/middle school)'] <- 2
Baseline_trail$Education[Baseline_trail$Education == 'Grades 9 through 11 (Some high school)'] <- 3
Baseline_trail$Education[Baseline_trail$Education == 'Grades 12 or GED (High school graduate)'] <- 4
Baseline_trail$Education[Baseline_trail$Education == 'College 1 year to 3 years (Some college or technical school)'] <- 5
Baseline_trail$Education[Baseline_trail$Education == 'College 4 years or more (College graduate)'] <- 6
Baseline_trail$Education[Baseline_trail$Education == 'Do not know'] <- 998
Baseline_trail$Education[Baseline_trail$Education == 'Refused'] <- 999
Baseline_trail$Intervention[Baseline_trail$Intervention == 'Intervention Group'] <- 1
Baseline_trail$Intervention[Baseline_trail$Intervention == 'Control Group'] <- 0
Baseline_trail$Gender <- as.numeric(Baseline_trail$Gender)
Baseline_trail$Device <- as.numeric(Baseline_trail$Device)
Baseline_trail$Education <- as.numeric(Baseline_trail$Education)
Baseline_trail$Intervention <- as.numeric(Baseline_trail$Intervention)

#Missing Data Process
Baseline_trail <- Baseline_trail %>% filter(Education != 998)

#Create Demographic Tables
Baseline_trail$Age_group <- cut(Baseline_trail$Age, breaks = c(60, 75, 100))

Baseline_trail %>%
  group_by(Gender, Education) %>%
  summarise(avg_time = mean(Timestamp), medium_time = median(Timestamp))

Baseline_trail %>%
  group_by(Age_group, Education) %>%
  summarise(avg_time = mean(Timestamp), medium_time = median(Timestamp))

Baseline_trail %>%
  group_by(Gender, Age_group) %>%
  summarise(avg_time = mean(Timestamp), medium_time = median(Timestamp))

#Histograms for variables
hist(Baseline_trail$Age)
hist(Baseline_trail$Timestamp)
hist(Baseline_trail$Gender)
hist(Baseline_trail$Education)
hist(Baseline_trail$Device)


#Regression models
model3 <- lm(Timestamp ~ Age + Gender + Education, data=Baseline_trail)
plot(model1, which = 1) #Residual plot
plot(model1, which = 2) #Normality plot

model4 <- lm(Timestamp ~ Age + Gender + Education + Device, data=Baseline_trail)
plot(model2, which = 1) #Residual plot
plot(model2, which = 2) #Normality plot

export_summs(model3, model4,model.names=c("Hypothesis 1","Hypothesis 4"))




