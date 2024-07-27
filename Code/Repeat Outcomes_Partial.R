# Import Packages will be usedin this study
# For Different Tables and Figures
library(dplyr)

# Impoort data set
setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcareI_R03_DATA_2022-10-21_1144_deidentified.csv')


##########################################################
# Important variable in Paper Can an Emergency Department#
# Initiated Intervention Prevent Subsequent Falls and    #
# Health Care Use in Older Adults? A Randomized          #
# Controlled Trial                                       #
##########################################################

##Important!! Control_data, Inter_data, and Total_data only choose from the first time
##Total_Data consider all times

# Create the two 55 patients groups based on intervention
Control_data <- DATA %>% filter(intervention_random == "0")
Inter_data <- DATA %>% filter(intervention_random == "1")
Total_data <- rbind(Control_data, Inter_data)

#Dim of Usual care, Intervention, and together
dim(Control_data)
dim(Inter_data)
dim(Total_data)

#ID of Usual care, Intervention, and together
id_Control <- Control_data$record_id
id_Inter <- Inter_data$record_id
id_Total <- Total_data$record_id

#Filter all data based on times in usual care, intervention, and together
Control_data_com <- DATA %>% filter(record_id %in% Control_data$record_id) 
Inter_data_com <- DATA %>% filter(record_id %in% Inter_data$record_id) 
Total_Data <- DATA %>% filter(record_id %in% id_Total) 
Intervention <- Total_Data$intervention_random

# Sex
dim(Control_data %>% filter(gender_chart == "1"))
dim(Inter_data %>% filter(gender_chart == "1"))
Total_Sex <- Total_Data$gender_chart

#Age
median(Control_data$age)
summary(Control_data$age)

median(Inter_data$age)
summary(Inter_data$age)

Total_Age <- Total_Data$age

#Race
length((Control_data %>% filter(race_chart == "1"))$race_chart) #White
length((Inter_data %>% filter(race_chart == "1"))$race_chart) #White
length((Control_data %>% filter(race_chart == "2"))$race_chart) #Black
length((Inter_data %>% filter(race_chart == "2"))$race_chart) #Black
length((Control_data %>% filter(race_chart == "5"))$race_chart) #AIAN
length((Inter_data %>% filter(race_chart == "5"))$race_chart) #AIAN
length((Control_data %>% filter(race_chart == "8"))$race_chart) #White and Asian
length((Inter_data %>% filter(race_chart == "8"))$race_chart) #White and Asian
length((Control_data %>% filter(race_chart == "17"))$race_chart) #Other
length((Inter_data %>% filter(race_chart == "17"))$race_chart) #Other
length((Control_data %>% filter(race_chart == "999"))$race_chart) #Refused
length((Inter_data %>% filter(race_chart == "999"))$race_chart) #Refused

Total_Race <- Total_Data$race_chart

#Ethnicity
length((Control_data %>% filter(ethnicity == "1"))$ethnicity) #Hispanic
length((Inter_data %>% filter(ethnicity == "1"))$ethnicity) #Hispanic
length((Control_data %>% filter(ethnicity == "2"))$ethnicity) #Non
length((Inter_data %>% filter(ethnicity == "2"))$ethnicity) #Non

Total_enthbicity <- Total_Data$ethnicity

#Education
length((Control_data %>% filter(education == "2"))$education) #1-8
length((Inter_data %>% filter(education == "2"))$education) #1-8
length((Control_data %>% filter(education == "3"))$education) #9-11
length((Inter_data %>% filter(education == "3"))$education) #9-11
length((Control_data %>% filter(education == "4"))$education) #12
length((Inter_data %>% filter(education == "4"))$education) #12
length((Control_data %>% filter(education == "5"))$education) #1-3
length((Inter_data %>% filter(education == "5"))$education) #1-3
length((Control_data %>% filter(education == "6"))$education) #>=4
length((Inter_data %>% filter(education == "6"))$education) #>=4

Total_education <- Total_Data$education

#Number of falls in previous 3 months
Control_data$no_falls
Inter_data$no_falls
Total_no_falls <- Total_Data$no_falls

#Number of injurious falls in previous 3 months
Control_data$no_falls_injured
Inter_data$no_falls_injured
Total_no_falls_injured <- Total_Data$no_falls_injured


#Charlson comorbidity index
Total_cci_mi <- Total_Data$cci_mi
Total_cci_hf <-Total_Data$cci_hf
Total_cci_pvd <- Total_Data$cci_pvd
Total_cci_copd <- Total_Data$cci_copd
Total_cci_emph <- Total_Data$cci_emph
Total_cci_ulcer <- Total_Data$cci_ulcer
Total_cci_liver <- Total_Data$cci_liver
Total_cci_hep <- Total_Data$cci_hep
Total_cci_stroke <- Total_Data$cci_stroke
Total_cci_hemi <- Total_Data$cci_hemi
Total_cci_dem <- Total_Data$cci_dem
Total_cci_rharth <- Total_Data$cci_rharth
Total_cci_contis <- Total_Data$cci_contis
Total_cci_bone <- Total_Data$cci_bone
Total_cci_kidney <- Total_Data$cci_kidney
Total_cci_dialysis <- Total_Data$cci_dialysis
Total_cci_diab <- Total_Data$cci_diab
Total_cci_hiv <- Total_Data$cci_hiv
Total_cci <- Total_cci_mi + Total_cci_hf+ Total_cci_pvd + Total_cci_copd + Total_cci_emph + Total_cci_ulcer + Total_cci_liver + Total_cci_hep+ Total_cci_stroke + Total_cci_hemi + Total_cci_dem + Total_cci_rharth + Total_cci_contis + Total_cci_bone + Total_cci_kidney + Total_cci_dialysis+ Total_cci_diab + Total_cci_hiv

#Activity of Daily Living
Total_bi_bowels <- Total_Data$bi_bowels
Total_bi_bladder <- Total_Data$bi_bladder
Total_bi_feeding <- Total_Data$bi_feeding
Total_bi_grooming <- Total_Data$bi_grooming
Total_bi_dressing <- Total_Data$bi_dressing
Total_bi_transfer <- Total_Data$bi_transfer
Total_bi_toilet <- Total_Data$bi_toilet
Total_bi_mobility <- Total_Data$bi_mobility
Total_bi_stairs <- Total_Data$bi_stairs
Total_bi_bathing <- Total_Data$bi_bathing
Total_bi_score <- Total_Data$bi_score

#Injured during index fall
Total_hurt <- Total_Data$hurt
Total_injuries_1 <- Total_Data$injuries___1
Total_injuries_2 <- Total_Data$injuries___2
Total_injuries_3 <- Total_Data$injuries___3
Total_injuries_4 <- Total_Data$injuries___4
Total_injuries_5 <- Total_Data$injuries___5
Total_injuries_6 <- Total_Data$injuries___6
Total_injuries <- Total_injuries_1 + Total_injuries_2 + Total_injuries_3 + Total_injuries_4 + Total_injuries_5 + Total_injuries_6

#Hospital Site
Total_Hospital <- Total_Data$hospital

#Previous equipment use
Total_equ <- Total_Data$equip

#fall related ED visits for Fall log
ED1 <- Total_Data$hc_f1___2
ED2 <- Total_Data$hc_f2___2
ED3 <- Total_Data$hc_f3___2
ED4 <- Total_Data$hc_f4___2
ED5 <- Total_Data$hc_f5___2
ED6 <- Total_Data$hc_f6___2
ED7 <- Total_Data$hc_f7___2
ED8 <- Total_Data$hc_f8___2
ED9 <- Total_Data$hc_f9___2
ED10 <- Total_Data$hc_f10___2
ED11 <- Total_Data$hc_f11___2
ED12 <- Total_Data$hc_f12___2
ED13 <- Total_Data$hc_f13___2
ED14 <- Total_Data$hc_f14___2
ED15 <- Total_Data$hc_f15___2
ED16 <- Total_Data$hc_f16___2
ED17 <- Total_Data$hc_f17___2
ED18 <- Total_Data$hc_f18___2
ED19 <- Total_Data$hc_f19___2
ED20 <- Total_Data$hc_f20___2
ED21 <- Total_Data$hc_f21___2
ED22 <- Total_Data$hc_f22___2
ED23 <- Total_Data$hc_f23___2
ED24 <- Total_Data$hc_f24___2
ED25 <- Total_Data$hc_f25___2
ED26 <- Total_Data$hc_f26___2
ED27 <- Total_Data$hc_f27___2
ED28 <- Total_Data$hc_f28___2
ED29 <- Total_Data$hc_f29___2
ED30 <- Total_Data$hc_f30___2
ED31 <- Total_Data$hc_f31___2
ED32 <- Total_Data$hc_f32___2
ED33 <- Total_Data$hc_f33___2
ED34 <- Total_Data$hc_f34___2
ED35 <- Total_Data$hc_f35___2
ED36 <- Total_Data$hc_f36___2
ED37 <- Total_Data$hc_f37___2
ED38 <- Total_Data$hc_f38___2
ED39 <- Total_Data$hc_f39___2
ED40 <- Total_Data$hc_f40___2
ED41 <- Total_Data$hc_f41___2
ED42 <- Total_Data$hc_f42___2
ED_Total1 <- ED1+ED2+ED3+ED4+ED5+ED6+ED7+ED8+ED9+ED10+ED11+ED12+ED13+ED14+ED15+ED16+ED17+ED18+ED19+ED20
ED_Total2 <- ED21+ED22+ED23+ED24+ED25+ED26+ED27+ED28+ED29+ED30+ED31+ED32+ED33+ED34+ED35+ED36+ED37+ED38+ED39+ED40+ED41+ED42
ED_Total <- ED_Total1+ED_Total2
ED <- na.omit(ED_Total)

#fall related ED visits for HealthCare log
HED1 <- Total_Data$type_mc1___1
HED2 <- Total_Data$type_mc2___1
HED3 <- Total_Data$type_mc3___1
HED4 <- Total_Data$type_mc4___1
HED5 <- Total_Data$type_mc5___1
HED6 <- Total_Data$type_mc6___1
HED7 <- Total_Data$type_mc7___1
HED8 <- Total_Data$type_mc8___1
HED9 <- Total_Data$type_mc9___1
HED10 <- Total_Data$type_mc10___1
HED11 <- Total_Data$type_mc11___1
HED_Total <- HED1+HED2+HED3+HED4+HED5+HED6+HED7+HED8+HED9+HED10+HED11
HED <- na.omit(HED_Total)

Clean_Data <- data.frame(Total_Data$record_id, Total_Sex, Total_Age, Total_Race, Total_enthbicity,
                         Total_education, Total_no_falls, Total_no_falls_injured,
                         Total_cci_mi, Total_cci_hf, Total_cci_pvd, Total_cci_copd,
                         Total_cci_emph, Total_cci_ulcer, Total_cci_liver,
                         Total_cci_hep, Total_cci_stroke, Total_cci_hemi, Total_cci_dem,
                         Total_cci_rharth, Total_cci_contis, Total_cci_bone, Total_cci_kidney,
                         Total_cci_dialysis, Total_cci_diab, Total_cci_hiv, Total_cci,
                         Total_bi_bowels, Total_bi_bladder, Total_bi_feeding, Total_bi_grooming,
                         Total_bi_dressing, Total_bi_transfer, Total_bi_toilet, Total_bi_mobility,
                         Total_bi_stairs, Total_bi_bathing, Total_bi_score, Total_hurt,
                         Total_injuries_1, Total_injuries_2, Total_injuries_3, Total_injuries_4,
                         Total_injuries_5, Total_injuries_6, Total_injuries, Total_Hospital, Total_equ,
                         ED1, ED2, ED3, ED4, ED5, ED6, ED7, ED8, ED9, ED10, ED11, ED12, ED13, ED14,
                         ED15, ED16, ED17, ED18, ED19, ED20, ED21, ED22, ED23, ED24, ED25, ED26, ED27,
                         ED28, ED29, ED30, ED31, ED32, ED33, ED34, ED35, ED36, ED37, ED38, ED39, ED40,
                         ED41, ED42, ED_Total, HED1, HED2, HED3, HED4, HED5, HED6, HED7, HED8, HED9,
                         HED10, HED11, HED_Total)


#Choose ED visits with fall
HED_frame <- data.frame(HED1, HED2, HED3, HED4, HED5, HED6,
                        HED7, HED8, HED9, HED10, HED11)
flmc_frame <- data.frame(Total_Data$fl_mc1, Total_Data$fl_mc2, Total_Data$fl_mc3,
                         Total_Data$fl_mc4, Total_Data$fl_mc5, Total_Data$fl_mc6,
                         Total_Data$fl_mc7, Total_Data$fl_mc8, Total_Data$fl_mc9,
                         Total_Data$fl_mc10, Total_Data$fl_mc11)
for (j in 1:11){
  for (i in 1:513){
    if(is.na(flmc_frame[i,j])){
      flmc_frame[i,j]=FALSE
    }
    else{
      if(flmc_frame[i,j]==3){
        HED_frame[i,j]=0
      }
      if(flmc_frame[i,j]==0){
        HED_frame[i,j]=0
      }
    }
  }
}

#The Final_ED are the Fall-related ED visits for each patients we will use in future analysis
Final_ED <- HED_frame$HED1+HED_frame$HED2+HED_frame$HED3+HED_frame$HED4+HED_frame$HED5+HED_frame$HED6+HED_frame$HED7+HED_frame$HED8+HED_frame$HED9+HED_frame$HED10+HED_frame$HED11
T <- na.omit(HED_frame$HED1+HED_frame$HED2+HED_frame$HED3+HED_frame$HED4+HED_frame$HED5+HED_frame$HED6+HED_frame$HED7+HED_frame$HED8+HED_frame$HED9+HED_frame$HED10+HED_frame$HED11)

#Final Dataset
Clean_Data <- cbind(Clean_Data, HED_frame, flmc_frame, Final_ED, Intervention)

#Change NA to blank in Clean_Data
Clean_Data[is.na(Clean_Data)] <- ""

#Write out New Data
write.csv(Clean_Data,"/Users/dhyscuduke/Desktop/GAPcare_I_cleaned.csv", row.names = FALSE)

