##############################################################
# Check the Completeness of patients visits in GAPCare II dataset #
##############################################################
library(dplyr)

Data <- read.csv('/Users/dhyscuduke/Desktop/GAPcareIIV2_DATA_2023-04-06_1414.csv')

#Select the redcap_event_name
Data_ed_visit <- Data %>% filter(redcap_event_name == 'ed_visit_arm_1')
Data_basline <- Data %>% filter(redcap_event_name == 'baseline_arm_1')
Data_adminother <- Data %>% filter(redcap_event_name == 'adminother_arm_1')
Data_participants_info <- Data %>% filter(redcap_event_name == 'participants_info_arm_1')

#Completeness Check for Enrollment
k1 <- as.vector(Data_basline$scr_1)
k2 <- as.vector(Data_participants_info$scr_1)
k3 <- as.vector(Data_adminother$scr_1)

#Intervention and Control Group
Intervention <- na.omit(as.vector(Data_ed_visit$intervention_random))
