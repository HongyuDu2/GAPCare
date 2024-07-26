##############################################################
# Check the missing of patients visits in GAPCare II dataset #
##############################################################
library(dplyr)

Data <- read.csv('/Users/dhyscuduke/Desktop/GAPcareIIV2_DATA_2023-03-28_1507.csv')

#Select the redcap_event_name
Data <- Data %>% select(c(1,2))

Data_ed_visit <- Data %>% filter(redcap_event_name == 'ed_visit_arm_1') %>% select(c(1))
Data_basline <- Data %>% filter(redcap_event_name == 'baseline_arm_1') %>% select(c(1))
Data_adminother <- Data %>% filter(redcap_event_name == 'adminother_arm_1') %>% select(c(1))
Data_participants_info <- Data %>% filter(redcap_event_name == 'participants_info_arm_1') %>% select(c(1))

#The ID of Missing for each stage
x <- c(1:31)
Miss_basline <- x[ !x %in% as.matrix(Data_basline)[,1]]
Miss_adminother <- x[ !x %in% as.matrix(Data_adminother)[,1]]
Miss_participants_info <- x[ !x %in% as.matrix(Data_participants_info)[,1]]

