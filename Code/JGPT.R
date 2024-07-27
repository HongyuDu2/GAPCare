library(tidyverse)
library(dplyr)

setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcareI_R03_DATA_2022-10-21_1144_deidentified.csv')


DATA_Int <- DATA %>% filter(intervention_random == 1)
DATA_Con <- DATA %>% filter(intervention_random == 0)

DATA2_Int <- DATA[DATA$record_id %in% DATA_Int$record_id, ]
DATA2_Con <- DATA[DATA$record_id %in% DATA_Con$record_id, ] 

# pt_face; pt_contact; pt; provider_ed_pt; pt_90; provider_ed_pt_d90; provider_ed_ptw30; ptw30
Int_pt <- data.frame(DATA2_Int$provider_ed_pt, 
           DATA2_Int$pt___1, DATA2_Int$pt___2,
            DATA2_Int$pt_face1,
           DATA2_Int$ptw30___1,
           DATA2_Int$ptw30___2)

Int_pt[is.na(Int_pt)] <- 0
Int_pt[Int_pt == 998] <- 0
Int_pt_new <- data.frame(DATA2_Int$record_id, rowSums(Int_pt))

K1 <- Int_pt_new %>% 
  group_by(DATA2_Int.record_id) %>% 
  summarize(sum_value = sum(rowSums.Int_pt.))

#For Control
Con_pt <- data.frame(DATA2_Con$provider_ed_pt, 
                     DATA2_Con$pt___1, DATA2_Con$pt___2,
                     DATA2_Con$pt_face1,
                     DATA2_Con$ptw30___1,
                     DATA2_Con$ptw30___2)

Con_pt[is.na(Con_pt)] <- 0
Con_pt[Con_pt == 998] <- 0
Con_pt_new <- data.frame(DATA2_Con$record_id, rowSums(Con_pt))

K2 <- Con_pt_new %>% 
  group_by(DATA2_Con.record_id) %>% 
  summarize(sum_value = sum(rowSums.Con_pt.))

# community; communityw30; community_d90 for intervention
Int_Com_1 <- data.frame(DATA2_Int$community, 
                      DATA2_Int$communityw30,
                      DATA2_Int$community_d90)

Int_Com_1[is.na(Int_Com_1)] <- 0
Int_Com_1[Int_Com_1 == 998] <- 0
Int_Com_1[Int_Com_1 == 2] <- 0

Int_Com_2 <- data.frame(DATA2_Int$provider)
Int_Com_2[is.na(Int_Com_2)] <- 0
Int_Com_2[Int_Com_2 == 1] <- 0
Int_Com_2[Int_Com_2 == 3] <- 0
Int_Com_2[Int_Com_2 == 8] <- 0

Int_Com <- cbind(Int_Com_1, Int_Com_2)

Int_Com_new <- data.frame(DATA2_Int$record_id, rowSums(Int_Com))

K11 <- Int_Com_new %>% 
  group_by(DATA2_Int.record_id) %>% 
  summarize(sum_value = sum(rowSums.Int_Com.))

# community; communityw30; community_d90 for control
Con_Com_1 <- data.frame(DATA2_Con$community, 
                      DATA2_Con$communityw30,
                      DATA2_Con$community_d90)

Con_Com_1[is.na(Con_Com_1)] <- 0
Con_Com_1[Con_Com_1 == 998] <- 0
Con_Com_1[Con_Com_1 == 2] <- 0

Con_Com_2 <- data.frame(DATA2_Con$provider)
Con_Com_2[is.na(Con_Com_2)] <- 0
Con_Com_2[Con_Com_2 == 1] <- 0
Con_Com_2[Con_Com_2 == 6] <- 0

Con_Com <- cbind(Con_Com_1, Con_Com_2)

Con_Com_new <- data.frame(DATA2_Con$record_id, rowSums(Con_Com))

K12 <- Con_Com_new %>% 
  group_by(DATA2_Con.record_id) %>% 
  summarize(sum_value = sum(rowSums.Con_Com.))

