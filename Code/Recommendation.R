library(dplyr)
setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcare_I_cleaned2 copy.csv')

#PT recommendations 1
With_PT <- DATA %>% filter(Intervention == 1)
With_PT1_1 <- With_PT %>% filter(PT_Recom1 == 2)
With_PT1_2 <- With_PT %>% filter(PT_Recom1 == 3)
(sum(With_PT1_1$Final_ED.1)+sum(With_PT1_2$Final_ED.1))/(nrow(With_PT1_1)+nrow(With_PT1_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT1_1$Final_ED.1)+sum(With_PT1_2$Final_ED.1)))/(55-(nrow(With_PT1_1)+nrow(With_PT1_2)))

#PT recommendations 2
With_PT <- DATA %>% filter(Intervention == 1)
With_PT2_1 <- With_PT %>% filter(PT_Recom2 == 2)
With_PT2_2 <- With_PT %>% filter(PT_Recom2 == 3)
(sum(With_PT2_1$Final_ED.1)+sum(With_PT2_2$Final_ED.1))/(nrow(With_PT2_1)+nrow(With_PT2_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT2_1$Final_ED.1)+sum(With_PT2_2$Final_ED.1)))/(55-(nrow(With_PT2_1)+nrow(With_PT2_2)))

#PT recommendations 3
With_PT <- DATA %>% filter(Intervention == 1)
With_PT3_1 <- With_PT %>% filter(PT_Recom3 == 2)
With_PT3_2 <- With_PT %>% filter(PT_Recom3 == 3)
(sum(With_PT3_1$Final_ED.1)+sum(With_PT3_2$Final_ED.1))/(nrow(With_PT3_1)+nrow(With_PT3_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT3_1$Final_ED.1)+sum(With_PT3_2$Final_ED.1)))/(55-(nrow(With_PT3_1)+nrow(With_PT3_2)))

#PT recommendations 4
With_PT <- DATA %>% filter(Intervention == 1)
With_PT4_1 <- With_PT %>% filter(PT_Recom4 == 2)
With_PT4_2 <- With_PT %>% filter(PT_Recom4 == 3)
(sum(With_PT4_1$Final_ED.1)+sum(With_PT4_2$Final_ED.1))/(nrow(With_PT4_1)+nrow(With_PT4_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT4_1$Final_ED.1)+sum(With_PT4_2$Final_ED.1)))/(55-(nrow(With_PT4_1)+nrow(With_PT4_2)))

#PT recommendations 5
With_PT <- DATA %>% filter(Intervention == 1)
With_PT5_1 <- With_PT %>% filter(PT_Recom5 == 2)
With_PT5_2 <- With_PT %>% filter(PT_Recom5 == 3)
(sum(With_PT5_1$Final_ED.1)+sum(With_PT5_2$Final_ED.1))/(nrow(With_PT5_1)+nrow(With_PT5_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT5_1$Final_ED.1)+sum(With_PT5_2$Final_ED.1)))/(55-(nrow(With_PT5_1)+nrow(With_PT5_2)))

#PT recommendations 6
With_PT <- DATA %>% filter(Intervention == 1)
With_PT6_1 <- With_PT %>% filter(PT_Recom6 == 2)
With_PT6_2 <- With_PT %>% filter(PT_Recom6 == 3)
(sum(With_PT6_1$Final_ED.1)+sum(With_PT6_2$Final_ED.1))/(nrow(With_PT6_1)+nrow(With_PT6_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT6_1$Final_ED.1)+sum(With_PT6_2$Final_ED.1)))/(55-(nrow(With_PT6_1)+nrow(With_PT6_2)))

#PT recommendations 7
With_PT <- DATA %>% filter(Intervention == 1)
With_PT7_1 <- With_PT %>% filter(PT_Recom7 == 2)
With_PT7_2 <- With_PT %>% filter(PT_Recom7 == 3)
(sum(With_PT7_1$Final_ED.1)+sum(With_PT7_2$Final_ED.1))/(nrow(With_PT7_1)+nrow(With_PT7_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT7_1$Final_ED.1)+sum(With_PT7_2$Final_ED.1)))/(55-(nrow(With_PT7_1)+nrow(With_PT7_2)))

#PT recommendations 8
With_PT <- DATA %>% filter(Intervention == 1)
With_PT8_1 <- With_PT %>% filter(PT_Recom8 == 2)
With_PT8_2 <- With_PT %>% filter(PT_Recom8 == 3)
(sum(With_PT8_1$Final_ED.1)+sum(With_PT8_2$Final_ED.1))/(nrow(With_PT8_1)+nrow(With_PT8_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT8_1$Final_ED.1)+sum(With_PT8_2$Final_ED.1)))/(55-(nrow(With_PT8_1)+nrow(With_PT8_2)))

#PT recommendations 9
With_PT <- DATA %>% filter(Intervention == 1)
With_PT9_1 <- With_PT %>% filter(PT_Recom9 == 2)
With_PT9_2 <- With_PT %>% filter(PT_Recom9 == 3)
(sum(With_PT9_1$Final_ED.1)+sum(With_PT9_2$Final_ED.1))/(nrow(With_PT9_1)+nrow(With_PT9_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT9_1$Final_ED.1)+sum(With_PT9_2$Final_ED.1)))/(55-(nrow(With_PT9_1)+nrow(With_PT9_2)))

#PT recommendations 10
With_PT <- DATA %>% filter(Intervention == 1)
With_PT10_1 <- With_PT %>% filter(PT_Recom10 == 2)
With_PT10_2 <- With_PT %>% filter(PT_Recom10 == 3)
(sum(With_PT10_1$Final_ED.1)+sum(With_PT10_2$Final_ED.1))/(nrow(With_PT10_1)+nrow(With_PT10_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT10_1$Final_ED.1)+sum(With_PT10_2$Final_ED.1)))/(55-(nrow(With_PT10_1)+nrow(With_PT10_2)))

#PT recommendations 11
With_PT <- DATA %>% filter(Intervention == 1)
With_PT11_1 <- With_PT %>% filter(PT_Recom11 == 2)
With_PT11_2 <- With_PT %>% filter(PT_Recom11 == 3)
(sum(With_PT11_1$Final_ED.1)+sum(With_PT11_2$Final_ED.1))/(nrow(With_PT11_1)+nrow(With_PT11_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT11_1$Final_ED.1)+sum(With_PT11_2$Final_ED.1)))/(55-(nrow(With_PT11_1)+nrow(With_PT11_2)))

#PT recommendations 12
With_PT <- DATA %>% filter(Intervention == 1)
With_PT12_1 <- With_PT %>% filter(PT_Recom12 == 2)
With_PT12_2 <- With_PT %>% filter(PT_Recom12 == 3)
(sum(With_PT12_1$Final_ED.1)+sum(With_PT12_2$Final_ED.1))/(nrow(With_PT12_1)+nrow(With_PT12_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT12_1$Final_ED.1)+sum(With_PT12_2$Final_ED.1)))/(55-(nrow(With_PT12_1)+nrow(With_PT12_2)))

#PT recommendations 13
With_PT <- DATA %>% filter(Intervention == 1)
With_PT13_1 <- With_PT %>% filter(PT_Recom13 == 2)
With_PT13_2 <- With_PT %>% filter(PT_Recom13 == 3)
(sum(With_PT13_1$Final_ED.1)+sum(With_PT13_2$Final_ED.1))/(nrow(With_PT13_1)+nrow(With_PT13_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT13_1$Final_ED.1)+sum(With_PT13_2$Final_ED.1)))/(55-(nrow(With_PT13_1)+nrow(With_PT13_2)))

#PT recommendations 14
With_PT <- DATA %>% filter(Intervention == 1)
With_PT14_1 <- With_PT %>% filter(PT_Recom14 == 2)
With_PT14_2 <- With_PT %>% filter(PT_Recom14 == 3)
(sum(With_PT14_1$Final_ED.1)+sum(With_PT14_2$Final_ED.1))/(nrow(With_PT14_1)+nrow(With_PT14_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT14_1$Final_ED.1)+sum(With_PT14_2$Final_ED.1)))/(55-(nrow(With_PT14_1)+nrow(With_PT14_2)))

#PT recommendations 15
With_PT <- DATA %>% filter(Intervention == 1)
With_PT15_1 <- With_PT %>% filter(PT_Recom15 == 2)
With_PT15_2 <- With_PT %>% filter(PT_Recom15 == 3)
(sum(With_PT15_1$Final_ED.1)+sum(With_PT15_2$Final_ED.1))/(nrow(With_PT15_1)+nrow(With_PT15_2)) 
(sum(With_PT$Final_ED.1)-(sum(With_PT15_1$Final_ED.1)+sum(With_PT15_2$Final_ED.1)))/(55-(nrow(With_PT15_1)+nrow(With_PT15_2)))

#pharmacy recommendations 1
With_Phar <- DATA %>% filter(Intervention == 1)
With_Phar1_1 <- With_Phar %>% filter(Phar_Recom1 == 2)
With_Phar1_2 <- With_Phar %>% filter(Phar_Recom1 == 3)
(sum(With_Phar1_1$Final_ED.1)+sum(With_Phar1_2$Final_ED.1))/(nrow(With_Phar1_1)+nrow(With_Phar1_2)) #rate of ED visists in uptake
#rate of ED visists in no uptake
(sum(With_Phar$Final_ED.1)-(sum(With_Phar1_1$Final_ED.1)+sum(With_Phar1_2$Final_ED.1)))/(55-(nrow(With_Phar1_1)+nrow(With_Phar1_2)))

#pharmacy recommendations 2
With_Phar2_1 <- With_Phar %>% filter(Phar_Recom2 == 2)
With_Phar2_2 <- With_Phar %>% filter(Phar_Recom2 == 3)
(sum(With_Phar2_1$Final_ED.1)+sum(With_Phar2_2$Final_ED.1))/(nrow(With_Phar2_1)+nrow(With_Phar2_2)) #rate of ED visists in uptake
#rate of ED visists in no uptake
(sum(With_Phar$Final_ED.1)-(sum(With_Phar2_1$Final_ED.1)+sum(With_Phar2_2$Final_ED.1)))/(55-(nrow(With_Phar2_1)+nrow(With_Phar2_2)))

#pharmacy recommendations 3
With_Phar3_1 <- With_Phar %>% filter(Phar_Recom3 == 2)
With_Phar3_2 <- With_Phar %>% filter(Phar_Recom3 == 3)
(sum(With_Phar3_1$Final_ED.1)+sum(With_Phar3_2$Final_ED.1))/(nrow(With_Phar3_1)+nrow(With_Phar3_2)) #rate of ED visists in uptake
#rate of ED visists in no uptake
(sum(With_Phar$Final_ED.1)-(sum(With_Phar3_1$Final_ED.1)+sum(With_Phar3_2$Final_ED.1)))/(55-(nrow(With_Phar3_1)+nrow(With_Phar3_2)))

#pharmacy recommendations 4
With_Phar4_1 <- With_Phar %>% filter(Phar_Recom4 == 2)
With_Phar4_2 <- With_Phar %>% filter(Phar_Recom4 == 3)
(sum(With_Phar4_1$Final_ED.1)+sum(With_Phar4_2$Final_ED.1))/(nrow(With_Phar4_1)+nrow(With_Phar4_2)) #rate of ED visists in uptake
#rate of ED visists in no uptake
(sum(With_Phar$Final_ED.1)-(sum(With_Phar4_1$Final_ED.1)+sum(With_Phar4_2$Final_ED.1)))/(55-(nrow(With_Phar4_1)+nrow(With_Phar4_2)))

#Overall Rate
s1 <- (sum(With_Phar1_1$Final_ED.1)+sum(With_Phar1_2$Final_ED.1))
s2 <- (sum(With_Phar2_1$Final_ED.1)+sum(With_Phar2_2$Final_ED.1))
s3 <- (sum(With_Phar3_1$Final_ED.1)+sum(With_Phar3_2$Final_ED.1))
s4 <- (sum(With_Phar4_1$Final_ED.1)+sum(With_Phar4_2$Final_ED.1))
n1 <- (nrow(With_Phar1_1)+nrow(With_Phar1_2))
n2 <- (nrow(With_Phar2_1)+nrow(With_Phar2_2))
n3 <- (nrow(With_Phar3_1)+nrow(With_Phar3_2))
n4 <- (nrow(With_Phar4_1)+nrow(With_Phar4_2))
(s1+s2+s3+s4)/(n1+n2+n3+n4)

s_1 <- (sum(With_Phar$Final_ED.1)-(sum(With_Phar1_1$Final_ED.1)+sum(With_Phar1_2$Final_ED.1)))
s_2 <- (sum(With_Phar$Final_ED.1)-(sum(With_Phar2_1$Final_ED.1)+sum(With_Phar2_2$Final_ED.1)))
s_3 <- (sum(With_Phar$Final_ED.1)-(sum(With_Phar3_1$Final_ED.1)+sum(With_Phar3_2$Final_ED.1)))
s_4 <- (sum(With_Phar$Final_ED.1)-(sum(With_Phar4_1$Final_ED.1)+sum(With_Phar4_2$Final_ED.1)))
n_1 <- (55-(nrow(With_Phar1_1)+nrow(With_Phar1_2)))
n_2 <- (55-(nrow(With_Phar2_1)+nrow(With_Phar2_2)))
n_3 <- (55-(nrow(With_Phar3_1)+nrow(With_Phar3_2)))
n_4 <- (55-(nrow(With_Phar4_1)+nrow(With_Phar4_2)))
(s_1+s_2+s_3+s_4)/(n_1+n_2+n_3+n_4)

