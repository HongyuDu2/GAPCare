# Import Packages will be usedin this study
# For Different Tables and Figures
library(dplyr)

# Impoort data set
setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcareI_R03_DATA_2022-10-21_1144_deidentified.csv')

#Number of Score 6
table(DATA$sis_score)[7]

#Number of Score 5
table(DATA$sis_score)[6]

#Number of Score 4
table(DATA$sis_score)[5]

#Number of Score 3
table(DATA$sis_score)[4]

#Number of Score 2
table(DATA$sis_score)[3]

#Number of Score 1
table(DATA$sis_score)[2]

#Number of Score 0
table(DATA$sis_score)[1]

#Number of Score 1 no random
DATA %>% filter(DATA$sis_score == "1") %>% filter(consent == "0")
3-sum((DATA %>% filter(DATA$sis_score == "1"))$consent)

#Number of Score 2 no random
DATA %>% filter(DATA$sis_score == "2") %>% filter(consent == "0")
6-sum((DATA %>% filter(DATA$sis_score == "2"))$consent)

#Number of Score 3 no random
DATA %>% filter(DATA$sis_score == "3") %>% filter(consent == "0")
7-sum((DATA %>% filter(DATA$sis_score == "3"))$consent)

#ID of Score 1 no random
(DATA %>% filter(DATA$sis_score == "1") %>% filter(consent == "0"))$record_id

#ID of Score 2 no random
(DATA %>% filter(DATA$sis_score == "2") %>% filter(consent == "0"))$record_id

#ID of Score 3 no random
(DATA %>% filter(DATA$sis_score == "3") %>% filter(consent == "0"))$record_id

#Figure
labelset <-c('4','3','6','7','18','38','44')
X <- c(0,1,2,3,4,5,6) #Score
Y <- c(4,3,6,7,18,38,44) #Number of Patients
plot(X, Y, pch=c(1), col=c('red'), lty=4, lwd=2, xlab = "Score", ylab = "Number of Patients", xlim=c(0,6.5))
text(X+0.4, Y, labelset,col='black')




