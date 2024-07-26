library(tidyverse)

accel_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Motor/Motor_gait_accel_new.csv")
R <- as.vector(accel_data_original$pt)
ID <- R[!duplicated(R)]

j <- 1

for (j in 1:10){
  for (i in 1:length(accel_data_original$pt)){
    if (accel_data_original$pt[i]==ID[j]){
      accel_data_original$pt[i] <- j
    }
  }
  j <- j+1
}

D1 <- accel_data_original %>% filter(pt == 6)

plot(D1$timestamp,D1$y,col='blue',type='l', ylim = c(-1.5, 1.5),xlab = 'time',ylab='value')

lines(D1$timestamp,D1$z,col='green',type='l', ylim = c(-1.5, 1.5))

lines(D1$timestamp,D1$x,col='red',type='l', ylim = c(-1.5, 1.5))
