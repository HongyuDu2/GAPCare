library(tidyverse)
library(dplyr)

Trailmaking_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Cognition/Cognition_trailmakingtest_new.csv")
R <- as.vector(Trailmaking_data_original$pt)
ID <- R[!duplicated(R)]

j <- 1

for (j in 1:length(ID)){
  for (i in 1:length(Trailmaking_data_original$pt)){
    if (Trailmaking_data_original$pt[i]==ID[j]){
      Trailmaking_data_original$pt[i] <- j
    }
  }
  j <- j+1
}


Trailmaking_data<- Trailmaking_data_original %>%
  group_by(pt) %>% slice(which.max(Timestamp))


D1 <- Trailmaking_data_original %>% filter(pt == 1)
plot(D1$Index,D1$Timestamp,col=c('blue','orange'),type='o',
     pch=19,lwd=2,cex=1.5,xlab = 'Index', ylab = 'Time')

D2 <- reaction_data_original %>% filter(pt == 2)
plot(D2$Index,D2$Timestamp,col=c('blue','orange'),type='o',
     pch=19,lwd=2,cex=1.5,xlab = 'Index', ylab = 'Time')

D3 <- reaction_data_original %>% filter(pt == 3)
plot(D3$Index,D3$Timestamp,col=c('blue','orange'),type='o',
     pch=19,lwd=2,cex=1.5,xlab = 'Index', ylab = 'Time')

D6 <- reaction_data_original %>% filter(pt == 6)
plot(D6$Index,D6$Timestamp,col=c('blue','orange'),type='o',
     pch=19,lwd=2,cex=1.5,xlab = 'Index', ylab = 'Time')

D7 <- reaction_data_original %>% filter(pt == 7)
plot(D7$Index,D7$Timestamp,col=c('blue','orange'),type='o',
     pch=19,lwd=2,cex=1.5,xlab = 'Index', ylab = 'Time')

Mean <- c()
Error <- c()
Max <- c()
for (i in 1:length(ID)){
  D <- reaction_data_original %>% filter(pt == i)
  Mean[i] <- mean(D$Timestamp)
  Max[i] <- max(D$Timestamp)
  Error[i] <- D$No.Errors[1] 
}
plot(Mean, Error,col=c('blue','orange'),pch=19,lwd=2,cex=1.5)
plot(Max, Error,col=c('blue','orange'),pch=19,lwd=2,cex=1.5)


