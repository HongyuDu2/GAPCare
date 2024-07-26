library(tidyverse)

reaction_data_original <- read.csv("/Users/dhyscuduke/Desktop/GAPCare/GAPCare II/Cleaned_Data_New_Cognition/Cognition_reaction_new.csv")
R <- as.vector(reaction_data_original$pt)
ID <- R[!duplicated(R)]
#Reaction_time <- (as.vector(reaction_data_original$reaction))/50000

j <- 1

for (j in 1:length(ID)){
  for (i in 1:length(reaction_data_original$pt)){
    if (reaction_data_original$pt[i]==ID[j]){
      reaction_data_original$pt[i] <- j
    }
  }
  j <- j+1
}

t <- 0
Split_Data <- c(1,2)
for (i in 1:138){
  Split_Data <- rbind(Split_Data,reaction_data_original %>% filter(pt == i))
}
Split_Data <- Split_Data[-1,]


boxplot(reaction/50000~ pt, data = Split_Data , xlab = "ID",
        ylab = "Reaction Time", main = "Cognition_Reaction")

