# Import Packages will be usedin this study
# For Different Tables and Figures
library(dplyr)

# Impoort data set
setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcareI_R03_DATA_2022-10-21_1144_deidentified.csv')

# Create the two 55 patients groups based on intervention
Control_data <- DATA %>% filter(intervention_random == "0")
Inter_data <- DATA %>% filter(intervention_random == "1")
Total_data <- rbind(Control_data, Inter_data)

#ID of Usual care, Intervention, and together
id_Control <- Control_data$record_id
id_Inter <- Inter_data$record_id
id_Total <- Total_data$record_id

#Filter all data based on times in usual care, intervention, and together
Control_data_com <- DATA %>% filter(record_id %in% Control_data$record_id) 
Inter_data_com <- DATA %>% filter(record_id %in% Inter_data$record_id) 
Total_Data <- DATA %>% filter(record_id %in% id_Total) 

Control_data_com$d180_fes1[which(Control_data_com$d180_fes1==99)] <- NA
Control_data_com$d180_fes2[which(Control_data_com$d180_fes2==99)] <- NA
Control_data_com$d180_fes3[which(Control_data_com$d180_fes3==99)] <- NA
Control_data_com$d180_fes4[which(Control_data_com$d180_fes4==99)] <- NA
Control_data_com$d180_fes5[which(Control_data_com$d180_fes5==99)] <- NA
Control_data_com$d180_fes6[which(Control_data_com$d180_fes6==99)] <- NA
Control_data_com$d180_fes7[which(Control_data_com$d180_fes7==99)] <- NA

Inter_data_com$d180_fes1[which(Inter_data_com$d180_fes1==99)] <- NA
Inter_data_com$d180_fes2[which(Inter_data_com$d180_fes2==99)] <- NA
Inter_data_com$d180_fes3[which(Inter_data_com$d180_fes3==99)] <- NA
Inter_data_com$d180_fes4[which(Inter_data_com$d180_fes4==99)] <- NA
Inter_data_com$d180_fes5[which(Inter_data_com$d180_fes5==99)] <- NA
Inter_data_com$d180_fes6[which(Inter_data_com$d180_fes6==99)] <- NA
Inter_data_com$d180_fes7[which(Inter_data_com$d180_fes7==99)] <- NA

#Complete of FEC in control group
length(na.omit(Control_data_com$d180_fes1))/55
length(na.omit(Control_data_com$d180_fes2))/55
length(na.omit(Control_data_com$d180_fes3))/55
length(na.omit(Control_data_com$d180_fes4))/55
length(na.omit(Control_data_com$d180_fes5))/55
length(na.omit(Control_data_com$d180_fes6))/55
length(na.omit(Control_data_com$d180_fes7))/55

#Complete of FEC in intervention group
length(na.omit(Inter_data_com$d180_fes1))/55
length(na.omit(Inter_data_com$d180_fes2))/55
length(na.omit(Inter_data_com$d180_fes3))/55
length(na.omit(Inter_data_com$d180_fes4))/55
length(na.omit(Inter_data_com$d180_fes5))/55
length(na.omit(Inter_data_com$d180_fes6))/55
length(na.omit(Inter_data_com$d180_fes7))/55

#Average rates of FEC in each group with different events
a1<-sum(na.omit(Control_data_com$d180_fes1))/length(na.omit(Control_data_com$d180_fes1))
a2<-sum(na.omit(Control_data_com$d180_fes2))/length(na.omit(Control_data_com$d180_fes2))
a3<-sum(na.omit(Control_data_com$d180_fes3))/length(na.omit(Control_data_com$d180_fes3))
a4<-sum(na.omit(Control_data_com$d180_fes4))/length(na.omit(Control_data_com$d180_fes4))
a5<-sum(na.omit(Control_data_com$d180_fes5))/length(na.omit(Control_data_com$d180_fes5))
a6<-sum(na.omit(Control_data_com$d180_fes6))/length(na.omit(Control_data_com$d180_fes6))
a7<-sum(na.omit(Control_data_com$d180_fes7))/length(na.omit(Control_data_com$d180_fes7))
b1<-sum(na.omit(Inter_data_com$d180_fes1))/length(na.omit(Inter_data_com$d180_fes1))
b2<-sum(na.omit(Inter_data_com$d180_fes2))/length(na.omit(Inter_data_com$d180_fes2))
b3<-sum(na.omit(Inter_data_com$d180_fes3))/length(na.omit(Inter_data_com$d180_fes3))
b4<-sum(na.omit(Inter_data_com$d180_fes4))/length(na.omit(Inter_data_com$d180_fes4))
b5<-sum(na.omit(Inter_data_com$d180_fes5))/length(na.omit(Inter_data_com$d180_fes5))
b6<-sum(na.omit(Inter_data_com$d180_fes6))/length(na.omit(Inter_data_com$d180_fes6))
b7<-sum(na.omit(Inter_data_com$d180_fes7))/length(na.omit(Inter_data_com$d180_fes7))

#Total average rates
Total_Con <- a1+a2+a3+a4+a5+a6+a7
Total_Inter <- b1+b2+b3+b4+b5+b6+b7

#Plots to compare rates between control and intervention
X <- c(1,2,3,4,5,6,7)
Y1 <- c(a1,a2,a3,a4,a5,a6,a7)
Y2 <- c(b1,b2,b3,b4,b5,b6,b7)
plot(X,Y1,col="red",cex=1,type = "o",pch=15, xlab = "Different Concerns", ylab = "FEC Rates",ylim = c(1, 2.5))
lines(X,Y2,col="blue",cex=1,type = "o",pch=15)
legend("topleft",cex=.6,c("Intervention","Control"),col=c("Blue","Red"),lty=1:1)
