#Sample Size Calculation
#Import Library
library(dplyr)
library(MASS)

# Impoort data set
setwd('/Users/dhyscuduke/Desktop')
DATA = read.csv('/Users/dhyscuduke/Desktop/GAPcare_I_cleaned.csv')

#Fall related ED visits
ED <- na.omit(DATA$Final_ED)

#Check the histogram of fall-related ED visits
hist(ED)

#Theoretical Based Sample Size Calculation

#Negative Binomial Regression
Inter <- na.omit(DATA$Intervention)
nbGLM <- glm.nb(ED ~ Inter, data=DATA)

#Power Calculation in GAPCare I Dataset
Disper <- nbGLM$theta #The parameter we will use in formula (k)
inter1 <- DATA %>% filter(Intervention == "0") 
inter2 <- DATA %>% filter(Intervention == "1")
mu1 <- mean(inter1$Final_ED.1)
mu2 <- mean(inter2$Final_ED.1)
step1 <- ((log(mu1)-log(mu2))*sqrt(500))/(sqrt(2*(2/Disper+1/mu1+1/mu2)))
power1 <- pnorm(step1-qnorm(0.975))

#Power Calculation by using 30% efficacy
Disper <- nbGLM$theta #The parameter we will use in formula (k)
inter1 <- DATA %>% filter(Intervention == "0") 
inter2 <- DATA %>% filter(Intervention == "1")
mu1 <- mean(inter1$Final_ED.1)
mu2 <- 0.7*mu1
step1 <- ((log(mu1)-log(mu2))*sqrt(500))/(sqrt(2*(2/Disper+1/mu1+1/mu2)))
power1 <- pnorm(step1-qnorm(0.975))



#Sample Size Calculation in Simulation
j <- 0 #Calculate the number of p-values smaller than 0.05
for (i in 1:1000){
  Simuy1 <- rnbinom(n=250, mu=mu1, size=Disper) #Simulate Control Group Dataset
  Simuy2 <- rnbinom(n=250, mu=0.7*mu1, size=Disper) #Simulate Case Group Dataset
  Simuy <- c(Simuy1, Simuy2) #Whole simulated Dataset
  Simux1 <- rep(0:0,250)
  Simux2 <- rep(1:1,250)
  Simux <- c(Simux1, Simux2) #Whole simulated intervention indicators
  nbGLM_2 <- glm.nb(Simuy ~ Simux) 
  p_value <- summary(nbGLM_2)$coefficients[2,4]
  if (p_value < 0.05){
    j <- j+1
  }
}
power2 <- (j+1)/1001

#Sample Size Calculation in Bootstrap
New_DATA <- data.frame(DATA$Final_ED.1, DATA$Intervention, DATA$Total_Age, DATA$Total_Sex,
                       DATA$Total_no_falls, DATA$Total_cci, DATA$Total_bi_score,
                       DATA$Total_injuries, DATA$Total_Hospital, DATA$Total_equ)
New_DATA1 <- New_DATA %>% filter(DATA.Intervention == "0")
New_DATA2 <- New_DATA %>% filter(DATA.Intervention == "1")
New_DATA <- rbind(New_DATA1, New_DATA2)

m1 <- 0
for (i1 in 1:1000){
  Bootstrap_DATA <- as.data.frame(matrix(nrow = 500, ncol = 10))
  for (j1 in 1:500){
    k <- sample(1:110,1)
    Bootstrap_DATA[j1,] = New_DATA[k,]
  }
  nbGLM_3 <- glm.nb(V1 ~ V2 + V3 + V4 + V5
                    +  V8 + V9
                    + V10, data = Bootstrap_DATA)
  p_value <- summary(nbGLM_3)$coefficients[2,4]
  if (p_value < 0.05){
    m1 <- m1 + 1
  }
}
power3 <- (1+m1)/1000


