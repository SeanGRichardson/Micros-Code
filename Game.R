
rm(list = ls())

library(tidyverse)
library(glue)

Players_Setup <- vector(length = 32)

Players_Setup <- c("1RA1A2AN", "1RA1A2KN", "1RA1K2AN", "1RA1K2KN",
                   "1RK1A2AN", "1RK1A2KN", "1RK1K2AN", "1RK1K2KN",
                   "1NA1A2AN", "1NA1A2KN", "1NA1K2AN", "1NA1K2KN",
                   "1NK1A2AN", "1NK1A2KN", "1NK1K2AN", "1NK1K2KN",
                   "2RA1A2AN", "2RA1A2KN", "2RA1K2AN", "2RA1K2KN",
                   "2RK1A2AN", "2RK1A2KN", "2RK1K2AN", "2RK1K2KN",
                   "2NA1A2AN", "2NA1A2KN", "2NA1K2AN", "2NA1K2KN",
                   "2NK1A2AN", "2NK1A2KN", "2NK1K2AN", "2NK1K2KN")%>% as.data.frame

colnames(Players_Setup) <- "Full_Type"

Types <- c("1", "1", "1", "1",
           "1", "1", "1", "1",
           "1", "1", "1", "1",
           "1", "1", "1", "1",
           "2", "2", "2", "2",
           "2", "2", "2", "2",
           "2", "2", "2", "2",
           "2", "2", "2", "2")

Signal <- c("1", "1", "1", "1",
            "1", "1", "1", "1",
            "N", "N", "N", "N",
            "N", "N", "N", "N",
            "2", "2", "2", "2",
            "2", "2", "2", "2",
            "N", "N", "N", "N",
            "N", "N", "N", "N")

R1_Response <- c("A", "A", "A", "A",
                 "K", "K", "K", "K",
                 "A", "A", "A", "A",
                 "K", "K", "K", "K",
                 "A", "A", "A", "A",
                 "K", "K", "K", "K",
                 "A", "A", "A", "A",
                 "K", "K", "K", "K")

R2_Response <- c("A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K",
                 "A", "A", "K", "K")

N_Response <- c("A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K")

Players_Setup <- cbind(Players_Setup, Types) %>%
  cbind(Signal) %>% cbind(R1_Response) %>%
  cbind(R2_Response) %>% cbind(N_Response)



#Specify values of the parameters


ALPHA <- 0.5

DELTA <- 0.1


#Specify initial distribution

n0 <- c(1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32,
        1/32, 1/32, 1/32, 1/32)



Max_Rounds <- 100

N <- n0


for(Round_Number in 1:Max_Rounds){
  
N <- get(glue("n{Round_Number-1}"))


Players <- Players_Setup

Players <- Players %>% cbind(N)

# Calculating initial probabilities

# Percentage of each type

p <- 0

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  Weight = Players$N[i]
  
  p <- p + Type_Yes*Weight
  
  
}



q <- 0

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  Weight = Players$N[i]
  
  q <- q + Type_Yes*Weight
  
  
}

# Percentage of each type and shirt colour


pr <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  Signal_Yes <- as.numeric(grepl("1", Players$Signal[i]))
  
  Weight = Players$N[i]
  
  pr <- pr + Type_Yes*Signal_Yes*Weight
  
  
}


qr <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  Signal_Yes <- as.numeric(grepl("2", Players$Signal[i]))
  
  Weight = Players$N[i]
  
  qr <- qr + Type_Yes*Signal_Yes*Weight
  
  
}


pn <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  Signal_Yes <- as.numeric(grepl("N", Players$Signal[i]))
  
  Weight = Players$N[i]
  
  pn <- pn + Type_Yes*Signal_Yes*Weight
  
  
}


qn <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  Signal_Yes <- as.numeric(grepl("N", Players$Signal[i]))
  
  Weight = Players$N[i]
  
  qn <- qn + Type_Yes*Signal_Yes*Weight
  
  
}


# Percentage each type, shirt colour and response to R

pa1 <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  R1_Response_Yes <- as.numeric(grepl("A", Players$R1_Response[i]))
  
  Weight = Players$N[i]
  
  pa1 <- pa1 + Type_Yes*Weight*R1_Response_Yes
  
}


pa2 <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  R2_Response_Yes <- as.numeric(grepl("A", Players$R2_Response[i]))
  
  Weight = Players$N[i]
  
  pa2 <- pa2 + Type_Yes*Weight*R2_Response_Yes
  
}


pan <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("1", Players$Types[i]))
  
  N_Response_Yes <- as.numeric(grepl("A", Players$N_Response[i]))
  
  Weight = Players$N[i]
  
  pan <- pan + Type_Yes*Weight*N_Response_Yes
  
}


qa1 <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  R1_Response_Yes <- as.numeric(grepl("A", Players$R1_Response[i]))
  
  Weight = Players$N[i]
  
  qa1 <- qa1 + Type_Yes*Weight*R1_Response_Yes
  
}


qa2 <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  R2_Response_Yes <- as.numeric(grepl("A", Players$R2_Response[i]))
  
  Weight = Players$N[i]
  
  qa2 <- qa2 + Type_Yes*Weight*R2_Response_Yes
  
}


qan <- 0 

for(i in 1:32){
  
  Type_Yes <- as.numeric(grepl("2", Players$Types[i]))
  
  N_Response_Yes <- as.numeric(grepl("A", Players$N_Response[i]))
  
  Weight = Players$N[i]
  
  qan <- qan + Type_Yes*Weight*N_Response_Yes
  
}









u_1RA1A2AN <- 0.5 *(1 - ALPHA*(qr + qn)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RA1A2KN <- 0.5 *(1*(pr+qr) - ALPHA*(qr)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RA1K2AN <- 0.5 *(1*(pr+pn+qn) - ALPHA*(qn)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RA1K2KN <- 0.5 *(1*(pr)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA





u_1RK1A2AN <- 0.5 *(1*(pn+qr+qn) - ALPHA*(qr + qn)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RK1A2KN <- 0.5 *(1*(qr) - ALPHA*(qr)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RK1K2AN <- 0.5 *(1*(pn+qn) - ALPHA*(qn)) +
  0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA


u_1RK1K2KN <- 0.5 * (1*(pa1 + qa1) - ALPHA*(qa1)) - DELTA





u_1NA1A2AN <- 0.5 *(1 - ALPHA*(qr + qn)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NA1A2KN <- 0.5 *(1*(pr+qr) - ALPHA*(qr)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NA1K2AN <- 0.5 *(1*(pr+pn+qn) - ALPHA*(qn)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NA1K2KN <- 0.5 *(1*(pr)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))




u_1NK1A2AN <- 0.5 *(1*(pn+qr+qn) - ALPHA*(qr + qn)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NK1A2KN <- 0.5 *(1*(qr) - ALPHA*(qr)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NK1K2AN <- 0.5 *(1*(pn+qn) - ALPHA*(qn)) +
  0.5 * (1*(pan + qan) - ALPHA*(qan))

u_1NK1K2KN <- 0.5 * (1*(pan + qan) - ALPHA*(qan))




u_2RA1A2AN <- 0.5 *(1 - ALPHA*(pr + pn)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RA1A2KN <- 0.5 *(1*(pr+qr) - ALPHA*(pr)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RA1K2AN <- 0.5 *(1*(pr+pn+qn) - ALPHA*(pr + pn)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RA1K2KN <- 0.5 *(1*(pr) - ALPHA*(pr)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA




u_2RK1A2AN <- 0.5 *(1*(pn+qr+qn) - ALPHA*(pn)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RK1A2KN <- 0.5 *(1*(qr)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RK1K2AN <- 0.5 *(1*(pn+qn) - ALPHA*(pn)) +
  0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA

u_2RK1K2KN <- 0.5 * (1*(pa2 + qa2) - ALPHA*(pa2)) - DELTA




u_2NA1A2AN <- 0.5 *(1 - ALPHA*(pr + pn)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NA1A2KN <- 0.5 *(1*(pr+qr) - ALPHA*(pr)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NA1K2AN <- 0.5 *(1*(pr+pn+qn) - ALPHA*(pr+pn)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NA1K2KN <- 0.5 *(1*(pr) - ALPHA*(pr)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))




u_2NK1A2AN <- 0.5 *(1*(pn+qr+qn) - ALPHA*(pn)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NK1A2KN <- 0.5 *(1*(qr)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NK1K2AN <- 0.5 *(1*(pn+qn) - ALPHA*(pn)) +
  0.5 * (1*(pan + qan) - ALPHA*(pan))

u_2NK1K2KN <- 0.5 * (1*(pan + qan) - ALPHA*(pan))









Expected_u <- c(u_1RA1A2AN, u_1RA1A2KN, u_1RA1K2AN, u_1RA1K2KN,
                u_1RK1A2AN, u_1RK1A2KN, u_1RK1K2AN, u_1RK1K2KN,
                u_1NA1A2AN, u_1NA1A2KN, u_1NA1K2AN, u_1NA1K2KN,
                u_1NK1A2AN, u_1NK1A2KN, u_1NK1K2AN, u_1NK1K2KN,
                u_2RA1A2AN, u_2RA1A2KN, u_2RA1K2AN, u_2RA1K2KN,
                u_2RK1A2AN, u_2RK1A2KN, u_2RK1K2AN, u_2RK1K2KN,
                u_2NA1A2AN, u_2NA1A2KN, u_2NA1K2AN, u_2NA1K2KN,
                u_2NK1A2AN, u_2NK1A2KN, u_2NK1K2AN, u_2NK1K2KN)



Max_u <- max(Expected_u)

Min_u <- min(Expected_u)

Norm_Expected_u <- Expected_u

for(i in 1:32){

Norm_Expected_u[i] <- (Expected_u[i] - Min_u)/(Max_u-Min_u)

}



Calc_N <- Players$N

Population_Weighting <- 0


for(i in 1:32){
  
  Calc_N[i] <- Players$N[i] +  Norm_Expected_u[i]*Players$N[i]
  
  Population_Weighting <- Population_Weighting + Calc_N[i]
}


Calc_N <- round(Calc_N/Population_Weighting, digits = 6)


assign(glue("n{Round_Number}"), Calc_N) 

assign(glue("Payoffs_Round_{Round_Number}"), Expected_u)


}


Payoff_Dynamics <- c("1RA1A2AN", "1RA1A2KN", "1RA1K2AN", "1RA1K2KN",
                     "1RK1A2AN", "1RK1A2KN", "1RK1K2AN", "1RK1K2KN",
                     "1NA1A2AN", "1NA1A2KN", "1NA1K2AN", "1NA1K2KN",
                     "1NK1A2AN", "1NK1A2KN", "1NK1K2AN", "1NK1K2KN",
                     "2RA1A2AN", "2RA1A2KN", "2RA1K2AN", "2RA1K2KN",
                     "2RK1A2AN", "2RK1A2KN", "2RK1K2AN", "2RK1K2KN",
                     "2NA1A2AN", "2NA1A2KN", "2NA1K2AN", "2NA1K2KN",
                     "2NK1A2AN", "2NK1A2KN", "2NK1K2AN", "2NK1K2KN")%>% as.data.frame


N_Dynamics <- c("1RA1A2AN", "1RA1A2KN", "1RA1K2AN", "1RA1K2KN",
                "1RK1A2AN", "1RK1A2KN", "1RK1K2AN", "1RK1K2KN",
                "1NA1A2AN", "1NA1A2KN", "1NA1K2AN", "1NA1K2KN",
                "1NK1A2AN", "1NK1A2KN", "1NK1K2AN", "1NK1K2KN",
                "2RA1A2AN", "2RA1A2KN", "2RA1K2AN", "2RA1K2KN",
                "2RK1A2AN", "2RK1A2KN", "2RK1K2AN", "2RK1K2KN",
                "2NA1A2AN", "2NA1A2KN", "2NA1K2AN", "2NA1K2KN",
                "2NK1A2AN", "2NK1A2KN", "2NK1K2AN", "2NK1K2KN")%>% as.data.frame


Time_Periods_N <- c(0:Max_Rounds)

Time_Periods_Payoffs <- c(1:Max_Rounds)

for(i in 0:Max_Rounds){

  N_Round <- get(glue("n{i}"))
  N_Dynamics <- N_Dynamics %>% cbind(get(glue("n{i}")))
  
}


for(i in 1:Max_Rounds){

  Payoff_Round <- get(glue("Payoffs_Round_{i}"))

  Payoff_Dynamics <- Payoff_Dynamics %>% cbind(get(glue("Payoffs_Round_{i}")))

}


colnames(N_Dynamics) <- c("Player_Types",Time_Periods_N)
colnames(Payoff_Dynamics) <- c("Player_Types",Time_Periods_Payoffs)

