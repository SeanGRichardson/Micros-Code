
Players_Setup <- vector(length = 16)

Players_Setup <- c("BRARAG", "BRARKG", "BRKRAG", "BRKRKG", 
             "BGARAG", "BGARKG", "BGKRAG", "BGKRKG",
             "ERARAG", "ERARKG", "ERKRAG", "ERKRKG",
             "EGARAG", "EGARKG", "EGKRAG", "EGKRKG") %>% as.data.frame

colnames(Players_Setup) <- "Full_Type"

Types <- c("B", "B", "B", "B",
           "B", "B", "B", "B",
           "E", "E", "E", "E",
           "E", "E", "E", "E")

Shirt <- c("R", "R", "R", "R",
           "G", "G", "G", "G",
           "R", "R", "R", "R",
           "G", "G", "G", "G")

R_Response <- c("A", "A", "K", "K",
                "A", "A", "K", "K",
                "A", "A", "K", "K",
                "A", "A", "K", "K")

G_Response <- c("A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K",
                "A", "K", "A", "K")

Players_Setup <- cbind(Players_Setup, Types) %>%
  cbind(Shirt) %>% cbind(R_Response) %>%
  cbind(G_Response)



#Specify values of the parameters


ALPHA <- 0.5

DELTA <- 0.2

GAMMA <- 0.6

#Specify initial distribution

n0 <- c(1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16,
        1/16)


Max_Rounds <- 1000

N <- n0


for(Round_Number in 1:Max_Rounds){
  
N <- get(glue("n{Round_Number-1}"))


Players <- Players_Setup

Players <- Players %>% cbind(N)

# Calculating initial probabilities

# Percentage of each type

p <- 0

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Weight = Players$N[i]
  
  p <- p + Type_Yes*Weight
  
  
}



q <- 0

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Weight = Players$N[i]
  
  q <- q + Type_Yes*Weight
  
  
}

# Percentage of each type and shirt colour


pr <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  Weight = Players$N[i]
  
  pr <- pr + Type_Yes*Shirt_Yes*Weight
  
  
}


qr <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  Weight = Players$N[i]
  
  qr <- qr + Type_Yes*Shirt_Yes*Weight
  
  
}


pg <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  Weight = Players$N[i]
  
  pg <- pg + Type_Yes*Shirt_Yes*Weight
  
  
}


qg <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  Weight = Players$N[i]
  
  qg <- qg + Type_Yes*Shirt_Yes*Weight
  
  
}


# Percentage each type, shirt colour and response to R

prar <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  R_Response_Yes <- as.numeric(grepl("A", Players$R_Response[i]))
  
  Weight = Players$N[i]
  
  prar <- prar + Type_Yes*Shirt_Yes*Weight*R_Response_Yes
  
}


prag <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  G_Response_Yes <- as.numeric(grepl("A", Players$G_Response[i]))
  
  Weight = Players$N[i]
  
  prag <- prag + Type_Yes*Shirt_Yes*Weight*G_Response_Yes
  
}


pgar <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  R_Response_Yes <- as.numeric(grepl("A", Players$R_Response[i]))
  
  Weight = Players$N[i]
  
  pgar <- pgar + Type_Yes*Shirt_Yes*Weight*R_Response_Yes
  
}



pgag <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("B", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  G_Response_Yes <- as.numeric(grepl("A", Players$G_Response[i]))
  
  Weight = Players$N[i]
  
  pgag <- pgag + Type_Yes*Shirt_Yes*Weight*G_Response_Yes
  
}

qrar <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  R_Response_Yes <- as.numeric(grepl("A", Players$R_Response[i]))
  
  Weight = Players$N[i]
  
  qrar <- qrar + Type_Yes*Shirt_Yes*Weight*R_Response_Yes
  
}


qrag <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("R", Players$Shirt[i]))
  
  G_Response_Yes <- as.numeric(grepl("A", Players$G_Response[i]))
  
  Weight = Players$N[i]
  
  qrag <- qrag + Type_Yes*Shirt_Yes*Weight*G_Response_Yes
  
}


qgar <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  R_Response_Yes <- as.numeric(grepl("A", Players$R_Response[i]))
  
  Weight = Players$N[i]
  
  qgar <- qgar + Type_Yes*Shirt_Yes*Weight*R_Response_Yes
  
}



qgag <- 0 

for(i in 1:16){
  
  Type_Yes <- as.numeric(grepl("E", Players$Types[i]))
  
  Shirt_Yes <- as.numeric(grepl("G", Players$Shirt[i]))
  
  G_Response_Yes <- as.numeric(grepl("A", Players$G_Response[i]))
  
  Weight = Players$N[i]
  
  qgag <- qgag + Type_Yes*Shirt_Yes*Weight*G_Response_Yes
  
}








u_BRARAG <- 0.5 *(GAMMA * pr + GAMMA * pg - ALPHA * qr - ALPHA * qg) +
  0.5 * (GAMMA * prar - ALPHA * pgar - qrar - ALPHA * qgar)


u_BRARKG <- 0.5 *(GAMMA * pr - ALPHA * qr) +
  0.5 * (GAMMA * prar - ALPHA * pgar - qrar - ALPHA * qgar)


u_BRKRAG <- 0.5 *(GAMMA * pg - ALPHA * qg) +
  0.5 * (GAMMA * prar - ALPHA * pgar - qrar - ALPHA * qgar)


u_BRKRKG <- 0.5 * (GAMMA * prar - ALPHA * pgar - qrar - ALPHA * qgar)





u_BGARAG <- 0.5 * (-ALPHA * pr - ALPHA * pg + qr + qg) +
  0.5 * (GAMMA * prag - ALPHA * pgag - qrag - ALPHA * qgag) - DELTA


u_BGARKG <-  0.5 * (-ALPHA * pr + qr) + 
  0.5 * (GAMMA * prag - ALPHA * pgag - qrag - ALPHA * qgag) - DELTA


u_BGKRAG <-  0.5 * (- ALPHA * pg + qg) +
  0.5 * (GAMMA * prag - ALPHA * pgag - qrag - ALPHA * qgag) - DELTA


u_BGKRKG <- 0.5 * (GAMMA * prag - ALPHA * pgag - qrag - ALPHA * qgag) - DELTA





u_ERARAG <- 0.5 *(pr + pg - ALPHA * qr - ALPHA * qg) +
  0.5 * (-ALPHA * prar - pgar - ALPHA * qrar + GAMMA * qgar) - DELTA 


u_ERARKG <- 0.5 *(pr - ALPHA * qr) +
  0.5 * (-ALPHA * prar - pgar - ALPHA * qrar + GAMMA * qgar) - DELTA 


u_ERKRAG <- 0.5 *(pg- ALPHA * qg) +
  0.5 * (-ALPHA * prar - pgar - ALPHA * qrar + GAMMA * qgar) - DELTA 


u_ERKRKG <- 0.5 * (-ALPHA * prar - pgar - ALPHA * qrar + GAMMA* qgar) - DELTA 






u_EGARAG <- 0.5 *(-ALPHA * pr - ALPHA * pg + GAMMA * qr + GAMMA * qg) +
  0.5 * (-ALPHA * prag - pgag - ALPHA * qrag + GAMMA*qgag)


u_EGARKG <- 0.5 *(-ALPHA * pr + GAMMA * qr) +
  0.5 * (-ALPHA * prag - pgag - ALPHA * qrag + GAMMA*qgag)

u_EGKRAG <- 0.5 *(- ALPHA * pg + GAMMA * qg) +
  0.5 * (-ALPHA * prag - pgag - ALPHA * qrag + GAMMA*qgag)


u_EGKRKG <- 0.5 * (-ALPHA * prag - pgag - ALPHA * qrag + GAMMA*qgag)


Expected_u <- c(u_BRARAG, u_BRARKG, u_BRKRAG, u_BRKRKG,
                u_BGARAG, u_BGARKG, u_BGKRAG, u_BGKRKG,
                u_ERARAG, u_ERARKG, u_ERKRAG, u_ERKRKG,
                u_EGARAG, u_EGARKG, u_EGKRAG, u_EGKRKG)

Max_u <- max(Expected_u)

Min_u <- min(Expected_u)

Norm_Expected_u <- Expected_u

for(i in 1:16){

Norm_Expected_u[i] <- (Expected_u[i] - Min_u)/(Max_u-Min_u)

}



Calc_N <- Players$N

Population_Weighting <- 0


for(i in 1:16){
  
  Calc_N[i] <- Players$N[i] +  Norm_Expected_u[i]*Players$N[i]
  
  Population_Weighting <- Population_Weighting + Calc_N[i]
}


Calc_N <- round(Calc_N/Population_Weighting, digits = 6)


assign(glue("n{Round_Number}"), Calc_N) 

assign(glue("Payoffs_Round_{Round_Number}"), Expected_u)


}


Payoff_Dynamics <- c("BRARAG", "BRARKG", "BRKRAG", "BRKRKG", 
                     "BGARAG", "BGARKG", "BGKRAG", "BGKRKG",
                     "ERARAG", "ERARKG", "ERKRAG", "ERKRKG",
                     "EGARAG", "EGARKG", "EGKRAG", "EGKRKG") %>% as.data.frame

N_Dynamics <- c("BRARAG", "BRARKG", "BRKRAG", "BRKRKG", 
                "BGARAG", "BGARKG", "BGKRAG", "BGKRKG",
                "ERARAG", "ERARKG", "ERKRAG", "ERKRKG",
                "EGARAG", "EGARKG", "EGKRAG", "EGKRKG") %>% as.data.frame


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

