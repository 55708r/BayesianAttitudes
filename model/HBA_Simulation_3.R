# Hierarchical Bayesian Atittudes Model Simulations 3

# Testing different setups of gamma: i. lower level weighted more ii. top-down level weighted slightly more, as operative/functional metacognition iii. strong weighting on higher-level, lower level heavily discounted

# Holding other conditions the same: initial activations (x, X) , weights matrix, lower-level flipping probabilities, which node is sampled, 

library(qgraph)
library(ggplot2)
library(ggpubr)
library(writexl)
library(readxl)
library(random)
library(gridExtra)
options(scipen=999)

ID = round(runif(1,min=1000, max=9999), digits=0)


# layout(matrix(1:3,3,1,byrow=T))


#### Initializing LIMA x AI ####

T = 100
n_nodes = 10

holdconstant = TRUE

intervention1a = FALSE
intervention1b = FALSE
intervention2 = FALSE
intervention3 = TRUE
Tint = 20

if(holdconstant){
  R_T_hc_100 <- read_excel("R_Tdf100.xlsx")
  R_T <- t(as.vector(R_T_hc_100))
  weights_hc <- read_excel("weightsdf.xlsx")
  weights <- as.matrix(weights_hc)
  x = c(-1, -1, -1, -1, -1, 1, 1, 1, 1, 1)
}


if(holdconstant==FALSE){
  x = sample(c(-1,1),n_nodes,replace=T)
  weights <- matrix(data = rnorm(100, mean = 0, sd = 0.01), n_nodes, n_nodes)
}

tau = array(-.01,dim=c(n_nodes)) #lower level dispositions

I = 1
Cp = 0.001
tw = 10
epsilon = 0.001
lambda = 0.001

# Precision weighting setups

Sim2s1HA = FALSE
Sim2s1LA = FALSE

Sim2s2HA = FALSE
Sim2s2LA = FALSE

Sim2s3HA = FALSE
Sim2s3LA = TRUE

if(Sim2s1HA){
  A = 2
  gamma = 0.5
  gammaTAU = 0.01
  gammaE = 1
  gammaHtoL = 0.01
  gammaR = 1
}

if(Sim2s1LA){
  A = 0.5
  gamma = 0.5
  gammaTAU = 0.01
  gammaE = 1
  gammaHtoL = 0.01
  gammaR = 1
}

if(Sim2s2HA){
  A = 2
  gamma = 3.5
  gammaTAU = 2
  gammaE = 1
  gammaHtoL = 1
  gammaR = 1
}

if(Sim2s2LA){
  A = 0.5
  gamma = 3.5
  gammaTAU = 2
  gammaE = 1
  gammaHtoL = 1
  gammaR = 1
}

if(Sim2s3HA){
  A = 2
  gamma = 10
  gammaTAU = 5
  gammaE = 0.3
  gammaHtoL = 1
  gammaR = 1
}

if(Sim2s3LA){
  A = 0.5
  gamma = 10
  gammaTAU = 5
  gammaE = 0.3
  gammaHtoL = 1
  gammaR = 1
}


# AI initial values
X = array(0,dim=c(2,T+1))
X[,1] = c(.5,.5) # Higher level (HL) states: veg or non-veg attitude
TAU = array(0,dim=c(n_nodes,2,T)) 
TAU[,1,] = rep(.2,n_nodes)
TAU[,2,] = rep(-.1,n_nodes)


x_T = matrix(0,n_nodes,T+1)
x_T[,1] = x
weights_T = array(0,dim=c(n_nodes,n_nodes,T+1))
weights_T[,,1] = weights
weights_mean_T = array(0,dim=c(T+1)) #For plotting weights average per iteration
weights_mean_T[1] = mean(weights[row(weights)!=col(weights)]) #Average of all weights, excl. diagonal
tau_mean_T = array(0,dim=c(T+1))
tau_mean_T[1] = mean(tau)
tau2_mean_T = array(0,dim=c(T+1))
tau2_mean_T[1] = mean(gammaTAU * (TAU[,1,1] * X[1,1] + TAU[,2,1] * X[2,1])) 
A_T = array(0,dim=c(T+1))
A_T[1] = A
A_T2 = array(0,dim=c(T+1)) + gammaHtoL

U_T = array(0,dim=c(T+1))
U_T[1] = 0
F_T = array(0,dim=c(T+1))
sum_x_T = array(0,dim=c(T+1))
sum_x_T[1] = sum(x)

mean_TAU1 <- mean(TAU[,1,])
mean_TAU2 <- mean(TAU[,2,])

#### Active inference extension ####

# HL transition
B = array(0, dim=c(2,2))
B[,] = c(0.7, 0.3, 0.3, 0.7)
Bbar = array(0,dim=c(2,2))
Bnorm = 0

# Higher-level transition probability

for(i in 1:2){
  for (j in 1:2){
    Bnorm = sum(B[,]**gamma)
    Bbar[i,j]=B[i,j]**gamma/sum(B[,j]**gamma)
  }
}

B[,] = Bbar

# Report

Report = array(-1,dim=c(1,T+1))

# Forced choice report
R0tau = array(0,3) #No report/undecided
R0tau = c(1, 1, 1)

R1tau = array(0,3) # Vegetarian
R1tau = c(0, 3, -3)

R2tau = array(0,3) # Non-vegetarian
R2tau = c(0, -3, 3)

#Measuring entropy
Entropy_T = array(0, dim=c(2,T))
FE_T = array(0, dim=c(2,T))

params <- data.frame(Sim2s1HA, Sim2s1LA, Sim2s2HA, Sim2s2LA, Sim2s3HA, Sim2s3LA,
                     A, I, Cp, tw, T, epsilon, lambda, gamma, gammaTAU, gammaE, tau_mean_T[1], mean(x), n_nodes, A_T2[1], mean_TAU1, mean_TAU2, R_T,
                     R0tau[1], R0tau[2], R0tau[3], R1tau[1], R1tau[2], R1tau[3], R2tau[1],  R2tau[2],  R2tau[3])

#### Settings ####

learning = TRUE
AI = TRUE
Rep = TRUE
Rep_BU = TRUE 

#### Glauber dynamics ####

for (t in 1:T) {
  
  # The intervention
  if(intervention1a){
    if((t>T/2) & t<(T/2+(T/5))){
      x = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1)
    }}
  
  if(intervention1b){
    if((t>T/2) & t<(T/2+(T/5))){
      X[,t] = c(.1,.9) #First state is vegetarian, second is non-veg
    }}
  
  if(intervention2){
    if(t>T/2){
      Rep = TRUE
      Rep_BU = TRUE
    }}
  
  
  # Top-down updating
  if(AI){
    tau2 = gammaTAU * (TAU[,1,t] * X[1,t] + TAU[,2,t] * X[2,t]) 
  }
  
  E = c(0,0) #Vector for storing current state and opposite state energy
  E2 = c(0,0)
  
  # if(holdconstant){
  # set.seed(4567)}
  i = sample(1:n_nodes,1) #Sample a random node i
  E2[1] = -tau2[i] * x[i] #Add disposition to stay
  E2[2] = -tau2[i] * -x[i] #Add disposition to flip
  
  E[1] = -tau[i] * x[i] #Add disposition to stay
  E[2] = -tau[i] * -x[i] #Add disposition to flip
  
  P0_flip = 1 / (1 + exp(-A_T[t] * (E[1] - E[2]) - A_T2[t] * (E2[1] - E2[2])))
  
  for (j in 1:10) {
    if (i != j) {
      E[1] = E[1] - weights[i,j] * x[i] * x[j]
      E[2] = E[2] - weights[i,j] * -x[i] * x[j]
    }
  }
  
  P_flip = 1 / (1 + exp(-A_T[t] * (E[1] - E[2]) - A_T2[t] * (E2[1] - E2[2])))
  
  Entropy_T[1,t] =   -1 * (P_flip * log(P_flip) + (1-P_flip) * log(1-P_flip))
  
    # The free energy of the lower level
  FE_T[2,t] = (1 - P_flip) * E[1] + P_flip * E[2] 
  FE_T[2,t] =  FE_T[2,t] + P_flip * log(P_flip/P0_flip) + (1-P_flip) * log((1-P_flip)/(1-P0_flip))

  
  
  if(holdconstant==FALSE){
    R = runif(1)}
  
  if(holdconstant){
    R = R_T[t]}
  
  if (R <= P_flip){
    x[i] = -x[i]
    F = 1 } 
  if(R > P_flip){
    F = 0}
  
  F_T[t] = F
  
  x_T[,t+1] = x
  
  
  if(learning)
    for (i in 1:n_nodes){
      for(j in 1:n_nodes){
        if (i != j){
          weights[i,j] = weights[i,j] + (epsilon * (1 - abs(weights[i,j] * x[i] * x[j] - lambda * weights[i,j])))
        }}}
  
  for (i in 1:n_nodes){
    tau[i] = tau[i] + (epsilon * (1 - abs(tau[i]) * x[i] - lambda * tau[i]))
  }
  
  weights_T[1:n_nodes,1:n_nodes,t+1] = weights
  
  
  if(t>=tw){
    U = mean(F_T[t-(tw-1):t])
    U_T[t] = U
    A_T[t+1] = (1-Cp) * A_T[t] + Cp * (I + I * U_T[t] - A_T[t])
  }
  if(t<tw){
    A_T[t+1] = A
  }
  
  
  weights_mean_T[t+1] = mean(weights[row(weights)!=col(weights)])
  
  sum_x_T[t+1] = sum(x)
  
  tau_mean_T[t+1] = mean(tau)
  tau2_mean_T[t+1] = mean(tau2)
  
  #### Report ####
  
  if(AI){
    if(Rep){
      Rtau = R0tau + R1tau*X[1,t] + R2tau*X[2,t] # get a vector of taus
      
      # Reporting probabilities
      
      P_Rtau = array(0,3)
      P_Rtau[1] = exp(Rtau[1])/sum(exp(Rtau[1]),exp(Rtau[2]),exp(Rtau[3])) #No rep
      P_Rtau[2] = exp(Rtau[2])/sum(exp(Rtau[1]),exp(Rtau[2]),exp(Rtau[3])) #Veg
      P_Rtau[3] = exp(Rtau[3])/sum(exp(Rtau[1]),exp(Rtau[2]),exp(Rtau[3])) #Non-veg
      
      # Choosing the report
      reports <- c(0.5, 1, 0)
      Report[,t] = sample(reports, size = 1, prob = c(P_Rtau[1], P_Rtau[2], P_Rtau[3])) #Outcome states weighed by probabilities calculated above

            ##### Intervention ####
      if(intervention3){
        if((t>T/2) & t<(T/2+Tint)){
          Report[,t] = 1
        }}
      
      # Bottom-up updating, from lower level nodes and report node
      
      if(Rep_BU){
        
        # Neutral report
        if (Report[,t]==0.5){
          P_X1 = log(X[1,t]+10**-7) + gammaE * sum(TAU[,1,t] * x) + gammaR * (R0tau[2]/n_nodes)
          P_X2 = log(X[2,t]+10**-7) + gammaE * sum(TAU[,2,t] * x) + gammaR * (R0tau[3]/n_nodes)
          
          X[1,t+1] = exp(P_X1)/sum(exp(P_X1),exp(P_X2))
          X[2,t+1] =  1 - X[1,t+1]
        }
        
        # Vegetarian report
        if (Report[,t]==1){
          P_X1 = log(X[1,t]+10**-7) + gammaE * sum(TAU[,1,t] * x) + gammaR * (R1tau[2]/n_nodes)
          P_X2 = log(X[2,t]+10**-7) + gammaE * sum(TAU[,2,t] * x) + gammaR * (R1tau[3]/n_nodes)
          
          X[1,t+1] = exp(P_X1)/sum(exp(P_X1),exp(P_X2))
          X[2,t+1] =  1 - X[1,t+1]
        }
        
        # Non-vegetarian report
        if (Report[,t]==0){
          P_X1 = log(X[1,t]+10**-7) + gammaE * sum(TAU[,1,t] * x) + gammaR * (R2tau[2]/n_nodes)
          P_X2 = log(X[2,t]+10**-7) + gammaE * sum(TAU[,2,t] * x) + gammaR * (R2tau[3]/n_nodes)
          
          X[1,t+1] = exp(P_X1)/sum(exp(P_X1),exp(P_X2))
          X[2,t+1] =  1 - X[1,t+1]
        }
      }
    }
    
    # Bottom-up updating, only from the lower level nodes
    
    if(Rep_BU==FALSE){
      P_X1 = log(X[1,t]+10**-7) + gammaE * sum(TAU[,1,t] * x)
      P_X2 = log(X[2,t]+10**-7) + gammaE * sum(TAU[,2,t] * x)
      
      # Inserting the approximate posterior of time t in the state beliefs X
      X[1,t+1] = exp(P_X1)/sum(exp(P_X1),exp(P_X2))
      X[2,t+1] =  1 - X[1,t+1]
    }
    
    #### Free energy computation HL ####
    
    tau2 = gammaTAU * (TAU[,1,t] * X[1,t+1] + TAU[,2,t] * X[2,t+1]) 
    
    P_x = exp(tau2[i])/sum(exp(tau2[i]),exp(-1*tau2[i]))
    
    if(x[i]==1){
      FE_T[1,t] = -log(P_x)
    }
    if(x[i]==-1){
      FE_T[1,t] = -log(1-P_x)
    }
    
    # FE_T[1,t] = -2 * (tau2[i] * x[i])
    
    #the higher level FE
    
    # tau2 is already a weighted sum of taus?
    #measuring how surprising is your observation. 
    # it's the prediction error
    # FE of the higher level (the quantity that the higher level is minimizing
    # to optimize its beliefs)
    
    FE_T[1,t] = FE_T[1,t] + X[1,t+1] * log(X[1,t+1]/X[1,t]) + X[2,t+1] * log(X[2,t+1]/X[2,t])
    
    Entropy_T[2,t] = -1 * (X[1,t+1] * log(X[1,t+1]) + (X[2,t+1]) * log(X[2,t+1]))
    
    X[,t+1] = B[,]%*%X[,t+1]
    
  }}


#### Plotting the dynamics ####

sumscores <-as.numeric(sum_x_T/20+0.5)
HL <- as.numeric(X[1,])
reports <- as.numeric(Report)
ccolors <- c("Lower level" = "black", "Higher level" = "#1497EE", "Report" = "#35FF69")

# png(filename=sprintf("%s.png",ID), pointsize=8, width=2400, height=975, res=400)


if(Sim2s1HA){
  p1 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s1HA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}

if(Sim2s1LA){
  p2 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s1LA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}
if(Sim2s2HA){
  p3 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s2HA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}
if(Sim2s2LA){
  p4 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s2LA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}
##
if(Sim2s3HA){
  p5 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s3HA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}

if(Sim2s3LA){
  p6 <- ggplot(mapping = aes(sumscores)) +
    geom_line(aes((x=0:T), y=sumscores, color ="Lower level")) +
    ylim(0, 1) +
    geom_line(aes((x=0:T), y=HL, color="Higher level")) +
    geom_point(aes((x=1:(T+1)), y=reports, color="Report"), size=1) +
    labs(x = "Iteration",
         y = "",
         color = " ") + 
    ggtitle("Sim2s3LA") +
    scale_color_manual(values = ccolors) +
    theme_light()
}


# Storing simulation data
simdata <- data.frame(sum_x_T, weights_mean_T, tau_mean_T, tau2_mean_T, A_T, U_T, A_T2, Entropy_T[1,], Entropy_T[2,], FE_T[1,], FE_T[2,])
write_xlsx(simdata, sprintf("/Users/Izabele/%s_simdata.xlsx",ID))
write_xlsx(params, sprintf("/Users/Izabele/%s_params.xlsx",ID))

#### Plotting E and FE ####

entropyLL <- as.numeric(Entropy_T[1,])
entropyHL <- as.numeric(Entropy_T[2,])
FE_HL <- as.numeric(FE_T[1,])
FE_LL <- as.numeric(FE_T[2,])
ecolors <- c("Lower level" = "#885053", "Higher level" = "#FA824C")
fecolors <- c("Lower level" = "#573280", "Higher level" = "#E85D75")

png(filename=sprintf("%s_E.png",ID), pointsize=8, width=2400, height=975, res=400)

e <- ggplot(mapping = aes(entropyLL)) +
  geom_line(aes((x=1:T), y=entropyLL, color ="Lower level")) +
  ylim(0, 1) +
  geom_line(aes((x=1:T), y=entropyHL, color="Higher level")) +
  labs(x = "Iteration",
       y = "",
       color = " ") + 
  ggtitle("Entropy") +
  scale_color_manual(values = ecolors) 
# +theme_light()

e

dev.off()

png(filename=sprintf("%s_FE.png",ID), pointsize=8, width=2400, height=975, res=400)

fe <- ggplot(mapping = aes(entropyLL)) +
  geom_line(aes((x=1:T), y=FE_LL, color ="Lower level")) +
  ylim(-3, 3) +
  geom_line(aes((x=1:T), y=FE_HL, color="Higher level")) +
  labs(x = "Iteration",
       y = "",
       color = " ") + 
  ggtitle("Free energy") +
  scale_color_manual(values = fecolors) 
# +theme_light()

fe

dev.off()


png(filename=sprintf("%s.png",ID), pointsize=8, width=2400, height=975, res=400)


if(Sim2s1HA){
  p1
}
if(Sim2s1LA){
  p2
}
if(Sim2s2HA){
  p3
}
if(Sim2s2LA){
  p4
}
if(Sim2s3HA){
  p5
}
if(Sim2s3LA){
  p6
}

dev.off()
dev.off()

if(Sim2s1HA){
  show(p1)
}
if(Sim2s1LA){
  show(p2)
}
if(Sim2s2HA){
  show(p3)
}
if(Sim2s2LA){
  show(p4)
}
if(Sim2s3HA){
  show(p5)
}
if(Sim2s3LA){
  show(p6)
}