#Model A: NB-L Regression


model {
  
  for( i in 1 : 510) {
    #Mean Linear Predictor Equation
    log(mu1[i]) <- beta[1] + beta[2] * EXP[i] + beta[3] * NJUNC[i] + beta[4] * LANE[i] + beta[5] * RAIN[i] + beta[6] * PROSHB[i]
    #Lindley Error Term (eps[])
    mu[i] <- mu1[i] * eps[i]
    
    m[i] <- 1+zed[i]
    thetap <- 1/(1+theta)
    zed[i] ~ dbern(thetap)
    eps[i] ~ dgamma(m[i], theta)
   
    CRASH[i] ~ dnegbin(z[i],phi)
    z[i] <- phi/(phi + mu[i])
  }
  
  for (j in 1: 6) {beta[j] ~ dnorm(0,0.01)}
  phi ~ dexp(1)	
  theta ~ dexp(1)
  
}

#Inits
list(beta=c(0,0,0,0,0,0), phi=1,theta=1)
list(beta=c(0.1,0.1,0.1,0.1,0.1,0.1), phi=2,theta=0.1)



#Model B: Grouped RP NB-L Regression


model {
  
  for (j in 1:2) { for( i in 1 : 102) { beta1[i,j] ~ dnorm(Mbeta[j],prec[j]) } 
    
    for (i in 1:102) { beta[i,j] <- beta1[i,j] }
    for (i in 103:204) { beta[i,j] <- beta1[i-102,j] }
    for (i in 205:306) { beta[i,j] <- beta1[i-204,j] }
    for (i in 307:408) { beta[i,j] <- beta1[i-306,j] }
    for (i in 409:510) { beta[i,j] <- beta1[i-408,j] }
    
  }
  
  for( i in 1 : 510) {
    
    #Mean Linear Predictor Equation
    log(mu1[i]) <- alpha[1] + alpha[2]*EXP[i] + alpha[3]*RAIN[i] + alpha[4]*LANE[i] + alpha[5]*NJUNC[i] + beta[i,1]*CVAH[i] + beta[i,2]*NORDEAL[i]
    
    #Lindley Error Term (eps[i])
    mu[i] <- mu1[i] * eps[i]
    
    m[i] <- 1+zed[i]
    thetap <- 1/(1+theta)
    zed[i] ~ dbern(thetap)
    eps[i] ~ dgamma(m[i], theta)

    CRASH[i] ~ dnegbin(z[i],phi)
    z[i] <- phi/(phi + mu[i])
    
  }
  
  
  for (j in 1: 2) {Mbeta[j] ~ dnorm(0,0.01)}
  for (j in 1:2) { 
    prec[j] <- 1/SD[j]
    SD[j] ~ dexp(1)
  }
  
  for (j in 1: 5) {alpha[j] ~ dnorm(0,0.01)}
  phi ~ dexp(1)	
  theta ~ dexp(1)
  
}

#Inits
list(Mbeta=c(0,0),SD=c(1,1),alpha=c(0,0,0,0,0),phi=1,theta=1)
list(Mbeta=c(0.1,0.1),SD=c(2,2),alpha=c(1,1,1,1,1),phi=2,theta=0.1)


