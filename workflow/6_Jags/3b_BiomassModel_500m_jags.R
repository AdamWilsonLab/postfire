
model {
  ## The recovery curve
  for (o in 1:nObs) {
    ndvi[o] ~ dnorm(mu[o], tau)
    mu[o] <- alpha[id[o]]+gamma[id[o]]-gamma[id[o]]*exp(-(age[o]/lambda[id[o]]))+
      sin((phi+((firemonth[o]-1)*3.141593/6))+6.283185*age[o])*A[id[o]]
  }
  
  ## Cell Level Means
  for (i in 1:nGrid) {  # loop through grid cells
    ## Draw the parameters
    alpha[i] ~ dlnorm(alpha.mu, alpha.tau)
    gamma[i] ~ dlnorm(gamma.mu[i], gamma.tau)
    lambda[i] ~ dlnorm(lambda.mu[i],lambda.tau)  
    A[i] ~ dlnorm(A.mu[i],A.tau)
  }

  ## month effects
  phi  ~ dunif(-3.141593,3.141593)
 
  ## Intercepts
  alpha.mu ~ dnorm(0.15,1/.1)
  ## Regressions
  gamma.mu <- env %*% gamma.beta
  lambda.mu <- env %*% lambda.beta 
  A.mu <- env %*% A.beta
  
  ## Beta priors
  for (l in 1:nBeta) {
    gamma.beta[l] ~ dnorm(0,0.1)
    lambda.beta[l] ~ dnorm(0,0.1)
    A.beta[l] ~ dnorm(0,0.1)
  }
  
  ## hyperpriors
  gamma.tau ~ dgamma(0.01,0.01)
  alpha.tau ~ dgamma(0.01,0.01)
  lambda.tau ~ dgamma(0.01,0.01)
  A.tau ~ dgamma(0.01,0.01)
  tau ~ dgamma(0.01,0.01)

  ## convert to SDs
  sigma<-1/sqrt(tau)
  gamma.sigma<-1/sqrt(gamma.tau)
  alpha.sigma<-1/sqrt(alpha.tau)
  lambda.sigma<-1/sqrt(lambda.tau)
  A.sigma<-1/sqrt(A.tau)
}

 
