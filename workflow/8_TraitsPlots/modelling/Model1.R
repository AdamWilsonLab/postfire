model {
  ## The recovery curve
  for (o in 1:nObs) {
    ndvi[o] ~ dnorm(mu[i], tau)
  }
  
  ## Cell Level Means
  for (i in 1:nGrid) {  # loop through grid cells
    ## Draw the parameters
    alpha[i] ~ dlnorm(alpha.mu, alpha.tau)
    gamma[i] ~ dlnorm(gamma.mu[i], gamma.tau)
    lambda[i] ~ dlnorm(lambda.mu[i],lambda.tau)  
  }
 
  ## Intercepts
  alpha.mu ~ dnorm(0.15,1/.1)
  ## Regressions
  gamma.mu <- env %*% gamma.beta
  lambda.mu <- env %*% lambda.beta 
  
  ## Beta priors
  for (l in 1:nBeta) {
    gamma.beta[l] ~ dnorm(0,0.1)
    lambda.beta[l] ~ dnorm(0,0.1)
  }
  
  ## hyperpriors
  gamma.tau ~ dgamma(0.01,0.01)
  alpha.tau ~ dgamma(0.01,0.01)
  lambda.tau ~ dgamma(0.01,0.01)
  tau ~ dgamma(0.01,0.01)

  ## convert to SDs
  gamma.sigma<-1/sqrt(gamma.tau)
  alpha.sigma<-1/sqrt(alpha.tau)
  lambda.sigma<-1/sqrt(lambda.tau)
  sigma<-1/sqrt(tau)
}

 
