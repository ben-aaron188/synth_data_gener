library(mvtnorm)

#############################################################################
################# ALMOST TRIVIAL START YET STILL A START ####################
#############################################################################

#basic fit function as used in e.g. SEM (compares two cov matrices)
# =0 if matrices are the same...the smallter the better...could serve as 
# operationalization of bias in evaluation
fit <- function(data, Sigma){
  s <- cov(data)
  n <- nrow(data)
  fml <- sum(diag(s %*% solve(Sigma))) -
    log(det(s %*% solve(Sigma))) - ncol(s)
  #for rmsea...needs degrees of freedom > a model 
  #chi2 <- fml*n 
  #when chi^2 distributed in this case????
  return(fml)
}


#generate positive definite matrix to use as covariance matrix
Posdef <- function (n, ev = runif(n, 0, 10)) 
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  diago <- diag(R)
  ph <- diago / abs(diago)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}



#inits

fitSumUp <- 0
fitSumDown <- 0
fitSumSim <- 0
iter <- 10

for (i in 1:iter){

  #create population data....with 3 features....
  #one cov mat for minority one for majority
  popMinMat <- Posdef(n=3, ev=1:3)
  datPopMin <- rmvnorm(n = 100000,sigma = popMinMat)

  popMajMat <- Posdef(n=3, ev=1:3)
  datPopMaj <- rmvnorm(n = 100000,sigma = popMajMat)


  ##############################################
  #sample from population data to create minority 
  #and majority groups
  ##############################################

  datMin <- datPopMin[sample(1:100000,500),]
  datMaj <- datPopMaj[sample(1:100000,1000),]


  ##############################################
  #simulate data to increase N in minority group
  ##############################################
  #cov mat based on data
  covMatMin <- cov(datMin)

  #sim data
  datMinSim <- rmvnorm(n = 400,sigma = covMatMin)
  datMinFull <- rbind(datMin, datMinSim)


  ##############################################
  #upsampling
  ##############################################
  datUpMin <- datMin[sample(1:100,1000, replace=TRUE),]



  ##############################################
  #downsampling
  ##############################################
  datDownMaj <- datMaj[sample(1:100),]


  ##############################################
  #which procedure performs best??
  ##############################################

  ##################################################
  # compare cov matrices with population cov matrices

  # first, calculate the variance-covariance matrices
  simCovMatMin <- cov(datMinFull)
  simCovMatMaj <- cov(datMaj)

  upCovMatMin <- cov(datUpMin)
  upCovMatMaj <- cov(datMaj)

  downCovMatMin <- covMatMin
  downCovMatMaj <- cov(datDownMaj)


  #evaluate for up&down sampling and simulated cases
  fitUp <- fit(datMin, upCovMatMin)
  fitDown <- fit(datMaj, downCovMatMaj)
  fitSim <- fit(datMin,simCovMatMin)

  fitSumDown <- fitSumDown + fitDown
  fitSumUp <- fitSumUp + fitUp
  fitSumSim <- fitSumSim + fitSim

}
fitSumDown/iter
fitSumUp/iter
fitSumSim/iter
