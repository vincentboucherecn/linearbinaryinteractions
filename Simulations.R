#################################################
############# Libraries and Seed ################
#################################################


set.seed(123)
library(maxLik)
library(igraph)

#################################################
###### Parameters and global variables ##########
#################################################

### Many markets asymptotics, m groups of n individuals each

## set parameters and initialize variables
ns <- c(30) # list of group sizes (here only one size)
m <- 300 # number of groups
nlist <- rep(0,m) # initialize list of group sizes

## restriction on parameters (range x in [0,1]): alpha+beta+gamma+delta < 1, and all parameters positive
alpha <- 0.1
beta <- 0.25
gamma <- 0.2
delta <- 0.1

#################################################
################# Functions #####################
#################################################

gendta <-function(){
  # generates variables
  # creates global variables
  X <<- vector("list", m) # list of X
  G <<- vector("list", m) # list of G
  Y <<- vector("list", m) # list of Y
  for (i in 1:m){ # for each group i
    n <- sample(ns,1) # sample the group size in the available list
    nlist[i] <<- n # set group size
    tG <- as.matrix(erdos.renyi.game(n,0.1)[,]) # random adjacency matrix: ER network (size,proba link)
    #tG <- as.matrix(sample_smallworld(1, n, 5, 0.05)[,]) # small world (dim lattice, size, n neighbours, p-rewire) NOT USED
    diag(tG) <- 0 # zero on diagonal ()
    tG <- tG/matrix(rep(pmax(rowSums(tG),1),n),n,n) # row-normalize
    G[[i]] <<- tG # store G
    tX <- matrix(runif(n),n,1) # explanatory variable is uniform [0,1]
    X[[i]] <<- tX # store X
    bX <- solve(diag(n)-beta*tG)%*%(matrix(alpha,n,1)+gamma*tX + delta*tG%*%tX) # Probability of y=1
    tY <- matrix(sapply(c(bX), function(t) sample(c(1,0),1, prob = c(t,1-t))),n,1) # sample Y
    Y[[i]] <<- tY # storeG[[i]]%*%
  }
}

nlscon <- function(b){
## internal function NLS: computes the objective function conditional on beta

  out <- 0 # initialize objective
  Z <- vector("list", m) # list of groups
  for (i in 1:m){ # for each group
    nt <- nlist[i] # size of group i
    Z[[i]] <- solve(diag(nt)-b*G[[i]])%*%cbind(matrix(1,nt,1),X[[i]],G[[i]]%*%X[[i]]) # probability of y=1
  }
  LZ <- do.call(rbind,Z) # binds probabilities y=1
  LY <- do.call(rbind,Y) # binds y

## computes objective function
  int <- solve(t(LZ)%*%LZ)
  ext <- t(LZ)%*%LY
  out <- t(ext)%*%int%*%ext
  return(-out)
}


nls <- function(){
## NLS estimator
  b <- optim(0.1,nlscon, method='L-BFGS-B',lower=-0.999,upper = 0.999)$par # estimate the concentrated NLS
  nt <- nlist[1] # size of group 1
  bZ <- solve(diag(nt)-b*G[[1]])%*%cbind(matrix(1,nt,1),X[[1]],G[[1]]%*%X[[1]]) # probability that y=1 in the first group
  bY <- Y[[1]] # y for group 1
  for (i in 2:m){ # for groups>1
    nt <- nlist[i] # size of group i
    Z <- solve(diag(nt)-b*G[[i]])%*%cbind(matrix(1,nt,1),X[[i]],G[[i]]%*%X[[i]]) # probability of y=1 in group i
    bZ <- rbind(bZ,Z) # binds probabilities
    bY <- rbind(bY,Y[[i]]) # binds y
  }
  par <- solve(t(bZ)%*%bZ)%*%t(bZ)%*%bY # OLS conditional on beta
  return(c(par[1],b,par[2],par[3]))
}


tsls <- function(){
## perform 2SLS estimation
  n <- nlist[1] # size of group 1
  bY <- Y[[1]] # y in group 1
  bX <- cbind(matrix(1,n,1),X[[1]],G[[1]]%*%X[[1]],G[[1]]%*%Y[[1]]) # 1,X,GX,Gy in group 1
  bZ <- cbind(matrix(1,n,1),X[[1]],G[[1]]%*%X[[1]],G[[1]]%*%G[[1]]%*%X[[1]]) # 1,X,GX,G2X in group 1
  for (i in 2:m){
    n <- nlist[i] # size of group i
    tX <- cbind(matrix(1,n,1),X[[i]],G[[i]]%*%X[[i]],G[[i]]%*%Y[[i]]) # 1,X,GX,Gy in group i
    Z <- cbind(matrix(1,n,1),X[[i]],G[[i]]%*%X[[i]],G[[i]]%*%G[[i]]%*%X[[i]]) # 1,X,GX,G2X in group i
    bX <- rbind(bX,tX) # binds explanatory vars
    bZ <- rbind(bZ,Z) # binds instruments
    bY <- rbind(bY,Y[[i]]) # binds y
  }
  proj <- solve(t(bZ)%*%bZ)%*%t(bZ) # projection matrix
  par <- solve(t(bX)%*%bZ%*%proj%*%bX)%*%t(bX)%*%bZ%*%proj%*%bY # 2SLS estimator
  return(c(par[1],par[4],par[2],par[3]))
}

simul <- function(){
## performs 1 simulation
    out <- matrix(0,4,2) # stores results
    gendta() # generates new data
    out[,1] <- tsls() # performs 2SLS estimation
    out[,2] <- nls() # perform NLS estimation
  return(out)
}


comp_mean <- function(){
## computes the mean across simulations
  out <- matrix(0,4,2)
  for (i in 1:nsimu){
    out <- out + res[[i]]/nsimu
  }
  return(out)
}

comp_mean2 <- function(){
## computes E(x^2) across simulations (is used to compute std dev across simulations)
  out <- matrix(0,4,2)
  for (i in 1:nsimu){
    out <- out + (res[[i]]^2)/nsimu
  }
  return(out)
}


#################################################
################ Execute code ###################
#################################################


nsimu <- 1000 # number of simulations
res <- vector("list", nsimu) # results
for (i in 1:nsimu){ # for each simulation
  res[[i]] <- simul() # get simulation results
  print(i)
  print(res[[i]])
}

moy <- comp_mean() # mean
s <- sqrt(comp_mean2()-moy^2) # std. dev.


