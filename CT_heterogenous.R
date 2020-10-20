
#################################################
################# Libraries #####################
#################################################

library(foreign)
library(AER)
library(Matrix)
library(clubSandwich)
library(ivpack)
library(gtools)
library(mvtnorm)


#################################################
################# Format data ###################
#################################################

fe <- 2 # flag for Airline specific fixed effect (=1 is within deviation, =2 is using dummies)

prior <- read.dta("/home/vincent/Dropbox/LPM/applications/CT/Ecta5368-5/CilibertoTamerEconometrica.dta") # get data from CT
prior$A1 <- sapply(prior$market,function(x) substring(x,1,3)) # origine airport
prior$A2 <- sapply(prior$market,function(x) substring(x,4,6)) # destination airport
m <- nrow(prior) # number of markets
kx <- 10 # number of explanatory variables
v2k <- unique(permutations(n=6,r=6,v=c(1,0,0,0,0,0),set=F,repeats.allowed = T)) # list all possible matket combinations
l2k <- nrow(v2k) # number of possible market combinations
X <- vector("list", m) # each element will be a matrix of kx+1 column (column 1 is intercept)
Y <- vector("list", m) # each element will be a vector
G <- vector("list", m) # each element will ba a n x n adjacency matrix
n <- rep(6,m) # number of firms for each market is 6

for (i in 1:m){ # for each market
  nt <- n[i] # size of market i
  Gt <- matrix(1,nt,nt) # initialize G
  Yt <- matrix(0,nt,1) # initialize Y
  Xt <- matrix(0,nt,kx) # initialize X
  Yt[,1] <- as.numeric(prior[i,c(which(colnames(prior)=="airlineAA"):which(colnames(prior)=="airlineWN"))]) # y for market i
  Xt[,1] <- as.numeric(prior[i,c(which(colnames(prior)=="marketpresenceAA"):which(colnames(prior)=="marketpresenceWN"))]) # market presence for market i
  Xt[,2] <- as.numeric(prior[i,c(which(colnames(prior)=="mindistancefromhubAA"):which(colnames(prior)=="mindistancefromhubWN"))]) # distance from hub for market i
  # market variables for market i
  Xt[,3] <- prior[i,"wrightamendmDAL"]
  Xt[,4] <- prior[i,"dallasmarket"]
  Xt[,5] <- prior[i,"marketsize"]
  Xt[,6] <- prior[i,"marketdistance"]
  Xt[,7] <- prior[i,"mindistance"]
  Xt[,8] <- prior[i,"fromcenterdistance"]
  Xt[,9] <- prior[i,"percapitaincmarket"]
  Xt[,10] <- prior[i,"changeincmarket"]

  diag(Gt) <- 0 # zero on diagonal of G
  X[[i]] <- Xt # store X
  Y[[i]] <- Yt # store Y
  G[[i]] <- Gt # store G
}

## initialize vectors and matrices of averages (across markets)
mY <- matrix(0,nt,1)
mX <- matrix(0,nt,kx)
mGY1 <- matrix(0,nt,1)
mGX1 <- matrix(0,nt,2)
mGY2 <- matrix(0,nt,1)
mGX2 <- matrix(0,nt,2)
mGY3 <- matrix(0,nt,1)
mGX3 <- matrix(0,nt,2)
mGY4 <- matrix(0,nt,1)
mGX4 <- matrix(0,nt,2)
mGY5 <- matrix(0,nt,1)
mGX5 <- matrix(0,nt,2)
mGY6 <- matrix(0,nt,1)
mGX6 <- matrix(0,nt,2)

## compute the variables' averages (across markets)
for (i in 1:m){
  mY <- mY + Y[[i]]/m
  mX <- mX + X[[i]]/m
  Gt1 <- matrix(0,6,6)
  Gt1[,1] <- 1
  diag(Gt1) <- 0
  Gt2 <- matrix(0,6,6)
  Gt2[,2] <- 1
  diag(Gt2) <- 0
  Gt3 <- matrix(0,6,6)
  Gt3[,3] <- 1
  diag(Gt3) <- 0
  Gt4 <- matrix(0,6,6)
  Gt4[,4] <- 1
  diag(Gt4) <- 0
  Gt5 <- matrix(0,6,6)
  Gt5[,5] <- 1
  diag(Gt5) <- 0
  Gt6 <- matrix(0,6,6)
  Gt6[,6] <- 1
  diag(Gt6) <- 0
  mGY1 <- mGY1 + Gt1%*%Y[[i]]/m
  mGY2 <- mGY2 + Gt2%*%Y[[i]]/m
  mGY3 <- mGY3 + Gt3%*%Y[[i]]/m
  mGY4 <- mGY4 + Gt4%*%Y[[i]]/m
  mGY5 <- mGY5 + Gt5%*%Y[[i]]/m
  mGY6 <- mGY6 + Gt6%*%Y[[i]]/m
  mGX1 <- mGX1 + Gt1%*%X[[i]][,1:2]/m
  mGX2 <- mGX2 + Gt2%*%X[[i]][,1:2]/m
  mGX3 <- mGX3 + Gt3%*%X[[i]][,1:2]/m
  mGX4 <- mGX4 + Gt4%*%X[[i]][,1:2]/m
  mGX5 <- mGX5 + Gt5%*%X[[i]][,1:2]/m
  mGX6 <- mGX6 + Gt6%*%X[[i]][,1:2]/m
}

#################################################
################## Functions ####################
#################################################


bdf <- function(){
## constructs a dataset for the 2SLS estimation

  siz <- sum(n) # total number of observations
  bX <- matrix(1,siz,(kx+6)) # regressors are X + gy
  bZ <- matrix(1,siz,12) # instruments are GX (excluding constant)
  bY <- matrix(1,siz,1) # explained var is Y
  clust <- matrix(1,siz,1) # market number
  for (school in 1:m){ # for each market "school"
## position of the first firm in market "school"
    if (school==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(school-1)])+1
    }
    p2 <- p1+n[school]-1 # position of the last firm in market "school"
    Yt <- Y[[school]] # y
    Xt <- X[[school]] # x
    Gt1 <- matrix(0,6,6) # interaction with firm 1
    Gt1[,1] <- 1
    diag(Gt1) <- 0
    Gt2 <- matrix(0,6,6)  # interaction with firm 2
    Gt2[,2] <- 1
    diag(Gt2) <- 0
    Gt3 <- matrix(0,6,6)  # interaction with firm 3
    Gt3[,3] <- 1
    diag(Gt3) <- 0
    Gt4 <- matrix(0,6,6)  # interaction with firm 4
    Gt4[,4] <- 1
    diag(Gt4) <- 0
    Gt5 <- matrix(0,6,6) # interaction with firm 5
    Gt5[,5] <- 1
    diag(Gt5) <- 0
    Gt6 <- matrix(0,6,6) # interaction with firm 6
    Gt6[,6] <- 1
    diag(Gt6) <- 0
    nt <- n[i] # number of firms
    
    GYt1 <- Gt1%*%Yt # interactions with firm 1
    GXt1 <- Gt1%*%Xt[,1:2] # instrumetns for firm 1
    GYt2 <- Gt2%*%Yt # interactions with firm 2
    GXt2 <- Gt2%*%Xt[,1:2] # instrumetns for firm 2
    GYt3 <- Gt3%*%Yt # interactions with firm 3
    GXt3 <- Gt3%*%Xt[,1:2] # instrumetns for firm 3
    GYt4 <- Gt4%*%Yt # interactions with firm 4
    GXt4 <- Gt4%*%Xt[,1:2] # instrumetns for firm 4
    GYt5 <- Gt5%*%Yt # interactions with firm 5
    GXt5 <- Gt5%*%Xt[,1:2] # instrumetns for firm 5
    GYt6 <- Gt6%*%Yt # interactions with firm 6
    GXt6 <- Gt6%*%Xt[,1:2] # instrumetns for firm 6
    
    if (fe==1){ # if fe=1, substract averages
      Yt <- Yt - mY
      Xt <- Xt - mX
      GYt1 <- GYt1 - mGY1
      GXt1 <- GXt1 - mGX1
      GYt2 <- GYt2 - mGY2
      GXt2 <- GXt2 - mGX2
      GYt3 <- GYt3 - mGY3
      GXt3 <- GXt3 - mGX3
      GYt4 <- GYt4 - mGY4
      GXt4 <- GXt4 - mGX4
      GYt5 <- GYt5 - mGY5
      GXt5 <- GXt5 - mGX5
      GYt6 <- GYt6 - mGY6
      GXt6 <- GXt6 - mGX6
    }
    Jt <- diag(6)
    bX[p1:p2,] <- Jt%*%cbind(Xt,GYt1,GYt2,GYt3,GYt4,GYt5,GYt6) # X, Gy1,...Gy6
    bZ[p1:p2,] <- Jt%*%cbind(GXt1,GXt2,GXt3,GXt4,GXt5,GXt6) # GX1,...,GX6
    bY[p1:p2,1] <- Jt%*%Yt # Y
    clust[p1:p2,1] <- school # market number
  }
  return(as.data.frame(cbind(bY,bX,bZ,clust)))
}


predfit <- function(probfit){
  ## computes number of correctly predicted markets
  obj <- 0 # initialize
  for (i in 1:m){
    ## position of the first firm in market i
    if (i==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(i-1)])+1
    }
    nt <- n[i] # size of market i=6
    p2 <- p1 + nt -1 # position of the last firm in market i
    Pi <- pmin(pmax(probfit[p1:p2],0),1) # predicted probability for y=1
    fit <- rep(0,l2k) # initialize probas
    for (j in 1:l2k){ # for each possible market structure
      yt <- c(v2k[j,]) # get the market structure
      fit[j] <- prod( (Pi^yt)*(1-Pi)^(1-yt)  ) # predicted probability
    }
    j <- which.max(fit) # market structure with the highest predicted probability
    fit <- all(v2k[j,]==c(Y[[i]])) # is observation = predicted market structure
    obj <- obj + sum(as.numeric(fit))
  }
  return(obj)
}

predictprob <- function(para){
  # computes the variance-covariance matrix for the NLS
  siz <- sum(n) # total number of individuals
  grad <- matrix(0,siz,1) # initialize
  if (fe==0){
    mpara <- matrix(para[1:(length(para)-6)],(length(para)-6),1) # parameter values as matrix
  } else {
    mpara <- matrix(c(para[1:(kx)],para[(kx+7):length(para)]),(length(para)-6),1) # parameter values as matrix
  }
  
  for (i in 1:m){ # for each group i
    nt <- n[i] # size of group i
    #position of the first individual in group i
    if (i==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(i-1)])+1
    }
    p2 <- p1+n[i]-1 # position of the last individual in group i
    if (fe==0){
      Gt1 <- matrix(0,6,6) # interaction with firm 1
      Gt1[,1] <- 1
      diag(Gt1) <- 0
      Gt2 <- matrix(0,6,6)  # interaction with firm 2
      Gt2[,2] <- 1
      diag(Gt2) <- 0
      Gt3 <- matrix(0,6,6)  # interaction with firm 3
      Gt3[,3] <- 1
      diag(Gt3) <- 0
      Gt4 <- matrix(0,6,6)  # interaction with firm 4
      Gt4[,4] <- 1
      diag(Gt4) <- 0
      Gt5 <- matrix(0,6,6) # interaction with firm 5
      Gt5[,5] <- 1
      diag(Gt5) <- 0
      Gt6 <- matrix(0,6,6) # interaction with firm 6
      Gt6[,6] <- 1
      diag(Gt6) <- 0
      betat <- Gt1*para[(kx+1)]+Gt2*para[(kx+2)]+Gt3*para[(kx+3)]+Gt4*para[(kx+4)]+Gt5*para[(kx+5)]+Gt6*para[(kx+6)]
      Minv <- solve(diag(nt)-betat) # computes the inverse (I-betaG)^(-1)
      bZ <- Minv%*%cbind(matrix(1,nt,1),X[[i]]) # computes the probability of y=1
    } else {
      Gt1 <- matrix(0,6,6) # interaction with firm 1
      Gt1[,1] <- 1
      diag(Gt1) <- 0
      Gt2 <- matrix(0,6,6)  # interaction with firm 2
      Gt2[,2] <- 1
      diag(Gt2) <- 0
      Gt3 <- matrix(0,6,6)  # interaction with firm 3
      Gt3[,3] <- 1
      diag(Gt3) <- 0
      Gt4 <- matrix(0,6,6)  # interaction with firm 4
      Gt4[,4] <- 1
      diag(Gt4) <- 0
      Gt5 <- matrix(0,6,6) # interaction with firm 5
      Gt5[,5] <- 1
      diag(Gt5) <- 0
      Gt6 <- matrix(0,6,6) # interaction with firm 6
      Gt6[,6] <- 1
      diag(Gt6) <- 0
      betat <- Gt1*para[(kx+1)]+Gt2*para[(kx+2)]+Gt3*para[(kx+3)]+Gt4*para[(kx+4)]+Gt5*para[(kx+5)]+Gt6*para[(kx+6)]
      Minv <- solve(diag(nt)-betat) # computes the inverse (I-betaG)^(-1)
      bZ <- cbind(X[[i]],diag(nt))
      bZ <- Minv%*%bZ # computes the probability of y=1
    }
    
    grad[p1:p2,1] <- bZ%*%mpara # computes predicted proba
  }
  return(c(grad))
}


#################################################
################# Execute Code###################
#################################################


dta <- bdf() # construct dataset

# name variables
cname <- c("MarketPres","DistHub","Wright","Dallas","Msize","Mdist","mindist","centerdist","percapinc","changeinc")
cname <- c("Y",cname,"GY1","GY2","GY3","GY4","GY5","GY6","GX11","GX21","GX12","GX22","GX13","GX23","GX14","GX24","GX15","GX25",
           "GX16","GX26","school")
colnames(dta) <- cname
dta$airline <- factor(rep(1:6,m))

# create formula
expl <- paste(cname[2:(kx+7)], collapse=" + ")
instr <- paste(cname[(kx+8):(kx+19)], collapse = " + ")
if (fe==1){
  formul <- as.formula(paste(paste( paste("Y", expl, sep=" ~ 0 + "), ". - GY1 - GY2 - GY3 - GY4 - GY5 - GY6", sep=" | "),instr,sep=" + "))
} else if (fe==2) {
  expl <- paste(c("0", expl,"airline"), collapse=" + ")
  formul <- as.formula(paste(paste( paste("Y", expl, sep=" ~ "), ". - GY1 - GY2 - GY3 - GY4 - GY5 - GY6", sep=" | "),instr,sep=" + "))
} else {
  formul <- as.formula(paste(paste( paste("Y", expl, sep=" ~ "), ". - GY1 - GY2 - GY3 - GY4 - GY5 - GY6", sep=" | "),instr,sep=" + "))
}

# estimate
out <- ivreg(formul, data = dta) # 2SLS
cluster.robust.se(out, dta$school) # clustered SE

if (fe !=1 ){
  probfit <- predictprob(out$coefficients)
  
  pred <- predfit(probfit)/m # fraction of correctly predicted market structures
  print(pred)
  print(mean(probfit>=0 & probfit<=1))
}


Evcov <- vcovCL(out,cluster=dta$school)
if (fe==0){
  Evcov <- Evcov[(nrow(Evcov)-5):nrow(Evcov),(nrow(Evcov)-5):nrow(Evcov)]
  Ebeta <- out$coefficients[(length(out$coefficients)-5):length(out$coefficients)]
} else if (fe==2){
  Evcov <- Evcov[(kx+1):(kx+6),(kx+1):(kx+6)]
  Ebeta <- out$coefficients[(kx+1):(kx+6)]
}


nsim <- 10000
val <- rep(NA,nsim)
for (sim in 1:nsim){
  tbeta <- rmvnorm(n=1,Ebeta,Evcov)
  val[sim] <- sum(abs(tbeta))
}
print(mean(val>=1))
