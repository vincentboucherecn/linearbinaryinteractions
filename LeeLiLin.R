#################################
### load external libraries #####
#################################

library(foreign)
library(AER)
library(Matrix)
library(ivpack)

#################################
######## Prepare dataset ########
#################################

fe <- 0 # fixed effect flag. fe=1 for school-grade fixed effects.
dummy <- 0 # school dummies flag. dummy=1 for school dummies, automatically disregards fe=1 if present.
prior <- read.dta("/home/vincent/Desktop/tmpah/Desktop/ecnaddhealth/LPM/cleandta.dta") # dataset constructed using the code from Lee Li Lin (2014)
prior$sschlcde <- prior$scid*100+prior$grade # generates a unique identifier for each group (school-grade)
listschools <- unique(prior$scid) # list of schools
nschools <- length(listschools) # number of schools
whichschool <- unique(prior[,c("scid","sschlcde")]) # matrix linking each school-grade index to its respective school id.

listsch <- unique(prior$sschlcde) # list of groups (school-grade)
m <- length(listsch) # number of groups
kx <- 17 # number of explanatory variables
X <- vector("list", m) # each element will be a matrix of kx+1 column (column 1 is intercept)
Y <- vector("list", m) # each element will be a vector
G <- vector("list", m) # each element will ba a n x n adjacency matrix
n <- rep(1,m) # initialize vector of group sizes

for (i in 1:m){ # for each group
  tdta <- prior[prior$sschlcde==listsch[i],] # keep data for group i
  nt <- nrow(tdta) # number of students in group i
  Gt <- matrix(0,nt,nt) # initialize G matrix
  Yt <- matrix(0,nt,1) # initialize y vector
  Xt <- matrix(0,nt,kx) # initialize X matrix
  n[i] <- nt # store group size
  for (j in 1:nt){  # for each individual in group i
    Yt[j,1] <- as.numeric(tdta[j,"smoke"]) # endogenous variable
    Xt[j,] <- as.numeric(tdta[j,c(which(colnames(tdta)=="age"):which(colnames(tdta)=="momjob_miss"))]) # individual variables
    lstfr <- c(which(colnames(tdta)=="fid1"):which(colnames(tdta)=="fid10")) # unique identifiers for j's friends
    for (k in lstfr){ # for each potential friendship relations
      if (!is.na(tdta[j,k])){ # if named a friend
        fr <- which(tdta$aid==tdta[j,k]) # find friend k' row number
        if (length(fr)>0){ # if friend k is found
          Gt[j,fr[1]] <- 1 # add link
        }
      }
    }
  }
  diag(Gt) <- 0 # zero on diagonal
  Gt <- Gt/matrix(rep(pmax(rowSums(Gt),1),nt),nt,nt) # row-normalize G
  X[[i]] <- Xt # store X for group i
  Y[[i]] <- Yt # store y for group i
  G[[i]] <- Gt # store G for group i
}

#################################
########### Functions ###########
#################################


bdf <- function(){
# Generates a dataset allowing for IV estimations
# final dataset is [Y,X,GX,GY,G2X,group_number] if dummy=0
# final dataset is [Y,X,GX,GY,G2X,group_number,school_dummies] if dummy=1


  siz <- sum(n) # total number of individuals
  if (dummy==0){ # no school dummy
    bX <- matrix(1,siz,(2*kx+2)) # regressors are constant + X + GX + gy
    bZ <- matrix(1,siz,kx) # instruments are G^2X
    bY <- matrix(1,siz,1) # explained var is Y
    clust <- matrix(1,siz,1) # group number
    for (school in 1:m){ # for each group
      # position of the first individual in group "school"
      if (school==1){
        p1 <-1
      } else{
        p1 <- sum(n[1:(school-1)])+1
      }
      p2 <- p1+n[school]-1 # position of the last individual in group "school"
      Yt <- Y[[school]] # get Y for group "school"
      Xt <- X[[school]] # get X for group "school"
      Gt <- G[[school]] # get G for group "school"
      nt <- nrow(Xt) # get the number of students in group "school"
      if (fe==1){ # if group-level fixed effect
        Jt <- diag(nt) -matrix((1/nt),nt,nt)
        bX[p1:p2,] <- cbind(rep(0,nt),Jt%*%Xt,Jt%*%Gt%*%Xt,Jt%*%Gt%*%Yt) # explanatory variables 0 + X + GX + GY (in deviation with group average)
        bZ[p1:p2,] <- Jt%*%Gt%*%Gt%*%Xt # instruments (in deviation with group average)
        bY[p1:p2,1] <- Jt%*%(Yt) # endogenous variable (in deviation with group average)
      }
      else {
        bX[p1:p2,] <- cbind(rep(1,nt),Xt,Gt%*%Xt,Gt%*%Yt) # explanatory variables 0 + X + GX + GY 
        bZ[p1:p2,] <- Gt%*%Gt%*%Xt # instruments
        bY[p1:p2,1] <- (Yt) # endogenous variable
      }
      clust[p1:p2,1] <- school # group number
    }
    return(as.data.frame(cbind(bY,bX[,1:(2*kx+2)],bZ,clust))) # returns the dataset
  } else {
    bX <- matrix(0,siz,(2*kx+2+nschools)) # regressors are constant + X + GX + gy + school dummies
    bZ <- matrix(1,siz,kx) # instruments are G^2X (excluding constant and dummies)
    bY <- matrix(1,siz,1) # explained var is Y
    clust <- matrix(1,siz,1) # group number

    for (school in 1:m){ # for each group
      # position of the first individual in group "school"
      if (school==1){
        p1 <-1
      } else{
        p1 <- sum(n[1:(school-1)])+1
      }
      p2 <- p1+n[school]-1 # position of the last individual in group "school"
      whichdummy <- which(listschools==whichschool[which(whichschool[,2]==listsch[school]),1]) # get the school number of the school associated with group "school"
      Yt <- Y[[school]] # get Y for group "school"
      Xt <- X[[school]] # get X for group "school"
      Gt <- G[[school]] # get G for group "school"
      nt <- nrow(Xt) # get the number of students in group "school"
      bX[p1:p2,1:(2*kx+2)] <- cbind(rep(1,nt),Xt,Gt%*%Xt,Gt%*%Yt) # explanatory variables 1 + X + GX + GY
      bX[p1:p2,(2*kx+2+whichdummy)] <- 1 # put 1 in the corresponding school dummy
      bZ[p1:p2,] <- Gt%*%Gt%*%Xt # instruments
      bY[p1:p2,1] <- (Yt) # endogenous variable
      clust[p1:p2,1] <- school # group number
    }
    return(as.data.frame(cbind(bY,bX[,1:(2*kx+2)],bZ,clust,bX[,(2*kx+3):(2*kx+2+nschools)]))) # returns the dataset
  }
}


nlscon <- function(b){
## sub-function for the (concentrated NLS)
## computes the objective function for a specific value of beta

 out <- 0 # initialize objective
 Z <- vector("list", m) # initialize list
 for (i in 1:m){ # for each group
  nt <- n[i] # number of students in group i
  Z[[i]] <- solve(diag(nt)-b*G[[i]])%*%cbind(matrix(1,nt,1),X[[i]],G[[i]]%*%X[[i]]) # probability of y=1 in group i
 }
 LZ <- do.call(rbind,Z) # bind all probabilities
 LY <- do.call(rbind,Y) # bind all observed choices

 # computes the objective function
 int <- solve(t(LZ)%*%LZ)
 ext <- t(LZ)%*%LY
 out <- t(ext)%*%int%*%ext
 return(-out)
}


nls <- function(){
# NLS routine, returns the estimated parameters
  b <- optim(0.1,nlscon, method='L-BFGS-B',lower=-0.999,upper = 0.999)$par # optimize the concentrated NLS
  nt <- n[1] # size of the first group
  bZ <- solve(diag(nt)-b*G[[1]])%*%cbind(matrix(1,nt,1),X[[1]],G[[1]]%*%X[[1]]) # probability of y=1 for the first group
  bY <- Y[[1]] # y for the first group
  for (i in 2:m){ # for each groups>1
    nt <- n[i] # size of group i
    Z <- solve(diag(nt)-b*G[[i]])%*%cbind(matrix(1,nt,1),X[[i]],G[[i]]%*%X[[i]]) # probability of y=1 for group i
    bZ <- rbind(bZ,Z) # bind with the probabilities of the other groups
    bY <- rbind(bY,Y[[i]]) # bind with the y of the other groups
  }
  par <- solve(t(bZ)%*%bZ)%*%t(bZ)%*%bY # OLS (conditional on b)
  return(c(par,b)) # return the parameters
}

nlsSE <- function(para){
# computes the standard errors for the NLS
  siz <- sum(n) # total number of individuals
  grad <- matrix(0,siz,length(para)) # initialize
  res <- matrix(0,siz,1) # initialize residuals
  mpara <- matrix(para[1:(length(para)-1)],(length(para)-1),1) # parameter values as matrix
  for (i in 1:m){ # for each group i
    nt <- n[i] # size of group i
    #position of the first individual in group i
    if (i==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(i-1)])+1
    }
    p2 <- p1+n[i]-1 # position of the last individual in group i
    Minv <- solve(diag(nt)-para[length(para)]*G[[i]]) # computes the inverse (I-betaG)^(-1)
    bZ <- Minv%*%cbind(matrix(1,nt,1),X[[i]],G[[i]]%*%X[[i]]) # computes the probability of y=1
    grad[p1:p2,1:(length(para)-1)] <- bZ # computes for alpha,gamma,delta
    grad[p1:p2,length(para)] <- Minv%*%G[[i]]%*%bZ%*%mpara # computes for beta
    bY <- Y[[i]] # get y for group i
    res[p1:p2,1] <- (bY - bZ%*%mpara)^2 # computes the squared residuals
  }
  #computes the robust variance covariance matrix
  DpD <- solve(t(grad)%*%grad)
  grad2 <- t(grad)
  for (i in 1:siz){
    grad2[,i] <- grad2[,i]*res[i,1]
  }
  se <- DpD%*%grad2%*%grad%*%DpD
  return(se)
}

#################################
######### Execute Code ##########
#################################




dta <- bdf() # generates the dataset for 2SLS

if (dummy==1){ # 2SLS estimation if dummy=1
  # name the variables
  nmvar <- c("Y","Zero","Age","Age2","YearsSchool","Male","Black","Asian","Hisp","OtherR", "BothPar", "Sport", "Mlhs", "Mmhs", "Meduc_mis", "Mprof","Mother", "Mwelfare", "Mjobmiss",
             "GAge","GAge2","GYearsSchool","GMale","GBlack","GAsian","GHisp","GOtherR", "GBothPar", "GSport", "GMlhs", "GMmhs", "GMeduc_mis", "GMprof","GMother", "GMwelfare", "GMjobmiss",
             "GY", "ZAge","ZAge2","ZYearsSchool","ZMale","ZBlack","ZAsian","ZHisp","ZOtherR", "ZBothPar", "ZSport", "ZMlhs", "ZMmhs", "ZMeduc_mis", "ZMprof","ZMother", "ZMwelfare", "ZMjobmiss", "schlab")
  nmvar <- c(nmvar, paste("D", 1:nschools, sep="") )
  colnames(dta) <- nmvar
  # create a formula
  listvar1 <- colnames(dta)[3:(2*kx+3)] # X, GX, GY
  listvar2 <- colnames(dta)[(2*kx+4):(3*kx+3)] # instruments
  listvar3 <- colnames(dta)[(3*kx+5):length(colnames(dta))] # dummies
  formula <- paste(colnames(dta)[1], "~ 0 + ", paste(listvar1, collapse = " + "), "+", paste(listvar3, collapse = " + "), "| . -GY + ", paste(listvar2, collapse = " + "), sep = " ")
  formula <- as.formula(formula)
  
  # Estimation
  out <- ivreg(formula, data = dta)
  cluster.robust.se(out, dta$schlab)

} else { # 2SLS estimation if dummy=0
  # name the variables
  colnames(dta) <- c("Y","Zero","Age","Age2","YearsSchool","Male","Black","Asian","Hisp","OtherR", "BothPar", "Sport", "Mlhs", "Mmhs", "Meduc_mis", "Mprof","Mother", "Mwelfare", "Mjobmiss",
                     "GAge","GAge2","GYearsSchool","GMale","GBlack","GAsian","GHisp","GOtherR", "GBothPar", "GSport", "GMlhs", "GMmhs", "GMeduc_mis", "GMprof","GMother", "GMwelfare", "GMjobmiss",
                     "GY", "ZAge","ZAge2","ZYearsSchool","ZMale","ZBlack","ZAsian","ZHisp","ZOtherR", "ZBothPar", "ZSport", "ZMlhs", "ZMmhs", "ZMeduc_mis", "ZMprof","ZMother", "ZMwelfare", "ZMjobmiss", "schlab")
  # estimation
  if (fe==0){
    out <- ivreg(Y ~ Age + Age2 + YearsSchool + Male + Black + Asian + Hisp + OtherR + BothPar + Sport + Mlhs + Mmhs + Meduc_mis + Mprof + Mother + Mwelfare + Mjobmiss + GAge + GAge2 +
                 GYearsSchool + GMale + GBlack + GAsian + GHisp + GOtherR + GBothPar + GSport + GMlhs + GMmhs + GMeduc_mis + GMprof + GMother + GMwelfare + GMjobmiss + GY | . - GY +
                 ZAge + ZAge2 + ZYearsSchool + ZMale + ZBlack + ZAsian + ZHisp + ZOtherR + ZBothPar + ZSport + ZMlhs + ZMmhs + ZMeduc_mis + ZMprof + ZMother + ZMwelfare + ZMjobmiss, data = dta)
    cluster.robust.se(out, dta$schlab)
  } else {
    out <- ivreg(Y ~ 0 + Age + Age2 + YearsSchool + Male + Black + Asian + Hisp + OtherR + BothPar + Sport + Mlhs + Mmhs + Meduc_mis + Mprof + Mother + Mwelfare + Mjobmiss + GAge + GAge2 +
                 GYearsSchool + GMale + GBlack + GAsian + GHisp + GOtherR + GBothPar + GSport + GMlhs + GMmhs + GMeduc_mis + GMprof + GMother + GMwelfare + GMjobmiss + GY | . - GY +
                 ZAge + ZAge2 + ZYearsSchool + ZMale + ZBlack + ZAsian + ZHisp + ZOtherR + ZBothPar + ZSport + ZMlhs + ZMmhs + ZMeduc_mis + ZMprof + ZMother + ZMwelfare + ZMjobmiss, data = dta)
    cluster.robust.se(out, dta$schlab)
  }
}

fitval <- out$fitted.values
okfit <- as.numeric((fitval>0)&(fitval<1))


