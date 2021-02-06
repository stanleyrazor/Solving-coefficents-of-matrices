## computation of cramers rule:

cramers <- function(amat, avec)
{
  # cheking for compatibility
  stopifnot(nrow(amat) == length(avec))
  
  ## checking for non singularity
  stopifnot(det(a) != 0)
  
  # a list to hold the replacement matrices
  matlist <- list()
  detvec <- vector()
  
  for (i in 1:ncol(amat))
  {
    temp <- amat
    temp[,i] <- avec
    matlist[[i]] <- temp
    
    detvec[i] <- det(temp)
  }
  #print(matlist)
  
  maindet <- det(amat)
  return(detvec/maindet)
}

a <- matrix(c(2,1,-1,1,1,1,1,-2,-3), nrow=3, byrow=T)
b <- c(3,1,4)

cramers(a,b)

aa <- matrix(c(1,1,2,2,4,-3,3,6,-5), nrow=3, byrow=T)
bb <- c(9,1,0)

cramers(aa,bb)

