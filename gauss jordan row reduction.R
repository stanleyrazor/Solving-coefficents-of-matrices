gj_rowred <- function(amat, avec)
{
  full <- cbind(amat, avec)
  
  # ensuring that at first all starting indexes are 1
  for (i in 1:nrow(full))
  {
    if (full[i, 1] != 0)
    {
      full[i,] <- full[i,] * 1/full[i,1]
    }
  }
  print(full)
  
  # row reduction by subtraction and dividing
  for (i in 1:nrow(full))
  {
    if ((i+1) <= nrow(full))
    {
      for (j in (i+1):nrow(full))
      {
        full[j, ] <- full[(i),] - full[j, ]
        
        if (full[j, j] != 1)
        {
          full[j, ] <- full[j, ] * 1/full[j ,j]
        }
      }
    }
    else
    {
      break
    }
  }
  
  print(full)
  #return(full)
}
