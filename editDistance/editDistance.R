#construct minimum function for editDistance
minimum <- function(a,b,c)
{
  mi <- a
  if(b < mi){
    mi <- b
  }
  if(c < mi){
    mi <- c
  }
  return(mi)
}

#editDistance function
editDist <- function(x,y)
{
  # construct char arrays from two strings
  # x is the row string, y is the col string
  
  xsplit <- strsplit(x, "")[[1]]
  ysplit <- strsplit(y, "")[[1]]
  
  #preprocess char arrays 
  xsplit <- tolower(xsplit)
  ysplit <- tolower(ysplit)
  
  #contruct an empty matrix 
  #the number of rows  = to x + 1 and cols y+1
  nrows <- length(xsplit) + 1
  ncols <-  length(ysplit) +1
  
  editMatrix <- matrix(0, nrow = nrows, ncol = ncols)
  
  #initialize first col and first row of editMatrix
  for (i in 1:nrows) {
    editMatrix[i, 1] <- i -1
  }
  for (j in 1:ncols) {
    editMatrix[1, j] <- j -1
  }
  #tabulate the cost to transform char in string to target char in string
  for( i in 2:nrows)
    {
    
    x_i <- xsplit[i-1]
    
    for(j in 2:ncols) {
      y_j <- ysplit[j-1]
      
      if(x_i == y_j) {
        cost <- 0
      }
      
      else {
        cost <- 1
      }
      a <- editMatrix[i-1, j] + 1
      b <- editMatrix[i, j-1] +1
      c <- editMatrix[i-1, j-1] + cost
      
      editMatrix[i,j] <- minimum(a,b,c)
    }

  }
  #edit distance will be the [i,j] value in edit matrix 
  distance <- editMatrix[nrows, ncols]
  print(editMatrix)
  return(distance)
}