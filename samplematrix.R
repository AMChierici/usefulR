
samplematrix <- function(M, n, replace, prob, seed=NULL, showprob=TRUE){
  #This function takes a matrix as an input and it samples a position in the matrix.
  #The output is a table with 3 columns: row index, column index and the probability wheight used.
  #To suppress the probability wheight put showprob=FALSE.
  #All other arguments are the same as the sample function from the base package.
  require(data.table)
  set.seed(seed)
  samplepos <- sample(x=length(M), size=n, replace=replace, prob=prob)
  norows <- nrow(M) #no. of rows
  posid <- samplepos%%norows #position identifier
  rowids <- 1:norows #row indeces
  colids <- 1:ncol(M) #columnd indeces
  if(showprob){
    res <- data.table(
      rowindex = ifelse(posid==0, rowids[norows], rowids[posid]),
      colindex = ifelse(posid==0, colids[samplepos/norows], colids[ceiling(samplepos/norows)]),
      prob = M[samplepos]
    )
  } else{
    res <- data.table(
      rowindex = ifelse(posid==0, rowids[norows], rowids[posid]),
      colindex = ifelse(posid==0, colids[samplepos/norows], colids[ceiling(samplepos/norows)]),
    )
  }
  res
}

# Try it out
x <- matrix(c(1:10), 2, 5)
samplematrix(x, 10, TRUE, x, 10)
