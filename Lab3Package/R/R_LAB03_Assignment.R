#' @param a,b as input parameters.
#' @export



euclidean <- function(a,b){
i <- max(a,b)
j <- min(a,b)
while(j > 0){
  i <- j * floor((max(i,j) / min(i,j))) + max(i,j) %% min(i,j)
  r <- max(i,j) %% min(i,j)
  if(r == 0){
    print(min(i,j))
    break
  }
  i <- j
  j <- r
  
}
}




