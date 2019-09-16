#' Find the GCD of two given integers using Euclidean Algorithm.
#' @param a,b Two input parameters to find GCD.
#' @examples 
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' @return Returns the GCD of given two inputs and the output is an integer.
#' @export



euclidean <- function(a,b){
  i <- max(a,b)
  j <- min(a,b)
  while(j != 0){
    
    r <- max(i,j) %% min(i,j)
    if(r == 0 ){
      return(abs(min(i,j)))
      break
    }
    i <- j
    j <- r
    
  }
}



