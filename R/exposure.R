#' Exposure Calculation for Clustered Interference
#'
#' This function constructs an exposure vector based on the assignment vector z
#' and the number of clusters K.
#'
#' @param z A numeric vector representing treatment assignments.
#' @param K An integer specifying the number of clusters.
#'
#' @return A numeric vector representing the exposure of each unit.
#' @export
exposure <- function(z,K){
  N <- length(z)
  z <- matrix(z,ncol=K)
  f <- matrix(rep(0,N),ncol=K)
  index1 <- which(apply(z,2,sum)==1)
  f[,index1] <- 1
  f <- f + z
  return(c(f))
}
