#'################################'#
#'
#' Functions used for the multifunc_new.Rmd 
#'
#'
#'################################'#


#' Function to make data of all combinations
#' of different levels of function, ranging from 
#' 0 to 1 with a user defined step size
#' 
make_data <- function(n=3,  minVal = 0.01, ...){
  
  #dependencies 
  require(dplyr)
  require(tibble) # not sure if needed?
  
  #avoid memory overflow
  if( length( seq( minVal, 1, ...))^n > 1e7){
    stop("too many combinations (> 1e7) - reduce n or step_size/length.out or both")}
  
  amat <- as.tibble( matrix( rep( seq( minVal, 1, ...), n), ncol = n))
  amat %>% expand.grid %>% as.tibble
}



