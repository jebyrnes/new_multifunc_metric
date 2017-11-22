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




#' Get the effective number of a collection
#' of values as defined by Simpson's
#' Diversity following Jost 2006, 2010

eff_div <- function(df, q=NULL,  type=NULL){
  
  #error-handling#
  
  if(is.null(q) && is.null(type)) {
    stop("you must specify either the type of diversity or the order of diveristy that you want to calculate")}
  
  if(!is.null(type) && !is.null(q)) {
    stop("please specify either the order q or the type - not both")
  }
  
  if( !is.null(type) && is.na( pmatch( type, c( "Richness", "Shannon", "Simpson")))){
    stop("type must be one of Richness, Shannon or Simpson or an unambiguous substring thereof")
  }
  ####
  
  if(!is.null(type) && pmatch(type, c( "Richness", "Shannon", "Simpson")) == 1){
    q <- 0
  }
  
  if(!is.null(type) && pmatch(type, c( "Richness", "Shannon", "Simpson")) == 2){
    q <- 1
  }
  
  if(!is.null(type) && pmatch(type, c( "Richness", "Shannon", "Simpson")) == 3){
    q <- 2
  }
  
  
  #proportion of community
  p_df <- df/rowSums(df)
  
  if(q != 1){
    div = rowSums(p_df^q)^(1/(1-q))
  }
  
  if( q == 1){
    
    ln_p_df <- log(p_df, base = exp(1))
    ln_p_df[ln_p_df==-Inf] <- 0
    ln_p_df[ln_p_df==NaN] <- 0
    
    div <-exp(-1*rowSums(p_df*ln_p_df))
  }
  div[is.nan(div)] <- 0
  div
}

#' calculate Shannon eveness
shannon_ev <- function(df){
  eff_div(df, q=1) / eff_div(df, q = 0)
}

#' Get the evenness factor
#' of a collection of values
#' as defined in Jost 2010
even_fact <- function(df, type="Simpson"){
  eff_div(df,type = type)/ncol(df)
}

