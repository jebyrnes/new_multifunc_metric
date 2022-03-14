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


shannon_s <- function(...){
  f <- c(...)
  p <- f/sum(f)
  p <- p[p>0]
  exp(-sum(p*log(p)))
}

gmean <- function(x, p=1) (length(x)^-1 * sum(x^p)) ^ (1/p)

f <- function(...){
  x <- c(...)
  as <- length(x)*mean(x)
  x <- x[x>0]
  #  exp(-1*sum(x/as*log(x/as)))
  #  exp(-1/as*sum(x*log(x/as)))
  # exp(-1/as*sum(x*log(x)-x*log(as)))
  #  exp(-1/as*sum(x*log(x))+ 1/as*sum(x*log(as)))
  #exp(-1/as*sum(x*log(x))+ log(as)/as*sum(x))
  ##exp(-1/as*sum(x*log(x))) * exp( log(as)/as*sum(x))
  ##exp(-1/as*sum(x*log(x))) * as^(sum(x)/as)
  g <- floor(as)
  #  ret <- exp(-1/as*(g*log(1/as)+(as-g)*log((as-g)/as)))
  #  if(is.nan(ret)) ret <- exp(-1/as*(g*log(1/as)))
  #  ret
  #exp(g*log(as)/as + log(as)/as*(as-g) - (as-g)/as*log(as-g))
  #simplified from http://www.wolframalpha.com/input/?i=simplify+exp(g*log(x)%2Fx+%2B+log(x)%2Fx*(x-g)+-+1%2Fx*(x-g)*log(x-g))
  ret <- -1*as*(as-g)^(g/as)/(g-as) #
  if(is.nan(ret)) ret <- as^(g/as) #exp(g*log(as)/as)
  ret
}

get_mfn_floor <- function(a, s, q){
  if(q==1){
    as <- a*s
    g <- floor(as)
    ret <- -1*as*(as-g)^(g/as)/(g-as)
    ix <- is.nan(ret)
    ret[ix] <- as[ix]^(g[ix]/as[ix])#exp(-1/as[ix]*(g[ix]*log(1/as[ix])))
    return(ret)
  }
  (a^-q*s^-q*(floor(a*s) + (a*s-floor(a*s))^q))^(1/(1-q))
}





