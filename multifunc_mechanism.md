Multifunctionality-mechanism
================
Robert Bagchi
24 November, 2020

  - [Rationale](#rationale)

# Rationale

We can think of the relationship between species composition and
function provision using the following relationship

  
![&#10;\\vec{F} = \\vec{N} \\cdot \\bf{F}
&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cvec%7BN%7D%20%5Ccdot%20%5Cbf%7BF%7D%20%0A
"
\\vec{F} =  \\vec{N} \\cdot \\bf{F} 
")  
Where the rwo vector
![\\vec{F}](https://latex.codecogs.com/png.latex?%5Cvec%7BF%7D
"\\vec{F}") measures the provision of each of
![m](https://latex.codecogs.com/png.latex?m "m") functions and
![\\bf{F}](https://latex.codecogs.com/png.latex?%5Cbf%7BF%7D "\\bf{F}")
is a ![n \\times
m](https://latex.codecogs.com/png.latex?n%20%5Ctimes%20m "n \\times m")
matrix where each row gives each species’ *per capita* contribution to
the ecosystem functions.
![\\vec{N}](https://latex.codecogs.com/png.latex?%5Cvec%7BN%7D
"\\vec{N}") is a row vector of abundances of each species.

The *multifunctionality* part of the model can be examined by expressing
each row, ![i](https://latex.codecogs.com/png.latex?i "i") of
![\\bf{F}](https://latex.codecogs.com/png.latex?%5Cbf%7BF%7D "\\bf{F}")
as

  
![&#10;\\begin{array}{lll}&#10;\\vec{F}\_{i.} \\sim MVN(\\mu, \\Sigma)
\\\\&#10;\\Rightarrow \\bf{F}\_{i.} = \\mu + \\vec{z} \\cdot (\\sigma
\\cdot \\Omega^{0.5}\\cdot \\sigma^t),\~ \\vec{z} \\sim
\\mathcal{N}(0,1)&#10;\\end{array}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Barray%7D%7Blll%7D%0A%5Cvec%7BF%7D_%7Bi.%7D%20%5Csim%20MVN%28%5Cmu%2C%20%5CSigma%29%20%5C%5C%0A%5CRightarrow%20%5Cbf%7BF%7D_%7Bi.%7D%20%3D%20%20%5Cmu%20%2B%20%5Cvec%7Bz%7D%20%5Ccdot%20%28%5Csigma%20%5Ccdot%20%5COmega%5E%7B0.5%7D%5Ccdot%20%5Csigma%5Et%29%2C~%20%5Cvec%7Bz%7D%20%5Csim%20%5Cmathcal%7BN%7D%280%2C1%29%0A%5Cend%7Barray%7D%0A
"
\\begin{array}{lll}
\\vec{F}_{i.} \\sim MVN(\\mu, \\Sigma) \\\\
\\Rightarrow \\bf{F}_{i.} =  \\mu + \\vec{z} \\cdot (\\sigma \\cdot \\Omega^{0.5}\\cdot \\sigma^t),~ \\vec{z} \\sim \\mathcal{N}(0,1)
\\end{array}
")  
where ![\\Omega](https://latex.codecogs.com/png.latex?%5COmega
"\\Omega") is the correlation matrix,
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") is the mean
of each function,
![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\\sigma") is
the standard deviation of each function.
![\\vec{z}](https://latex.codecogs.com/png.latex?%5Cvec%7Bz%7D
"\\vec{z}") is a row vector of length
![m](https://latex.codecogs.com/png.latex?m "m").

For example, if we assume a unit normal distribution for each function
and assume z is an ![n \\times
m](https://latex.codecogs.com/png.latex?n%20%5Ctimes%20m "n \\times m")
matrix

  
![&#10;\\vec{F} = \\vec{N} \\cdot \\bf{z} \\cdot
\\Omega^{0.5}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cvec%7BN%7D%20%5Ccdot%20%5Cbf%7Bz%7D%20%5Ccdot%20%5COmega%5E%7B0.5%7D%0A
"
\\vec{F} =  \\vec{N} \\cdot \\bf{z} \\cdot \\Omega^{0.5}
")  

We can see how each function depends on the correlation matrix. How that
might translate into a directional effect however, I don’t yet
understand.

``` r
n <- 10; m <- 5
Omega <- rethinking::rlkjcorr(1, K =m, eta= 2) ## smaller eta is more correlation among species

z <- matrix(rnorm(n*m), nr=n)

Sigma <-  z %*% diag(m) %*% chol(Omega) %*% t(diag(m))

F_k <- rlnorm(n) %*% Sigma
F_k
```

    ##           [,1]     [,2]     [,3]      [,4]      [,5]
    ## [1,] -3.715685 -8.94745 9.481541 0.4823969 -6.777561

If we want, it is easy to express this for several communities, perhaps
with varying diversity (say 1 - 10 species).

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
sp <- matrix(rlnorm(100), nr=10)
sp[upper.tri(sp)] <- 0
F_k <- sp %*% Sigma

funcs <- data.frame(S= 1:10, f = F_k %*% rep(1/m, m))

ggplot(funcs, aes(x=S, y =f)) + geom_point() + geom_smooth(method="lm") +
  labs(x= "Species richness", y = "mean functioning") + theme_bw()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](multifunc_mechanism_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We can the do this for different values of eta.

``` r
genSpFunc <- function(n, m, eta)
{
  Omega <- rethinking::rlkjcorr(1, K =m, eta= eta) ## smaller eta is more correlation among species
  z <- matrix(rnorm(n*m), nr=n)
  Sigma <-  z %*% diag(m) %*% chol(Omega) %*% t(diag(m))
  return(Sigma)
}
eta <- seq(0.5, 10, len = 5)
dat <- data.frame(eta = rep(eta, each = n), S = rep(1:n, length(eta)),  meanfunc =do.call("c", lapply(eta, function(eta, spA )
  spA %*% genSpFunc(n, m, eta) %*% rep(1/m, m), spA = sp)))
  
ggplot(dat, aes(x = S, y = meanfunc, colour = eta, group = eta)) + geom_point() + 
  geom_smooth(method = "lm") + theme_bw() + scale_colour_viridis_c() + facet_wrap(~eta)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](multifunc_mechanism_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

It is perhaps also worth folding in the work I did in last year on
competition, which would influence
![\\vec{N}](https://latex.codecogs.com/png.latex?%5Cvec%7BN%7D
"\\vec{N}"). We could work out the composition of the community at
equilibrium and use that in the equations.

Using Lotka-Volterra theory we can estimate equilibrium abundances of
![n](https://latex.codecogs.com/png.latex?n "n") species given a
competition matrix
![\\mathbf{A}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BA%7D
"\\mathbf{A}") as,

  
![&#10;N^\* = \\vec{1} \\cdot
\\mathbf{A}^{-1}&#10;](https://latex.codecogs.com/png.latex?%0AN%5E%2A%20%3D%20%5Cvec%7B1%7D%20%5Ccdot%20%5Cmathbf%7BA%7D%5E%7B-1%7D%0A
"
N^* = \\vec{1} \\cdot \\mathbf{A}^{-1}
")  
where ![\\vec{1}](https://latex.codecogs.com/png.latex?%5Cvec%7B1%7D
"\\vec{1}") is a row of 1s with length
![n](https://latex.codecogs.com/png.latex?n "n").

This means that we can modify the equation for ecosystem function from
above by substituting
![\\vec{N}](https://latex.codecogs.com/png.latex?%5Cvec%7BN%7D
"\\vec{N}") with the equation for
![N^\*](https://latex.codecogs.com/png.latex?N%5E%2A "N^*")

  
![&#10;\\vec{F} = \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot \\bf{F}
&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cvec%7B1%7D%20%5Ccdot%20%5Cmathbf%7BA%7D%5E%7B-1%7D%20%5Ccdot%20%5Cbf%7BF%7D%20%0A
"
\\vec{F} =  \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot \\bf{F} 
")  

And, if ![\\bf{z} \\sim
\\mathcal{N}(0, 1)](https://latex.codecogs.com/png.latex?%5Cbf%7Bz%7D%20%5Csim%20%5Cmathcal%7BN%7D%280%2C%201%29
"\\bf{z} \\sim \\mathcal{N}(0, 1)") is an n x m matrix

  
![&#10;\\vec{F} = \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot ( \\mu +
\\bf{z} \\cdot (\\sigma \\cdot \\Omega^{0.5}\\cdot
\\sigma^t))&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cvec%7B1%7D%20%5Ccdot%20%5Cmathbf%7BA%7D%5E%7B-1%7D%20%5Ccdot%20%28%20%5Cmu%20%2B%20%5Cbf%7Bz%7D%20%5Ccdot%20%28%5Csigma%20%5Ccdot%20%5COmega%5E%7B0.5%7D%5Ccdot%20%5Csigma%5Et%29%29%0A
"
\\vec{F} =  \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot ( \\mu + \\bf{z} \\cdot (\\sigma \\cdot \\Omega^{0.5}\\cdot \\sigma^t))
")  

we have a general formula for the provision of multiple ecosystem
functions by multiple competing species.

Because we probably lose little from assuming functions arise from a
unit normal, we could get rid of
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") and
![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\\sigma") to
get a simpler equation

  
![&#10;\\vec{F} = \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot \\bf{z} \\cdot
\\Omega^{0.5}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cvec%7B1%7D%20%5Ccdot%20%5Cmathbf%7BA%7D%5E%7B-1%7D%20%5Ccdot%20%20%5Cbf%7Bz%7D%20%5Ccdot%20%5COmega%5E%7B0.5%7D%0A
"
\\vec{F} =  \\vec{1} \\cdot \\mathbf{A}^{-1} \\cdot  \\bf{z} \\cdot \\Omega^{0.5}
")  

``` r
## S = no. species, a_ii is intra-specific comp,, a_ij is interspecific comp
makeCmat  <- function(S, a_ii, a_ij)
{
  Alpha <-   matrix(a_ij, nr=S, nc=S) 
  diag(Alpha) <- a_ii
  return(Alpha)
}
Amat <- makeCmat(n, 0.1, 0.01)

rep(1, n) %*%  solve(Amat) %*% (z)  %*% Omega
```

    ##          [,1]     [,2]     [,3]      [,4]      [,5]
    ## [1,] 19.33384 13.16585 34.41573 -25.87746 -34.17016

One thing that emerges from this is that the
diversity-multifunctionality relationship is determined by two things,
the inverse of the competition matrix and the correlations among species
contributions to functions. Indeed, I think we could probably get a
pretty good idea of the expected relationships by just looking at how
those two matrices interact, but I’m not quite sure how to explore that
yet.
