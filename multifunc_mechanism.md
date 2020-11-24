Multifunctionality-mechanism
================
Robert Bagchi
24 November, 2020

  - [Rationale](#rationale)

# Rationale

We can think of the relationship between species composition and
function provision using the following relationship

  
![&#10;\\vec{F} = \\bf{F} \\cdot
\\vec{N}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cvec%7BF%7D%20%3D%20%20%5Cbf%7BF%7D%20%5Ccdot%20%5Cvec%7BN%7D%0A
"
\\vec{F} =  \\bf{F} \\cdot \\vec{N}
")  
Where the vector
![\\vec{F}](https://latex.codecogs.com/png.latex?%5Cvec%7BF%7D
"\\vec{F}") measures the provision of each of
![m](https://latex.codecogs.com/png.latex?m "m") functions and
![\\bf{F}](https://latex.codecogs.com/png.latex?%5Cbf%7BF%7D "\\bf{F}")
is a ![m \\times
n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \\times n")
matrix where each column gives each species’ contribution to each
ecosystem function.

The *multifunctionality* part of the model can be examined by expressing
![\\bf{F}](https://latex.codecogs.com/png.latex?%5Cbf%7BF%7D "\\bf{F}")
as

  
![&#10;\\begin{array}{ll}&#10;\\bf{F} \\sim MVN(\\mu, \\Sigma)
\\\\&#10;\\Rightarrow F \\sim \\mu + \\sigma \\cdot \\Omega\\cdot
\\sigma^t \\cdot z,\~ z \\sim
\\mathcal{N}(0,1)&#10;\\end{array}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Barray%7D%7Bll%7D%0A%5Cbf%7BF%7D%20%5Csim%20MVN%28%5Cmu%2C%20%5CSigma%29%20%5C%5C%0A%5CRightarrow%20F%20%5Csim%20%5Cmu%20%2B%20%5Csigma%20%5Ccdot%20%5COmega%5Ccdot%20%5Csigma%5Et%20%5Ccdot%20z%2C~%20z%20%5Csim%20%5Cmathcal%7BN%7D%280%2C1%29%0A%5Cend%7Barray%7D%0A
"
\\begin{array}{ll}
\\bf{F} \\sim MVN(\\mu, \\Sigma) \\\\
\\Rightarrow F \\sim \\mu + \\sigma \\cdot \\Omega\\cdot \\sigma^t \\cdot z,~ z \\sim \\mathcal{N}(0,1)
\\end{array}
")
