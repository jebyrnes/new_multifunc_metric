# Exploring the relationship between E and A
Robert Bagchi  
`r format(Sys.time(), '%d %B, %Y')`  

## 1. Definitions

The data inputted into the equations are:

*  $S$   = number of functions
*  $F_i$ = observed provision of each function, $i$, standardised by dividing by the observed (or theoretical) maximum of $i$.

### 2. Calculated metrics

The terms above are then used to estimate a number of metrics, including:

1. The average level of theset of ecosystem functions considered. 
$$ 
A = \frac{1}{S}\cdot{\sum_{i=1}^S {F_i}} 
$$
2. The proportion of the total functioning contributed 
$$
p_i = \frac{F_i}{\sum_{i=1}^S{F_i}}
$$
3. The effective number of functions, $^qN$
$$
^{q}N = \left(\sum_{i=1}^{S}p_{i}^q \right)^{1/(1-q)}
$$
which can be converted into 

4.  the evenness of functioning, $^qE$

$$
^qE = {^qN}/S
$$
## 3. Examining the relationships

The first thing we need to do is to incorporate $A$ into the equations for $^qN$ and $^qE$. To do so, we first note that $p_i$ can be related to $A$ because:

$$
p_i = \frac{F_i}{A \cdot S}
$$

This allows us to replace $p_i$ with a function of $A$ in subsequent equations. Thus, we get,

$$
^{q}N = \left[\sum_{i=1}^{S}\left({\frac{F_{i}}{A \cdot S}}\right)^q \right]^{1/(1-q)}
$$
Which can be simplified by removing the constants $A$ and $S$ out of the equation, as 
$$
^{q}N = \left[{\frac{S}{A^q \cdot S^{q}}} \cdot \sum_{i=1}^{S}F_{i}^q \right]^{1/(1-q)}
$$
Because both $q$ and $F_i$ are always positive, we can express then $^qE$ as

$$
\begin{aligned}  
^{q}E &= \frac{1}{S}\left[{\frac{1}{A^q \cdot S^{q}}} \sum_{i=1}^{S}F_{i}^q \right]^{1/(1-q)} \\
&\Rightarrow \frac{1}{S}\frac{(A \cdot S)^{1-q}}{A \cdot S}  \sum_{i=1}^{S}F_{i}^q ^{1/(1-q)} \\
&\Rightarrow \frac{A \cdot S}{S}\left[\frac{1}{A \cdot S} \cdot \sum_{i=1}^{S}F_{i}^q \right]^{1/(1-q)} \\
&\Rightarrow A \left[\frac{\cdot \sum_{i=1}^{S}F_{i}^q}{A \cdot S} \right]^{1/(1-q)}
\end{aligned}
$$
From these formula, the relationship between $^qE$ and $A$ can be derived for specific values of $q$ as 

* q = 0 (number of functions with non-zero provision)

$$
\begin{aligned}
^{0}E = \cdot \sum_{i=1}^{S}F_{i}^0 = S
\end{aligned}
$$
* q = 2 (equivalent to Simpson's index)

$$
^{2}E = \frac{A^{2}}{\sum_{i=1}^{S}F_{i}^2}
$$
* q = 1 (equivalent to Shannon's entropy)
The $q = 1$ case is a little trickier because of the need to use limits. Rather than use limits themselves here, we start off with the equation for the effective number of species for $q = 1$, $^1S = \exp(H) = \exp(-\sum_{i=1}^{S}{p_i \cdot \log{p_i}})$. We can then derive $^1E$ as:

$$
^1E = \frac{1}{S}\exp{\left[\sum_{i=1}^S{\frac{F_i}{A\cdot S}\cdot \log{\frac{F_i}{A \cdot S}}}\right]}
$$
