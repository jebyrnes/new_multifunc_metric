A New Multifunc
================

#### Introduction

The past decade has witnessed the slow growth of the concept of ecosystem **multifunctionality**. Multifunctionality is defined as a measure of the simultaneous performance of multiple functions. Note that we do not say ‘ecosystem functions’, as multifunctionality is a broader concept that can be easily applied outside of community and ecosystem ecology - and even even outside of ecology all together. This concept has proven unifying in a number of different situations. In the Biodiversity Ecosystem Function literature, it has enabled researchers to discover new mechanisms by which diversity is altering the function of ecosystems. Researchers are beginning to recognize that foundation species create multifunctional landscapes (Angelini et al. DATE) that differ not only in species diversity and abundance, but ecosystem-level properties. Invasion biology has begun to use multifunctionality to quantify whole-system level changes (Ramus et al. PNAS). Multifunctionality has tremendous application as well, as managers often attempt to work towards the provision of multiple ecosystem services (REFS) - fundamentally the same concept as multifunctionality - or see single ecosystem services as the product of multiple different ecosystem functions (Frontiers REF).

However, adoption of multifunctionality as a fundamental concept linking community and ecosystem ecology has been slow. Nor is there a robust theoretical core behind multifunctionality, further hindering its penetration into scientific thinking. We posit that there is at least one simple explanation behind this lack of progress. While some researchers and managers have latched on to the concept of multifunctionality, the field still lacks a clear single metric that it can use to summarize the phenomenon. Frankly, current metrics range from the simple yet misstated to the complex and acruate yet unweildy. We acknowledge some of our own blame in this state of affairs (Byrnes et al. 2014); although we do note that throwing even some of the finest minds against this problem in a stimulating working group format over multiple years (we are thankful for NCEAS) has likely resulted in more ink impregnated on foreheads hitting whiteboards than should be typical. We are similarly thankful that many in the field have continued to be critical and force us to look at our current failures with honesty (Fabian's paper).

Here we briefly review the current state of affairs, propose a solution, and discuss some of the ongoing challenges to the field, including whole conceptual fields of exploration that we feel have been entirely missed.

#### The current state of multficuntional affairs

The definition of multifunctionality presents a challenge. How to we define a metric that captures both the level of performance of a broad suite of functions as well as the distribution of differences in performance between functions? Researchers have sought to capture this question in four ways, after standardizing functions to similar levels in order to prevent apples-to-oranges comparisons.

First, on the side of simplicty, taking the average of all functions. This approach appears on its face appealing, particularly as it provides a metric that can be put on a y-axis while a predictor is on the x. However, it sacrifices crucial information about the system. An arithmetic mean tells us, were we to sample any one function, what level of functioning would we expect? Consider two plots - one where all functions are similar and performing at half their value and one where half of the functions are at their maximum while half are absent. The averaging approach says that they are identical. Geometric averaging appears to get around this problem to some degree, as a geometric mean is approximately the arithmetic mean minus the variance of observations. However, given its formulation, one critically low function can bias the measurement, nor do we always have a sufficient sample of functions to achieve that approximation.

Second, we have metrics of the Multivariate Diversity Interactions framework (Dooley et al. 2015). This elegant framework allows us to tease apart the importance of correlations between functions and the contribution of different drivers to simultaneous change in those functions. It does not, however, provide a holistic metric of multifunctionality *per se*, much like the overlap approach before it. While we gain rich information about a system, we do not gain holistic interpretability.

Last, we have the multiple threshold approach. This approach seeks to balance the goals of measuring the simultaneous performance of multiple ecosystem functions with the arbitrariness of choosing a threshold of relevance for those functions. We note that many who use it simply choose a single threshold, although present many in their appendicies (e.g., SOMEONE). This is a reasonable choice in order to understand a single set of observations. Further, for those who do look at all thresholds, the results are, frankly, difficult to interpret as the quantities they yeild are non-obvious in their links to the concept of multifunctionality. The approach yields rich information about multifunctionality *sensu stricto*, but in so doing, becomes unweildy for most if not all who chose to use it.

How can we solve this? What is a good metric of multifunctionality that yields the information we need in a proper holistic fashion such that we might even ultimately link it back to theory?

#### A Way Forward

The definition of multifunctionality presents a challenge. How to we define a metric that captures both the level of performance of a broad suite of functions as well as the distribution of differences in performance between functions? This problem seems akin to the problem of capturing diversity of species within a community. We know the ‘richness’ of functions - it is the total number of functions we are observing. This richness can be partitioned independently into evenness and a measure of compositional complexity (Jost 2010). In the diversity literature, this metric of complexity can be simply translated into an effective number of species, i.e., the number of equally abundant species that would produce the same metric (Jost 2006, 2010), via Hill Numbers. This concept of effective number of functions is equally powerful for the multifunctionality world. For some set of functions that have been standardized to a common scale (i.e., between 0 and 1) such that *F*<sub>1, 2, 3...*S*</sub> is their level of function

$$p\_i = \\frac{F\_i}{\\sum F\_i}$$

$$^{q}N = (\\sum\_{i=1}^{S}p\_{i}^q)^{1/(1-q)}$$

where *N*<sub>*q*</sub> is the effective number of species for some order q. Adapting from Jost (2006), if q = 0, this is the total number of functions, S. If q&lt;1, then functions at low level dominate the calculation. For q = 1, the approximation of this function is equivalent to results from Shannon diversity for species. For q&gt;1, higher functions are given greater weight. At q = 2, we get results that are equivalent to the equivalent number of functions calculated from Simpson's diversity. If one of our goals is to up-weight high performing functions, q=2 is a reasonable choice. However, to be conservative, q=1 is sufficient.

At this point, we can calculate evenness as this effective number of functions divided by the number of functions divided we are observing (Jost 2010).

$$E = \\frac{^{q}N}{S}$$

Armed with functional evenness, effective number of functions, and average level of function, here defined as *A*, we can turn to creating a meaningful multifunctionality metric based on evenness.

*M*<sub>*e*</sub> = *E**A*

This metric can also be rescaled back to units of number of functions by multiplying it by total number of functions providing an metric on the scale of the absolute number of functions, such that *M*<sub>*f*</sub> = *M*<sub>*e*</sub>*S*.

Why must we consider level of function and evenness together in one metric? From a convenience standpoint, a combined metric satisfies our definition of multfunctionality. More importantly, both metrics are non-independent (Figure 1A). Consider, for example, that it is not possible to have a patch where all functions are at their maximum, but is anything less than completely even. Similarly, a patch whose functions are maximally ‘even’ - or all doing something different from one another - cannot by definition have the highest levels of average function. Indeed, if functional evenness is &lt; 1, then by definition the average is bound to be less that 1. The lowest evenness possible is 1/S. We can visualize this in phase space as well (Figure 1b).

**yeah, the linear case below works for q=2, but I need to see if there's a more general expression here. consider this a starting point**

![](multifunc_new_files/figure-markdown_github/show_conceptual-1.png)

Further, having this single metric now allows us to begin to examine it as any other response variable. In the BEF world, we might look at additive partioning in addition to complementary overlap approaches. In global change biology, we can look at stability, resistance, and resilience. The options are open.

#### Application to real data

To see how this metric can be used, consider the example of Duffy et al. 2003. In this experiment, Duffy and colleagues sought to examine how biodiversity of grazers influences multiple different ecosystem functions in seagrass ecosystems. Using the functions discussed in the paper, we standardized and reflected them as per how Duffy et al. discuss their results. Comparing M<sub>e</sub>, average functional performance, and functional evenness (Figure 2).

![](multifunc_new_files/figure-markdown_github/duffy-1.png)

What is intriguing about this is that across the experiment, functional evenness remained high. However, when combined with average function, the slope appears slightly steeper than either of the two relationships - diversity's importance might well be gerater. Part of this might be driven by the positive correlation between the two variables given their functional constraint (Figure 3).

![](multifunc_new_files/figure-markdown_github/duffy_trdeoff-1.png)

Regardless, our results broadly reproduce the qualitative conclusions of Duffy et al. (2003) but with additional clarity, as they do for biodepth as well (SUPPLEMENT).

#### Robutness of metric

FABIAN DO MAGIC HERE
====================

#### A note on correlated functions

A great deal of the confusion in the multifunctionality literature even when using metrics has sprung up from the issue of correlation between functions (BACK AND FORTH IN PNAS). In no small part this is likely due to the incessant focus on accounting for correlation in areas such as spatial and temporal analysis. To have correlations between functions seems to somehow either "contaminate" results and make them suspect or, for others, only correlated functions are valid to indicate multifunctionality (PNAS ref). Neither of these are strictly true.

The difficulty comes down to defining just what a 'function' is. This semantic argument has led down the path of considering "true" functions as the single response to a driver that gives rise to all other responses. Consider the example of aboveground and belowground production in plants. The two are driven by similar, if not mutually overlapping, biological processes. Thus, are they truly separate functions? If biological reductionism in functional space is the goal of a researcher, then dimensional reduction techniques before analysis would seem to be in order. We offer a caution, however, that techniques such as PCA reduce dimensions with absolutely no basis in biology. Rather, Confirmatory Factor Analysis (ref) with a meaningful underlying factor structure might provide a far more useful and biologically meaningful option.

On the other hand, when we consider an ecosystem function as a measurement of some flux within an ecosystem, the issue of correlation becomes moot. Was there a flux, or was there not a flux? Was that flux uniquely relevant to some other ecosystem processes? Root microbes care not for larger leaves. Similarly, managers have goals that focus on functions and services that are independent of any biological reductionism. Hence, sweeping this correlation under the rug for a single metric of multifunction is not only acceptable, but desirable in terms of defining a clear metric of the concept. If researchers are interested in exploring the correlation structure of functions - or even how that structure changes across treatments (something to our knowledge that has not been done) - then that is a separate piece of a good ecological story. It does not, however, invalidate any metric of multifunctionality seeking to capture simultaneous change in multiple functions. It comes down to a researchers question and how that question forces them to define functions. This is an epistomological point beyond the scope of any metric.

#### Thinking Beyond Measured Functions

One potential pitfall of this technique is to assume that it is robust to a researchers choice in what functions to measure in terms of the inferences it can deliver about a system. In an ideal world, when we quantify multifunctionality we would think of all possible functions as a population. We would then measure a random (or stratified!) sample of functions in order to draw good inferences about how a driver influences system-wide multifunctionality. To our knowledge, this type of careful thought about relevant functions from a population sampling perspective has never been applied in multifunctionality research. We hope we are wrong about this, and applaud anyone who has done so. Recently, several excellent guides to standardized measuring of relevant ecosystem functions have begun to appear in the literature (TWO TREE PAPERS). We suggest these as a starting point for any researchers interested in thinking carefully about the topic, in addition to fruitful discussions of with ecosystem ecologists working in the same system. They are highly likely to disabuse any community ecologist of the notion that they have fully captures a good sample of the population of relevant functions, and provide guidance on further ways to do so (J. Bowen, pers. com.).

#### Conclusions

We hope that this piece will provide the field of multifunctionality with a way out of its current state of division and confusion. Further, we hope it provides food for additional theory that addresses the causes and consequences of ecosystem multifunctionality, something that is currently sorely lacking. We have been heartened by the idea leaving the cradle of the field of biodiversity and ecosystem function, and feel that it has the promise to provide a holistic unifying concept for anyone interested in capturing a snapshot of system dynamics in single meaningful metric.
