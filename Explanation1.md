### What is a bootstrap sample?
A bootstrap sample -or resample to be more precise- is **drawn by resampling with replacement** from from a regular sample. This means that if we have a regular sample of size _n_, we randomly extract _n_ elements from it with replacement and this _n_ elements will constitute our bootstrap sample. With enough computational power we can get as many bootstrap samples as we want.

### Why do we use them?
In order to know how close is our sample statistic from the population parameter we need to either have a large sample size or assume the shape of the distribution, but this is not always possible.

In these cases it's a common practice to assume that with a large enough number of bootstrap resamples we can get a **sampling distribution of estimates** whose centrality is **close to the real value** for the population.

In the boxplot above we can see where the parameter lies inside the bootstrap sampling distribution of estimates.


<br/>
<cite>Source: Spiegelhalter D. The art of statistics. London: Penguin Books; 2020.</cite>