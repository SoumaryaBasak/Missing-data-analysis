# Missing-data-analysis

Estimation of the parameters from a data with missing values is a cumbersome task. To get the _maximum likelihood estimator_ we need a special sort of treatment namely _The E-M Algorithm_. _Monotone Missingness_ is an exception of that. It allows us to find the parameter rather easily then EM algorithm.

Monotone missingness is a special type of missingness that allows us to maximize the likelihood by maximizing the conditional likelihoods of the each variables given the previous variables provided the variables are arranged in a increasing order of no. of missingness.

 - Here I propose a function to get estimates from a data having monotone missingness.
 - The `mono_ml.R` file contains that function, with a example on a simulated data.  
