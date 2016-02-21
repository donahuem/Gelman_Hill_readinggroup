# 2. Predictive checks: using data of interest to you, fit a model of interest.
# (a) Simulate replicated datasets and visually compare to the actual data.
# (b) Summarize the data by a numerical test statistic, and compare to the values
# of the test statistic in the replicated datasets.

# I will just provide pseudo-code for simple linear regression 
# model : y = xB + e where e ~ N(0,1)

# R code ref: P. 156 :p

# (a) Simulate replicated datasets 
# 1. run lm(y~x) for model y = xB + e where e~ N(0,1)
# 2. save coefficients (parameter estimates: a, b in the example on 156)

# 3. generate fake data using the saved coefficients 
#    the fake_y(n) = xB + rnorm(n, mean =0, sd = 1)
# 5. repeat 3 for many times (n.sims times) 
# 6. For visual comparison, you can plot y and fake_y's   


# (b) comparing observed values and the fake ones numerically
#     reference 3rd block of R code on P156

