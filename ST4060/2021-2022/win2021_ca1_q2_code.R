set.seed(4060)
N = 100
dfx = 2
thbar = 8
x = runif(N, 1, 2) # random variables from a uniform distrbution
M = 1000
lmos = lmeds = lmeans = numeric(M)
for (i in 1:M) {
  z = rt(n=N, df=dfx) # N random samples from the t-distribution with dfx degrees of freedom
  y = thbar * x + z 
  lmo = lm(y~x+0) # fit linear model
  lmos[i] = as.numeric(coef(lmo)) # ordinary least squares estimator
  lmeds[i] = median(y / x)
  lmeans[i] = mean(y / x)
}

#
mean(lmos) # expected value
sd(lmos) # standard error

#
mean(lmeds)
sd(lmeds)

#
mean(lmeans)
sd(lmeans)

#
boxplot(lmos, lmeds, lmeans)
abline(h=thbar, col=3)