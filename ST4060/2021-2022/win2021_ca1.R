set.seed(4060)
N = 100
dfx = 2
thbar = 8
x = runif(N, 1, 2)
M = 1000
lmos = lmeds = lmeans = numeric(M)
for (i in 1:M) {
  z = rt(n=N, df=dfx)
  y = thbar * x + z
  lmo = lm(y~x+0)
  lmos[i] = as.numeric(coef(lmo))
  lmeds[i] = median(y / x)
  lmeans[i] = mean(y / x)
}

#
mean(lmos)
sd(lmos)

#
mean(lmeds)
sd(lmeds)

#
mean(lmeans)
sd(lmeans)

