# Monte Carlo illustration of Le Cam’s Theorem
N = 100
p = .1
set.seed(4060)
M = 1000
means = vars = NULL
Ps = seq(.1,.9,by=.1)
for(p in Ps) {
  m = v = numeric(M)
  for(i in 1:M) {
    X = matrix(rbinom(5*N,1,p),nrow=N,ncol=5)
    S = apply(X,1,sum)
    m[i] = mean(S)
    v[i] = var(S)
  }
  means = cbind(means, m)
  vars = cbind(vars, v)
}
boxplot(means, names=Ps, col='cyan')
abline(h=5*Ps, col=8)