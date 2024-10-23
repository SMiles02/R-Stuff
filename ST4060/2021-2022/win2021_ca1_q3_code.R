dat = data.frame(wt=mtcars$wt, mpg=mtcars$mpg)
set.seed(6040)
B = 100
int = pval = eff = numeric(B)
for (b in 1:B) {
  ib = sample(1:nrow(dat), nrow(dat), replace=TRUE)
  xb = dat[ib,]
  lmb = lm(mpg~wt, data=xb)
  int[b] = summary(lmb)$coef[1,1]
  eff[b] = summary(lmb)$coef[2,1]
  pval[b] = summary(lmb)$coef[2,4]
}

# (a)
mean(eff)

# (b)
quantile(pval, c(0.025, 0.975))

# (c)
lm0 = lm(mpg~wt, data=dat)
eff0 = summary(lm0)$coefficients[2,1]
2*eff0-rev(quantile(eff, c(0.025, 0.975)))

# (d)
se0 = summary(lm0)$coefficients[2,2]
eff0 + c(-1, 1) * 1.96 * se0