set.seed(4060)
dat = read.csv('C:/Users/mahaj/Documents/R-Stuff/ST4060/2022-2023/commute.csv', stringsAsFactors=TRUE)
B = 1000
n = nrow(dat)
samples = matrix(sample(dat$Time, size=B*n, replace=TRUE), B, n)
bmeans = apply(samples, 1, mean)
time.se = sd(bmeans)

# (a)
# BS expected value
mean(bmeans)
# BS std error
time.se

pb = numeric(B)
for(b in 1:B) {
  ib = sample(1:n,n,replace=TRUE)
  datb = dat[ib,]
  pb[b] = cor.test(datb$Age,datb$Time)$p.value
}

# (b)
mean(pb)
# (c)
mean(pb<.05)

# statistic of original sample:
p0 = cor.test(dat$Age,dat$Time)$p.value
# (d)
# BS bias:
mean(pb)-p0
# (e)
# 95% Normal CI:
mean(pb)+c(-1,1)*1.96*sd(pb)
# (f)
# BS-adjusted CI:
2*p0 - quantile(pb,c(.975,.025))
# (g)
# Normal CI not appropriate given the distribution:
hist(pb)