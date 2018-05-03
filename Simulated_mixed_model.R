set.seed(12346)
nclus = 100                                # number of groups
clus = factor(rep(1:nclus, each=10))       # cluster variable
n = length(clus)                           # total n

# parameters
sigma = 1                                  # residual sd
tau = .5                                   # re sd
gamma_ = rnorm(nclus, mean=0, sd=tau)      # random effects
e = rnorm(n, mean=0, sd=sigma)             # residual error
intercept = 3                              # fixed effects
b1 = .75

# data
x = rnorm(n)                               # covariate
y = intercept + b1*x + gamma_[clus] + e    # see model 1
d = data.frame(x, y, clus=clus)

# matrix form
# X = cbind(1, x)
# B = c(intercept, b1)
# Z = model.matrix(~-1+clus)               # see model 3a
# y2 = X%*%B + Z%*%gamma_ + e
# head(cbind(y, y2))

library(lme4)
lmeMod = lmer(y ~ x + (1|clus), data=d)
summary(lmeMod)