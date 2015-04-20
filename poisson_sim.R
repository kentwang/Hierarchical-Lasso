library(dplyr)
set.seed(20150413)

n.sim = 1000
x1 = rnorm(n.sim)
x2 = rnorm(n.sim)
x3 = rnorm(n.sim)
mu = exp(0.1 + 0.2 * x1 + 0.5 * x2 + 0.1 * x1 * x2 + 0.01 * rnorm(n.sim))
y.sim = rpois(n.sim, mu)
barplot(table(y.sim))

# fit the simple poison regression
df = data.frame(y.sim = y.sim, x1 = x1, x2 = x2, x3 = x3)
fit = glm(y.sim ~ x1 + x2 + x1* x2 + x3, data = df, family = "poisson")
summary(fit)


## simulation with categorical interactions

set.seed(20150413)
pCat = 3
x1 = factor(sample(1:3, replace = TRUE, size = n.sim))
x2 = factor(sample(1:3, replace = TRUE, size = n.sim))
x3 = factor(sample(1:3, replace = TRUE, size = n.sim))
numLevels = c(length(levels(x1)), length(levels(x2)), length(levels(x3)))

X_raw = data.frame(x1 = x1, x2 = x2, x3 = x3)

X = as.data.frame(model.matrix(~ x1 + x2 + x3 + x1 * x2 + x1 * x3 + x2 * x3))

## logistic. works just fine  "binomial"
# mu = -5 + 3*X$x12 + 2*X$x13 + 3*X$x22 + 2*X$x23 + X$x12 * X$x22 + 1.5 * 
#            X$x12 * X$x23 + 2 * X$x13 * X$x22 + 2.5 * X$x13 * X$x23 + 0.001*rnorm(n.sim)
# pi <- exp(mu) / (1 + exp(mu))
# y.sim <- rbinom(n=n.sim, size=1, prob=pi)

## Poisson. Looks like its fine as well "Poisson" but intercept should be negative
mu = exp(-5 + 3*X$x12 + 2*X$x13 + 3*X$x22 + 2*X$x23 + X$x12 * X$x22 + 1.5 * 
  X$x12 * X$x23 + 2 * X$x13 * X$x22 + 2.5 * X$x13 * X$x23 + 0.001*rnorm(n.sim)) # noise ratio is too big
y.sim = rpois(n.sim, mu)
# barplot(table(y.sim))


# Normal works just fine "gaussian"
# y.sim =  -5 + 3*X$x12 + 2*X$x13 + 3*X$x22 + 2*X$x23 + X$x12 * X$x22 + 1.5 * 
#   X$x12 * X$x23 + 2 * X$x13 * X$x22 + 2.5 * X$x13 * X$x23 + 0.001*rnorm(n.sim)

# full model  paste(colnames(X)[-1], collapse = " + ")
fmla.full = formula(y.sim ~ x12 + x13 + x22 + x23 + x32 + x33 + 
                      x12:x22 + x13:x22 + x12:x23 + x13:x23 + 
                      x12:x32 + x13:x32 + x12:x33 + x13:x33 + 
                      x22:x32 + x23:x32 + x22:x33 + x23:x33)
# ground true
fmla.0 = formula(y.sim ~ x12 + x13 + x22 + x23 + x12:x22 + x13:x22 + x12:x23 + x13:x23)

fmla.1 = formula(y.sim ~ x12 + x13 + x22 + x23 + x32 + x33 + 
                   x12:x22 + x13:x22 + x12:x23 + x13:x23 + 
                   x12:x32 + x13:x32 + x12:x33 + x13:x33)


# define data frame
df = cbind(y.sim, X)
df.raw = cbind(y.sim, X_raw)

# df1 = cbind(y.sim, X)
# fit1 = glm(fmla1, data = df1, family = "poisson")
# summary(fit1)


## fit different models using glm
fit.full = glm(fmla.full, data = df, family = "poisson")
summary(fit.full)

fit.0 = glm(fmla.0, data = df, family = "poisson")
summary(fit.0)

fit.1 = glm(fmla.1, data = df, family = "poisson")
summary(fit.1)


## fit models using gglasso
group.0 = c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6)
#group.0 = NULL
# select best lambda using CV
fit.cv.gglasso.0 = cv.gglasso(as.matrix(X[, -1]), y.sim, group = group.0)
plot(gglasso(as.matrix(X[, -1]), y.sim, group = group.0))
abline(abline(v = log(fit.cv.gglasso.0$lambda.min)))
# 
fit.gglasso.0 = gglasso(as.matrix(X[, -1]), y.sim, group = group.0, lambda = fit.cv.gglasso.0$lambda.min)
coef(fit.gglasso.0)
