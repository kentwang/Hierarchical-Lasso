set.seed(20150413)

n.sim = 1000
x1 = rnorm(n.sim)
x2 = rnorm(n.sim)
x3 = rnorm(n.sim)
mu = exp(0.1 + 0.2 * x1 + 0.5 * x2 + 0.1 * x1 * x2)
y.sim = rpois(n.sim, mu)
barplot(table(y.sim))

# fit the simple poison regression
df = data.frame(y.sim = y.sim, x1 = x1, x2 = x2, x3 = x3)
fit = glm(y.sim ~ x1 + x2 + x1* x2 + x3, data = df, family = "poisson")
summary(fit)


## simulation with categorical interactions

set.seed(20150413)
x1 = factor(sample(1:5, replace = TRUE, size = n.sim))
x2 = factor(sample(1:2, replace = TRUE, size = n.sim))
x3 = factor(sample(1:3, replace = TRUE, size = n.sim))
X = matrix()
