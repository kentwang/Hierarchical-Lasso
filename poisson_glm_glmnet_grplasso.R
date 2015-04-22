library(mvtnorm)
library(dummies)

set.seed(20150422)

##- generate 15 cat vars with 3 levels
p = 15
n = 1000

# multivariate normal data matrix
rowMat = matrix(rep(1:p, p), ncol = p)
colMat = matrix(rep(1:p, each = p), ncol = p)
Sigma = 0.5^(abs(rowMat - colMat)) # covariance matrix

Z = rmvnorm(n = n, mean = rep(0, p), sigma = Sigma)
Z[Z > qnorm(2/3)] = 3
Z[Z <= qnorm(1/3)] = 1
Z[!(Z == 1) & !(Z == 3)] = 2
Z = as.data.frame(Z)
names(Z) = paste("z", 1:p, sep = "")
Z = as.data.frame(lapply(Z, as.factor))

# dummy variables. Include intercept. No interaction for now
# NOTE: Centralization of response variable doesn't work here
# todo: add interaction
Z_dummy = model.matrix(as.formula(paste("~", paste(names(Z), collapse = " + "))), Z)

# true coefficients
p_beta = 6
id_nonzero = c(2, 3, 6, 7, 10, 11) # intercept. z12 z13 z32 z33 z52 z53
beta_true = rep(0, dim(Z_dummy)[2])
beta_true[id_nonzero] = round(rnorm(p_beta), digits = 1)
beta_true[1] = -2 # intercept

# generate response
mu = exp(Z_dummy %*% as.vector(beta_true) + 0.1 * rnorm(n))
y = rpois(n = n, lambda = mu)



##- Fit models

# generate id's for training data and testing data
id.train = sort(sample(1:n, 0.8*n))
id.test = setdiff(1:n, id.train)

# GLM: naive statistical regression
data.glm = data.frame(y, Z)[id.train, ]
data.glm.test = data.frame(y, Z)[id.test, ]
fit.glm = glm(as.formula(paste("y ~ ", paste(names(Z), collapse = " + "))), 
              data = data.glm, family = poisson)

cbind(truth = beta_true, glm = coef(fit.glm))

y.pred.glm = predict(fit.glm, newdata = data.glm.test, type = "response")
mse.glm = mean((y[id.test] - y.pred.glm)^2)

# glmnet: lasso with all dummies and no grouping
data.glmnet = as.matrix(data.frame(y, Z_dummy[, -1]))[id.train, ]
data.glmnet.test = as.matrix(data.frame(y, Z_dummy[, -1]))[id.test, ]
fit.cv.glmnet = cv.glmnet(data.glmnet[, -1], data.glmnet[, 1], family = "poisson")
plot(fit.cv.glmnet)
plot(fit.cv.glmnet$glmnet.fit, xvar = "lambda")
abline(v = log(fit.cv.glmnet$lambda.min))

cbind(truth = beta_true, glmnet = as.numeric(coef(fit.cv.glmnet, s = "lambda.min")))

y.pred.glmnet = predict(fit.cv.glmnet$glmnet.fit, newx = data.glmnet.test[, -1], 
                        type = "response", s = fit.cv.glmnet$lambda.min)
mse.glmnet = mean((y[id.test] - y.pred.glmnet)^2)


