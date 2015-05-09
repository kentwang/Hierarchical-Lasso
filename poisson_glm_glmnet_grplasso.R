###- Comparing performance of GLM, Lasso and Group Lasso on Poisson regression
#- todo: offset term


library(mvtnorm)
library(dummies)
library(ggplot2)

set.seed(20150422)

##- generate 15 cat vars with 3 levels. Roughly follow Yuan and Lin (2006)
p = 15
n = 1000

# multivariate normal data matrix
rowMat = matrix(rep(1:p, p), ncol = p)
colMat = matrix(rep(1:p, each = p), ncol = p)
baseCor = 0.9 # baseline correlation
Sigma = 0.5^(abs(rowMat - colMat)) # covariance matrix

Z = rmvnorm(n = n, mean = rep(0, p), sigma = Sigma)
Z[Z > qnorm(2/3)] = 3
Z[Z <= qnorm(1/3)] = 1
Z[!(Z == 1) & !(Z == 3)] = 2
Z = as.data.frame(Z)
names(Z) = c(paste("z0", paste(1:9, "_", sep = ""), sep = ""), paste("z", paste(10:p, "_", sep = ""), sep = ""))
Z = as.data.frame(lapply(Z, as.factor))

# dummy variables. Include intercept. No interaction for now
# NOTE: Centralization of response variable doesn't work here
# todo: add interaction
Z_dummy = model.matrix(as.formula(paste("~", paste(names(Z), collapse = " + "))), Z)

# true coefficients
# p_beta = 6
# id_nonzero = c(2, 3, 6, 7, 10, 11) # intercept. z12 z13 z32 z33 z52 z53

p_beta = 16
id_nonzero = 2:17
beta_true = rep(0, dim(Z_dummy)[2])
beta_true[id_nonzero] = round(rnorm(p_beta), digits = 1)
beta_true[1] = -2 # intercept

# generate response
mu = exp(Z_dummy %*% as.vector(beta_true) + 0.1 * rnorm(n))
y = rpois(n = n, lambda = mu)



##- Fit models

#- generate id's for training data and testing data
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

#- glmnet: lasso with all dummies and no grouping
data.glmnet = as.matrix(data.frame(y, Z_dummy[, -1]))[id.train, ]
data.glmnet.test = as.matrix(data.frame(y, Z_dummy[, -1]))[id.test, ]
fit.cv.glmnet = cv.glmnet(data.glmnet[, -1], data.glmnet[, 1], alpha = 1, family = "poisson")
plot(fit.cv.glmnet)
plot(fit.cv.glmnet$glmnet.fit, xvar = "lambda")
abline(v = log(fit.cv.glmnet$lambda.min))

cbind(truth = beta_true, glmnet = as.numeric(coef(fit.cv.glmnet, s = "lambda.min")))

y.pred.glmnet = predict(fit.cv.glmnet$glmnet.fit, newx = data.glmnet.test[, -1], 
                        type = "response", s = fit.cv.glmnet$lambda.min)
mse.glmnet = mean((y[id.test] - y.pred.glmnet)^2)


#- grplasso: grouplasso with all dummies
# use the same Lambda sequence as cv.glmnet
index = c(NA, rep(1:p, each = 2))
lambda = lambdamax(Z_dummy[id.train, ], y = y[id.train], index = index, penscale = sqrt, 
                    model = PoissReg()) * 0.7^(0:20)
# lambda = 100:1
fit.cv.grplasso = cv.grplasso.my(Z_dummy[id.train, ], y = y[id.train], index = index,
                                 model = PoissReg(), lambda = lambda)

# plot the coefficients path and the optimal one
plot(fit.cv.grplasso$fit)
abline(v = fit.cv.grplasso$lambda.min)

cbind(truth = beta_true, grplasso = fit.cv.grplasso$fit$coefficients[, which.min(fit.cv.grplasso$lambda)])

y.pred.grplasso = predict.cv.grplasso.my(fit.cv.grplasso, newdata = Z_dummy[id.test, ], 
                                         s = fit.cv.grplasso$lambda.min)

mse.grplasso = mean((y[id.test] - y.pred.grplasso)^2)



##- Comparing coefficients and mse
coefficients = data.frame(truth = beta_true, glm = coef(fit.glm), 
                   glmnet = as.numeric(coef(fit.cv.glmnet, s = "lambda.min")),
                   grplasso = fit.cv.grplasso$fit$coefficients[, which.min(fit.cv.grplasso$lambda)])

coef_reshape = rbind(
  data.frame(var = rownames(coefficients), model = rep("truth", dim(Z_dummy)[2]), coef = coefficients$truth),
  data.frame(var = rownames(coefficients), model = rep("glm", dim(Z_dummy)[2]), coef = coefficients$glm),
  data.frame(var = rownames(coefficients), model = rep(" glmnet", dim(Z_dummy)[2]), coef = coefficients$glmnet),
  data.frame(var = rownames(coefficients), model = rep(" grplasso", dim(Z_dummy)[2]), coef = coefficients$grplasso))

coef_reshape$coef = abs(coef_reshape$coef)

# Cleveland Dot Plot (Use the absolute value)
dot_theme = theme_bw() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_line(color="grey60",
                                        linetype="dashed"))

ggplot(coef_reshape, aes(y=var, x=coef)) + 
  geom_point(aes(shape = model, col = model), size = 3) + 
  dot_theme +
  scale_y_discrete("")
