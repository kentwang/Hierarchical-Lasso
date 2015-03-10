library(glmnet) # alpha = 1 is lasso
library(glinternet)
library(hierNet)
library(grplasso) # package by Meier
library(gglasso) # package by Yuan and Lim
library(dplyr)
library(dummies)


n = 100
p = 3

# generate random categorical vars with 100 obs
# x1 = sample(c(1, 2, 3, 4, 5), n, replace = T)
# x2 = sample(c(1, 2, 3), n, replace = T)
# x3 = sample(c(1, 2), n, replace = T)
# X = data.frame(cbind(intercept = rep(1, n), x1, x2, x3))
# X$x1 = as.factor(X$x1)
# X$x2 = as.factor(X$x2)
# X$x3 = as.factor(X$x3)

# simulation from glinternet package
# continuous response, categorical variables
set.seed(20150305)
X = matrix(sample(0:2, 100*p, replace=TRUE), nrow=100) # design matrix
numLevels = rep(3, p)

x1 = as.factor(X[, 1])
x2 = as.factor(X[, 2])
x3 = as.factor(X[, 3])

X = data.frame(x1, x2, x3)

# dummies
intercept = rep(1, n)
x1_d = data.frame(dummy(x1)); names(x1_d) = paste("x1", 0:(p-1), sep = "")
x2_d = data.frame(dummy(x1)); names(x2_d) = paste("x2", 0:(p-1), sep = "")
x3_d = data.frame(dummy(x1)); names(x3_d) = paste("x3", 0:(p-1), sep = "")
x12_d = interaction_dummy(x1_d, x2_d)
x13_d = interaction_dummy(x1_d, x3_d)
x23_d = interaction_dummy(x2_d, x3_d)
X_d = cbind(intercept, x1_d, x2_d, x3_d, x12_d, x13_d, x23_d)


# fit = glinternet(X, Y, numLevels)

# hierarchical truth \Theta_jk != 0 => \beta_j != 0 and \beta_k != 0
# draw both the coefficients and error from N(0, 1).
# coefficient convention: b0, b1, b2, b3, b12, b13, b23

## strong hierrchy model-I x1 x2 x3 x1x2. Simulation of coefficients
beta0 = rnorm(1)
beta1 = rnorm(length(levels(x1))-1); beta1 = c(-sum(beta1), beta1)
beta2 = rnorm(length(levels(x2))-1); beta2 = c(-sum(beta2), beta2)
beta3 = rnorm(length(levels(x3))-1); beta3 = c(-sum(beta3), beta3)
beta12 = rnorm(length(levels(x1)) * length(levels(x2)) - 1); beta12 = c(-sum(beta12), beta12)
beta13 = rep(0, length(levels(x1)) * length(levels(x3)))
beta23 = rep(0, length(levels(x2)) * length(levels(x3)))

Beta = t(t(c(beta0, beta1, beta2, beta3, beta12, beta13, beta23)))

# simulation of response with some stn ratio
# Note: given X, the signal variability of a categirical variable is due to one of its levels
var_signal = 4 # intercept is constant
y_true = as.matrix(X_d) %*% Beta
y_sim = y_true + rnorm(100, 0, 0.5)


## Try to use all the models and get the model calls unified
data_sim = data.frame(intercept, v1 = x1, v2 = x2, v3 = x3, y = y_sim) # in case of data input
fmla = as.formula("y ~ v1 + v2 + v3 + v1*v2 + v1*v3 + v2*v3") 

# fit model with lm
fit.lm = lm(y ~ v1 + v2 + v3 + v1 * v2 + v1 * v3 + v2 * v3, data = data_sim)
summary(fit.lm)

# fit model with pure lasso using glmnet
fit.lasso.cv = cv.glmnet(model.matrix(fmla, data_sim)[,-1], 
                      as.matrix(data_sim[,as.character(fmla[[2]])]), 
                      family = "gaussian", alpha = 1, standardize = FALSE)
coef(fit.lasso.cv, s="lambda.min")
# predict(cvob1,newx=x[1:5,], s="lambda.min")
fit.lasso = glmnet(model.matrix(fmla, data_sim)[,-1], 
            as.matrix(data_sim[,as.character(fmla[[2]])]), 
            family = "gaussian", alpha = 1, standardize = FALSE)

plot(fit.lasso)


# fit fit model using Lim & Hastie (2013) glinternet
# probability need to select the best lambda using the cross-validation
fit.lh = glinternet(X, y_sim, numLevels = c(3, 3, 3))

# group lasso by yuan and lin. R gglasso
group = c(1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6)
fit.gl = gglasso(model.matrix(fmla, data_sim)[,-1], y_sim, group = group)

save(data_sim, file = "data/categorical.RData")
