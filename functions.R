#== create interaction matrix from design matrices from two categorical variables
interaction_dummy = function(df1, df2) {
  temp = NULL
  temp.names = NULL
  for(i in 1:ncol(df1)) {
    for(j in 1:ncol(df2)) {
      temp = cbind(temp, df1[, i] * df2[, j])
      temp.names = c(temp.names, paste(names(df1)[i], names(df2)[j], sep = "."))
    }
  }
  
  temp = data.frame(temp)
  names(temp) = temp.names
  
  return(temp)  
}

#== partition data for cross-validation
cvfolds <- function(n,k=10,seed) { 
  if(!missing(seed)) set.seed(seed)
  sample(rep(seq(k),length=n))
}

# evaluation of predictive performance through cross validation
# 
# todo:
#    - keep the same foldid for each iteration

#== model evaluation using CV
#=  todo:
#     - consider what to return
#     - maybe recoded for glm
predEvaluation = function(data, flma, method) {
  X = model.matrix(fmla,data)
  Y = as.matrix(data[,as.character(fmla[[2]])])
}

#== cross validation for grplasso
# Z_dummy[id.train, ]; y = y[id.train]
cv.grplasso.my = function(x, y, index, lambda, model, ...) {
  n = length(y)
  p = dim(x)[2]
  nlambda = length(lambda)
  
  # define the output object
  result = list()
  result$lambda = lambda
  result$model = model
  result$index = index
    
  foldid = cvfolds(n, seed = 20150423)
  CVError = rep(0, nlambda)
  
  K = sort(unique(foldid))
  for(k in K) {
    # cat("iter", k, "\n")
    test = which(foldid == k)
    train = which(foldid != k)
    fit = grplasso(x[train, ], y = y[train], index = index, model = model, 
                   lambda = lambda, control = grpl.control(trace = 0), ...) 
    pred.grplasso = predict(fit, newdata = x[test, ], type = "response")
    CVError = CVError + apply(pred.grplasso, 2, FUN = function(v) mean((v - y[test])^2))
  }
  CVError = CVError/length(K)
  
  result$CVError = CVError
  result$lambda.min = lambda[which.min(CVError)]
  result$fit = grplasso(x, y = y, index = index,model = model, 
                        lambda = lambda, control = grpl.control(trace = 0)) 
  
  return(result)
}

##== Prediction for cv.grplasso.my
predict.cv.grplasso.my = function(fit.cv.grplasso, newdata, s, type = "response") {
  id = which(fit.cv.grplasso$lambda == s)
  prediction = predict(fit.cv.grplasso$fit, newdata = newdata, type = type)[, id]
  return(prediction)
}


