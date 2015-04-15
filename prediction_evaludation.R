load("data/categorical.RData") # load data_sim

fmla = as.formula("y ~ v1 + v2 + v3 + v1*v2 + v1*v3 + v2*v3")

fold = cvfolds(nrow(data_sim),k=10,seed=20150311)


