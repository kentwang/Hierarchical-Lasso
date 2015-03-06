# create interaction matrix from design matrices from two categorical variables
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