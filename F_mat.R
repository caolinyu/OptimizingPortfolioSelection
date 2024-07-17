F_mat = function(data){

  p = dim(data)[2]
  n = dim(data)[1]

  MR = rowMeans(data)
  S1 = data[,1]
  fit = lm(S1~MR)
  fit$coefficients[2]
  fit$residuals
  beta = rep(0,p)
  sigma = rep(0,p)
  var_MR = var(MR)
  for(i in 1:p){
    fit = lm(data[,i]~MR)
    beta[i] = fit$coefficients[2]
    sigma[i] = var(fit$residuals)
  }
  F_mat = var_MR*beta%*%t(beta)+diag(sigma)
  return(F_mat)
}
