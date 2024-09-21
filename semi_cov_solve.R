semi_cov_solve = function (mat){
  n1 = dim(mat)[1]
  clo_mean = colMeans(mat)
  one_mat = matrix(rep(1,n1),n1,1)
  mean_mat = one_mat%*%clo_mean
  mid = mat-mean_mat
  mat1 = mid * (mid<0) 
  seimv = colMeans(mat1*mat1)
  seimv_new = diag(seimv)
  return(seimv_new)
}
