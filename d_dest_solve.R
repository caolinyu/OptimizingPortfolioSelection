d_dest_solve = function(W_R_matrix){
  a_mat = matrix(rep(0,121),11,11)
  b_mat = matrix(rep(0,121),11,11)
  c_mat = c(rep(0,11))
  
  #求a_mat c_mat
  for(k in 1:11){
    for(j in 1:11){
      a_mat[k,j] = -var(W_R_matrix[,k] - W_R_matrix[,j]) + var(W_R_matrix[,k] - W_R_matrix[,12]) + var(W_R_matrix[,j] - W_R_matrix[,12])
    }
    b_mat[k,k] = 2* var((W_R_matrix[,k] - W_R_matrix[,12]))
    c_mat[k] = var(W_R_matrix[,k]) -var(W_R_matrix[,12]) - var(W_R_matrix[,k] - W_R_matrix[,12])
  }
  for(k in 1:11){
    a_mat[k,k] =  b_mat[k,k]
  }
  
  d_est = ginv(a_mat) %*% c_mat
  return(d_est)
}
