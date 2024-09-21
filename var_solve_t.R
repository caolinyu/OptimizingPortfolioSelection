
var_solve_t = function(data1,data2){
  W_R_matrix = matrix(0,t,12)
  for(j in 1:t){
    data1_new = data1[-j,]
    data2_new = data2[-j,]
    mat_t_ni_new_3 = mat_t_solve(data1_new,data2_new)
    w_mat_new = w_solve(mat_t_ni_new_3) 
    for(k in 1:12){
      W_R_matrix[j,k]=  (as.numeric(data1[j,]) %*% as.numeric(w_mat_new[,k]))[1,1]
    }
    
  }
    return(W_R_matrix) 
  
}

