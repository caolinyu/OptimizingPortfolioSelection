
w_solve = function(mat_3){

  I_mat = matrix(rep(1,p),p,1)

  w_mat = matrix(rep(0,p*12),p,12)
  
  for(j in 1:12){
    w_mat[,j] = ( mat_3[,,j] %*% matrix(rep(1,p),p,1) ) / (t(I_mat) %*% mat_3[,,j] %*%I_mat)[1,1]
  }
  return(w_mat) 
  
}






