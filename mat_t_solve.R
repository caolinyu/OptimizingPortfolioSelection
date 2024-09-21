mat_t_solve = function(data1,data2){
  Cov_mat_t = semi_cov_solve(data1)
  Cov_mat_t_1 = semi_cov_solve(data2)
  Cor_mat_t = semi_cor_solve(data1)
  Cor_mat_t_1 = semi_cor_solve(data2)
  if(is.na( Cor_mat_t)[1,1] | is.na( Cor_mat_t_1)[1,1] ){
    mat_t_ni = array(NA,dim=c(p,p,12))
  }else{
     Diag_mat_t = diag(diag(Cov_mat_t))
     Diag_mat_t_1 = diag(diag(Cov_mat_t_1))
     #F matrix or identity matrix
     I_mat_t = F_mat(data1)  #I_mat_t = diag(p)
     mat_t = array(0,dim=c(p,p,12))
     mat_t[,,1] = Diag_mat_t %*% Cor_mat_t %*% Diag_mat_t
     mat_t[,,2] = Diag_mat_t %*% Cor_mat_t_1 %*% Diag_mat_t
     mat_t[,,3] = Diag_mat_t %*% I_mat_t %*% Diag_mat_t
     mat_t[,,4] = Diag_mat_t_1 %*% Cor_mat_t %*% Diag_mat_t_1
     mat_t[,,5] = Diag_mat_t_1 %*% Cor_mat_t_1 %*% Diag_mat_t_1
     mat_t[,,6] = Diag_mat_t_1 %*% I_mat_t %*% Diag_mat_t_1
     mat_t[,,7] = Diag_mat_t %*% Cor_mat_t %*% Diag_mat_t_1
     mat_t[,,8] = Diag_mat_t %*% Cor_mat_t_1 %*% Diag_mat_t_1
     mat_t[,,9] = Diag_mat_t %*% I_mat_t %*% Diag_mat_t_1
     mat_t[,,10] = Diag_mat_t_1 %*% Cor_mat_t %*% Diag_mat_t
     mat_t[,,11] = Diag_mat_t_1 %*% Cor_mat_t_1 %*% Diag_mat_t
     mat_t[,,12] = Diag_mat_t_1 %*% I_mat_t %*% Diag_mat_t
     mat_t_ni = array(0,dim=c(p,p,12))

  
     for(j in 1:12){
      mat_t_ni[,,j] = ginv(mat_t[,,j])
      }
  }
  
  return(mat_t_ni)
}



