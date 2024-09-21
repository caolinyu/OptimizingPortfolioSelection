semi_cor_solve = function(data1){
  p = dim(data1)[2]
  mat_zeros = matrix(rep(1,p*p),p,p)
  for(i in 1:(p-1)){
    for(j in (i+1):p){
      #print(c(i,j))
      
      if( sum(data1[,i])>0  & sum(data1[,j])>0 ){
          mat_sel = subset( data1[,c(i,j)] , data1[,i]>0 & data1[,j]>0)
          mat_zeros[i,j] = cor(mat_sel)[1,2]
          mat_zeros[j,i] = cor(mat_sel)[1,2]
          }
        
      else if(sum(data1[,i])<0  & sum(data1[,j])<0){
          mat_sel = subset( data1[,c(i,j)] , data1[,i]<0 & data1[,j]<0)
          mat_zeros[i,j] = cor(mat_sel)[1,2]
          mat_zeros[j,i] = cor(mat_sel)[1,2]
        }
      else{
        mat_sel = data1[,c(i,j)]
        mat_zeros[i,j] = cor(mat_sel)[1,2]
        mat_zeros[j,i] = cor(mat_sel)[1,2]
      }
    }
  }
  mat_zeros[is.na(mat_zeros)] = 0
  return(mat_zeros)
}

