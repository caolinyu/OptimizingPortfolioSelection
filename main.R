library(MASS)
#384*20
datad = read.csv("C:/Users/cly/Desktop/lunwen1/第四次修改/OptimizingPortfolioSelection-main/new_data14-16.csv",header = FALSE) 

time = 10  # Number of repetitions
count = 5  # Stock dimension: IP

for(t1 in 1: time){
  data = datad[,sample(ncol(datad), count)]
  
  dim_mid=dim(data) 
  n = dim_mid[1] #384
  p = dim_mid[2] #count

  w = c(80,90,100,110,120) #Window width
  result = matrix(rep(0,6*length(w)),length(w),6)
  
  for(a in 1:length(w)){
    t=w[a]
    print(t)
    zero_mat = matrix(rep(0,(p*(n-t-1))),p,n-t-1) #"Weight matrix"
    mu_i = c(rep(0,n-t-1)) #Mean

    for ( i in 1:(n-t-1)){
      cat("window widths:",t,"——",i,"\n")
      data1 = data[(i+1):(i+t),] 
      data2 = data[(i):(i+t-1),]
      mat_t_ni = mat_t_solve(data1,data2)
      if(!is.na(mat_t_ni)[1,1,1]){
        w_mat = w_solve(mat_t_ni)
        W_R_matrix = var_solve_t(data1,data2)
        d_dest = d_dest_solve(W_R_matrix)
        Cov_mat = matrix(c(rep(0,p)),p,1)
        for( k in 1:11){ Cov_mat = Cov_mat + w_mat[,k] * d_dest[k]}
        Cov_mat = Cov_mat + w_mat[,12] * (1-sum(d_dest))
        zero_mat[,i]=Cov_mat
      }
      
      mu_i[i]=t(zero_mat[,i]) %*% as.numeric(data[t+i,])
    }
  
    mean_mu= mean(subset(mu_i,mu_i!=0))
  
    sigma_i = rep(0,n-t-1)
    for (i in 1:(n-t-1)){
      if(t(zero_mat[,i]) %*% as.numeric(data[t+i,]) == 0 ){
        sigma_i[i] = 0
      }else{
        sigma_i[i] = (t(zero_mat[,i]) %*% as.numeric(data[t+i,]) - mean_mu)**2
      }
    }
    sigma = mean(subset(sigma_i,sigma_i!=0))
    SR = mean_mu/sqrt(sigma)
    
    Trans_zeromat = t(zero_mat)
    newdata = data[(t+1):(n-1),]
    del_l = which(rowMeans(Trans_zeromat) ==0)
    
    if(is.null(del_l)){
      Trans_zeromat1 = Trans_zeromat[-del_l,]
      newdata1 = newdata[-del_l,]
      n1 = dim(newdata1)[1]
    }
    else{
      Trans_zeromat1 = Trans_zeromat
      newdata1 = newdata 
      n1 = dim(newdata1)[1]
    }
  
    TR_mat = matrix(0,(n1-1),p)
    for ( i in 1:(n1-1)){
        for ( j in 1:p){
          TR_mat[i,j] = abs(Trans_zeromat1[i,j]*(newdata1[i,j]+1)-Trans_zeromat1[(i+1),j])
        }
      }
  
    Turnover = mean(rowSums(TR_mat))
    SR_adj= (mean_mu-0.005*Turnover)/sqrt(sigma)
  
    result[a,1] = t
    result[a,2] = mean_mu
    result[a,3] = sigma
    result[a,4] = SR
    result[a,5] = Turnover
    result[a,6] = SR_adj
}
  
  #write.table(result,file=paste0("E:/U/result/",count ,"IP",t1,".csv"),sep=",",row.names=TRUE,col.names=TRUE,quote=TRUE)

  }




