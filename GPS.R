GPS <- function(G,ID){
  
  ind_ctrl = which(ID=="Control")
  ind_case = which(ID=="Case")
  
  euc.dist <- function(x1, x2) sqrt((1/ncol(x1))*rowSums((x1 - x2) ^ 2)) 
  
  GVS = matrix(0,length(G),length(ID))
  for(i in 1:length(G)){
  
  mean_control = colMeans(G[[i]][ind_ctrl,])
  
  GVS[i,] = euc.dist(G[[i]],matrix(rep(mean_control,nrow(G[[i]])),
                                nrow(G[[i]]),
                                ncol(G[[i]]),
                                byrow=T))
  }
  
  GPS = sqrt((1/nrow(GVS))*colSums((GVS) ^ 2)) 
  
  return(list(GPS,GVS))
}