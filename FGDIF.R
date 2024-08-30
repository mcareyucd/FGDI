FGDIF <- function(G,ID,PVE_I){
  
  library("elbow")
  library("ggplot2")
  library("refund")
  library("face")
  library("MFPCA")
  
  NT = ncol(G[[1]])
  N  = nrow(G[[1]])
  NJ = length(G)
  
  ind_ctrl = which(ID=="Control")
  ind_case = which(ID=="Case")
  
  ## Calculate the Univariate expansions
  NPC         = matrix(0,NJ,1)
  UPVE        = matrix(0,NJ,1)
  RMSE        = matrix(0,NJ,N)
  Uni_FGDI    = matrix(0,N,NJ)
  Scores      = list(NJ)
  Fits        = list(NJ)
  N_M         = min(N,50)
  for(i in 1: NJ){
  Datav        = as.matrix(G[[i]]) 
  pca2         = fpca.face(Y=Datav,pve=PVE_I,knots=N_M-2,center = TRUE)
  NPC[i]       = pca2$npc
  UPVE[i]      = pca2$pve
  Scores[[i]]  = pca2$scores
  Fits[[i]]    = pca2$Yhat
  RMSE[i,]     = sqrt((1/ncol(Datav))*rowSums((Datav-pca2$Yhat)^2))
  #print(c(NPC[i],mean(RMSE[i,]),pca2$pve))
  if(is.vector(Scores[[i]])){
    D            = (Scores[[i]]-mean(Scores[[i]][ind_ctrl]))^2
    GDI_MAT      = log(sqrt(D))
  }else{
    D            = (Scores[[i]]-t(matrix(rep(colMeans(Scores[[i]][ind_ctrl,]),N),NPC[i],N)))^2
    GDI_MAT      = log(sqrt(rowSums(apply(D,2,function(x){x}))))
  }
  Uni_FGDI[,i] = (GDI_MAT - mean(GDI_MAT[ind_ctrl]))/sd(GDI_MAT[ind_ctrl])
  }
  
  ## Multivariate PCA (Both Legs)
  ScoresC =cbind(Scores[[1]],Scores[[2]],Scores[[3]],Scores[[4]],Scores[[5]],Scores[[6]],
                      Scores[[7]],Scores[[8]],Scores[[9]],Scores[[13]],Scores[[14]],Scores[[15]],
                     Scores[[16]],Scores[[17]],Scores[[18]]) 
  Z = scale(ScoresC,scale = TRUE, center = TRUE)/ sqrt(N-1)
  e <- eigen(stats::cov(Z))
  M       <- which.min(abs(cumsum(e$values)/sum(e$values)-PVE_I))
  print(M)
  values  <- e$values[seq_len(M)]
  vectors <- e$vectors[,seq_len(M)]
  PVE     <- 100*sum(values)/sum(e$values)
 
  # normalization factors
  normFactors <- 1/sqrt(diag(as.matrix(Matrix::crossprod(Z %*% vectors))))

  ### Calculate scores
  scores <- Z %*% vectors * sqrt(N-1) # see defintion of Z above!
  scores <- as.matrix(scores %*% diag(sqrt(values) * normFactors, nrow = M, ncol = M)) # normalizatio

  #GDI Index
  ctrl     = t(matrix(rep(colMeans(scores[ind_ctrl,]),N),M,N))
  D        = (scores-ctrl)^2
  GDI_MAT  = log(sqrt(rowSums(apply(D,2,function(x){x}))))
  zGDI_MAT = (GDI_MAT - mean(GDI_MAT[ind_ctrl]))/sd(GDI_MAT[ind_ctrl])
  SGDI_MAT = 100-10*((GDI_MAT - mean(GDI_MAT[ind_ctrl]))/sd(GDI_MAT[ind_ctrl]))

  ## Multivariate PCA (Left Leg)
  ScoresL = cbind(Scores[[1]],Scores[[2]],Scores[[3]],Scores[[4]],Scores[[5]],Scores[[6]],
                  Scores[[7]],Scores[[8]],Scores[[9]])

  Z <- ScoresL / sqrt(N-1)
  e <- eigen(stats::cov(Z))
  M1       <- which.min(abs(cumsum(e$values)/sum(e$values)-PVE_I))
  print(M1)
  values <- e$values[seq_len(M1)]
  vectors <- e$vectors[,seq_len(M1)]

  # normalization factors
  normFactors <- 1/sqrt(diag(as.matrix(Matrix::crossprod(ScoresL %*% vectors))))

  ### Calculate scores
  scoresL <- ScoresL %*% vectors * sqrt(N-1) # see defintion of Z above!
  scoresL <- as.matrix(scoresL %*% diag(sqrt(values) * normFactors, nrow = M1, ncol = M1)) # normalizatio

  #GDI Index (Left Leg)
  ctrl = t(matrix(rep(colMeans(scoresL[ind_ctrl,]),N),M1,N))
  D = (scoresL-ctrl)^2
  GDI_MATL = sqrt(rowSums(apply(D,2,function(x){x})))
  zGDI_MATL =(GDI_MATL - mean(GDI_MATL[ind_ctrl]))/sd(GDI_MATL[ind_ctrl])

  ## Multivariate PCA (Right Leg)
  ScoresR = cbind(Scores[[10]],Scores[[11]],Scores[[12]],Scores[[13]],Scores[[14]],Scores[[15]],
                  Scores[[16]],Scores[[17]],Scores[[18]])

  Z <- ScoresR / sqrt(N-1)
  e <- eigen(stats::cov(Z))
  M2       <- which.min(abs(cumsum(e$values)/sum(e$values)-PVE_I))
  print(M2)
  values  <- e$values[seq_len(M2)]
  vectors <- e$vectors[,seq_len(M2)]

  # normalization factors
  normFactors <- 1/sqrt(diag(as.matrix(Matrix::crossprod(ScoresR %*% vectors))))

  ### Calculate scores
  scoresR <- ScoresR %*% vectors * sqrt(N-1) # see defintion of Z above!
  scoresR <- as.matrix(scoresR %*% diag(sqrt(values) * normFactors, nrow = M2, ncol = M2)) # normalizatio

  #GDI Index (Right Leg)
  ctrl = t(matrix(rep(colMeans(scoresR[ind_ctrl,]),N),M2,N))
  D = (scoresR-ctrl)^2
  GDI_MATR = log(sqrt(rowSums(apply(D,2,function(x){x}))))
  zGDI_MATR =(GDI_MATR - mean(GDI_MATR[ind_ctrl]))/sd(GDI_MATR[ind_ctrl])

  return(list(SFGDIB=SGDI_MAT,zFGDIU=Uni_FGDI,zFGDI=zGDI_MAT,Fits=Fits,
              RMSE=RMSE,SFGDIL=zGDI_MATL,SFGDIR=zGDI_MATR,PVE=PVE,UPVE=UPVE,NPC=NPC))
  
}  
