prcompRecon <- function(pca, pcs=NULL){
  if(is.null(pcs)) pcs <- seq(pca$sdev)
  recon <- as.matrix(pca$x[,pcs]) %*% t(as.matrix(pca$rotation[,pcs]))
  if(pca$scale[1] != FALSE){
    recon <- scale(recon , center=FALSE, scale=1/pca$scale)
    attr(recon, "scaled:scale") <- NULL
  }
  if(pca$center[1] != FALSE){
    recon <- scale(recon , center=-pca$center, scale=FALSE)
    attr(recon, "scaled:center") <- NULL
  }
  recon
}