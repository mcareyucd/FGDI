overall_abnormality_fun = function (Subj, Ref, stopping_rule = "Kaiser-Guttman", dist_measure = "MAD", 
          TVE = 1, k = 2) 
{
  n_vars <- length(Subj)
  n <- nrow(Ref)
  refpop_means <- colMeans(Ref)
  refpop_sd <- apply(Ref, 2, stats::sd)
  refpop_sc <- scale(Ref, scale = T, center = T)
  refpop_cov <- stats::cov(refpop_sc)
  p <- stats::prcomp(Ref, center = T, scale = T, retx = T)
  EigenValues <- p$sdev^2
  if (stopping_rule == "Kaiser-Guttman") {
    numPCs <- sum(EigenValues >= 1)
  }
  if (stopping_rule == "brStick") {
    numPCs <- brStick(EigenValues)
  }
  if (stopping_rule == "TVE") {
    EigenSum <- sum(p$sdev^2)
    PC_VAF <- t(matrix(abs(EigenValues/EigenSum)))
    cum_PC_VAF <- round(cumsum(PC_VAF), 5)
    numPCs <- min(which(cum_PC_VAF >= TVE))
  }
  refpop_projs <- p$x
  PCmeans <- colMeans(refpop_projs)
  PCsds <- p$sdev
  Subj_sc <- (Subj - refpop_means)/refpop_sd
  Subj_projs <- Subj_sc %*% p$rotation[, 1:numPCs]
  dist <- (Subj_projs - PCmeans[1:numPCs])/PCsds[1:numPCs]
  if (dist_measure == "Euclidean") {
    k = 2
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }
  if (dist_measure == "MAD") {
    dist_abs <- abs(dist)
    abnormality <- sum(dist_abs)/numPCs
  }
  if (dist_measure == "Manhattan") {
    k = 1
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }
  if (dist_measure == "RMSE") {
    n <- length(dist)
    sum_dist_squared <- sum(dist^2)
    abnormality <- sqrt(sum_dist_squared/n)
  }
  if (dist_measure == "Lk-Norm") {
    dist_squared <- (abs(dist))^k
    abnormality <- sum(dist_squared)^(1/k)
  }
  return(list(abnormality,p,numPCs))
}