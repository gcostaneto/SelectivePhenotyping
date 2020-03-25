effective.N <- function(Kernel,fraction=.98,plot=FALSE,svd.print=FALSE){
  svd.KERNEL <- svd(Kernel, nu = nrow(Kernel), nv = ncol(Kernel))
  SVdD<-cumsum((svd.KERNEL$d[1:ncol(Kernel)])^2/sum(svd.KERNEL$d^2))
  if(isTRUE(plot)) plot(SVdD, col = "red")
  N <- length(SVdD[which(SVdD < fraction)])
  cat(paste0('--------------------------------------','\n'))
  cat(paste0('Explained variance ',100*fraction,'% \n'))
  cat(paste0('Effective Ne:',N,'\n'))
  cat(paste0('--------------------------------------','\n'))
  if(isFALSE(svd.print)) return(list(Ne=N))
  if(isTRUE(svd.print)) return(list(Ne=N,svd.d = svd.KERNEL$d,svd.u=svd.KERNEL$u,svd.v=svd.KERNEL$v))
}
