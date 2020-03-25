#' effective.N(): Effective population size realized by variance-covariance Kernels (see Misztal, 2016 - Genetics).
#' Kernel: a variance-covariance relationship matrix (for genotyeps or environments)
#' fraction: expected fraction of the Kernel matrix variance
#' plot=TRUE; returns a plot
#' svd.print; returns the outputs of the SVD

effective.N <- function(Kernel,fraction=.98,plot=TRUE,svd.print=FALSE){
  svd.KERNEL <- svd(Kernel, nu = nrow(Kernel), nv = ncol(Kernel))
  SVdD<-cumsum((svd.KERNEL$d[1:ncol(Kernel)])^2/sum(svd.KERNEL$d^2))
  N <- length(SVdD[which(SVdD < fraction)])
  if(isTRUE(plot)){
    plot(SVdD,lwd=2,pch=15,xlab='Individuals',bty='l', cex.lab=1.5, cex.axis=1.5, cex.main=2.2,
         main='Effective N size',ylab='Fraction of Kernel Variance')
    lines(SVdD,lwd=2)
    abline(v = N,lwd=3,col='red')
  }
    
  cat(paste0('--------------------------------------','\n'))
  cat(paste0('Explained variance ',100*fraction,'% \n'))
  cat(paste0('Effective Ne:',N,'\n'))
  cat(paste0('--------------------------------------','\n'))
  if(isFALSE(svd.print)) return(list(Ne=N))
  if(isTRUE(svd.print)) return(list(Ne=N,svd.d = svd.KERNEL$d,svd.u=svd.KERNEL$u,svd.v=svd.KERNEL$v))
}
