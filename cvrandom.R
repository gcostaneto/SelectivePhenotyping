CV.random <- function(gids,ngids,f.train=.7, df, rep=20, seed=NULL){
  CV1<-Sampling.CV1(gids = gids,ngids = ngids,f = f.train,seed = seed,rep = rep)
  CV2<-Sampling.CV2(f = f.train,Y = df,seed = seed,rep = rep)
  return(list(CV1=CV1,CV2=CV2))
}

Sampling.CV1 <- function(gids,ngids=NULL,f=NULL,seed=NULL,rep=NULL){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  if(is.null(ngids)) ngids <- length(unique(gids))
  n <- f*ngids
  set1 <- matrix(NA,nrow=n,ncol=rep)
  set.seed(seed)
  for(s in 1:rep) set1[,s] <- sort(sample(1:ngids, size = n , replace = FALSE))
  return(set1)
}

Sampling.CV2 <- function(f,Y,seed=NULL,rep=NULL){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  n <- nrow(Y)
  set1 <- matrix(NA,nrow=n*f,ncol=rep)
  set.seed(seed)
  for(s in 1:rep) set1[,s] <- sort(sample(1:n, size = n*f , replace = FALSE));set.seed(seed+s)
  return(set1)
}
