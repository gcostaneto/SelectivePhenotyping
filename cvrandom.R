
Sampling.CV1 <- function(gids,ngids=NULL,nenv=5,f=NULL,seed=NULL,rep=NULL,gidlevel=TRUE){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  if(is.null(ngids)) ngids <- length(unique(gids))
  n <- f*ngids
  set1 <- matrix(NA,nrow=n,ncol=rep)
  set2 <- list()
  set.seed(seed)
  for(s in 1:rep) set1[,s] <- sort(sample(1:ngids, size = n , replace = FALSE));set.seed(seed+s)
  if(isFALSE(gidlevel)){
    for(s in 1:rep) set2[[s]] <-which(gids %in% unique(gids)[set1[,s]])
    return(set2)
  }else{
    return(set1)
  }

}

Sampling.CV2 <-function(f,Y,seed=NULL,rep=NULL){
  if(is.null(seed)) seed <-1010231
  if(is.null(rep)) rep<-10
  if(is.null(f)) f <-.7
  n <- nrow(Y)
  set1 <- list()
  set.seed(seed)
  for(s in 1:rep) set1[[s]] <- sort(sample(1:n, size = n*f , replace = FALSE));set.seed(seed+s)
  return(set1)
}

Sampling.CV0 <- function (gids, envs, ngids, f, out.env, seed, rep) 
{
  envs <- as.factor(envs)
  gids <- as.factor(gids)
  (env <- levels(envs))
  (gid <- levels(gids))

  out <- list()
  for (s in 1:rep) {
     set.seed(seed)
    (trai.env <- env[sample(1:length(env), size = length(env) - 
                              out.env, replace = F)])
    cida<-cida+1
    cat('training environments \n')
    cat(paste0(trai.env,'\n'))
    (trai.gid <- gid[Sampling.CV1(gids = gids, ngids = ngids, 
                                  f = f, seed = seed, rep = 1)])

    (newG <- which(!gids %in% trai.gid))
    (newE <- which(!envs %in% trai.env))
    newGE <- which(!envs %in% trai.env & !gids %in% trai.gid)
    training <- which(envs %in% trai.env & gids %in% trai.gid)
    out[[s]] <- list(newG = newG, newE = newE, newGE = newGE, 
                     training = training)
    seed <- seed+s
  }
  names(out) <- paste0("Rep", 1:rep)
  return(out)
}
